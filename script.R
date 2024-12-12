library(tidyverse)
library(data.table)
library(Rmpfr)
library(knitr)
library(randomForest)
library(Rtsne)
library(viridis)
library(ggrepel)
library(Rphenograph)
library(gplots)

prec <- 2000
h <- 3
options(digits = 6)

files <- list.files(path = "intermediate", full.names = TRUE)

# read in data and merge files
results <-
files |> 
  map(~fread(., sep = "|", strip.white = TRUE,
             header = TRUE,
            colClasses = 'character'
            )) %>%
  map(~.[-1, 2:(dim(.)[2]-1)]) %>%
  Reduce(function(...) merge(..., by = "calculator"), .)

# ignore default answer for double root puzzle
results <- results[, !c("5", "6")]

# read in meta data
meta_data <-
fread("./data/TVM_meta.wiki",
      sep = "|",
      strip.white = TRUE,
      header = TRUE) %>%
  .[-1, 2:(dim(.)[2]-1)] %>%
  .[, year := as.numeric(year)] %>%
  .[!is.na(year)]
  
# long format
results_long <-
melt(results,
     id.vars = "calculator",
     variable.name = "puzzle")

# transposed data
results_cal <-
dcast(results_long, puzzle ~ calculator)

# back to long but with reference
results_ref_long <-
melt(results_cal,
     id.vars = c("puzzle", "Ref"),
     variable.name = "calculator")

# special cases
two_root1 <- c("5A", "5B")
two_root2 <- c("6A", "6B")
rel_acc <- c("1", "2", "3", "9", "10", "12")

# calculate accuracy
results_ref_long[, fail := ifelse(value == "ERR", TRUE, FALSE)] %>%
  .[, value := ifelse(fail, NA, value)] %>%
  .[, value := ifelse(value == "", NA, value)] %>% 
  .[, Ref := ifelse(Ref == "", NA, Ref)] %>% 
  .[, value := mpfr(value, prec)] %>% 
  .[, Ref := mpfr(Ref, prec)] %>% 
  .[, abs_accuracy := -log10(abs(Ref - value))] %>%
  .[, rel_accuracy := -log10((abs(Ref - value)) / abs(Ref))]

# merge relative and absolute accuracy
results_ref_long[puzzle %in% rel_acc, accuracy := rel_accuracy]
results_ref_long[!(puzzle %in% rel_acc), accuracy := abs_accuracy]

# pull measures of accuracy
results_abs <- results_ref_long[, c("puzzle", "calculator", "fail", "abs_accuracy")]
results_rel <- results_ref_long[, c("puzzle", "calculator", "fail", "rel_accuracy")]
results_com <- results_ref_long[, c("puzzle", "calculator", "fail", "accuracy")]

results_abs[, abs_accuracy := as.numeric(abs_accuracy)]
results_rel[, rel_accuracy := as.numeric(rel_accuracy)]
results_com[, accuracy := as.numeric(accuracy)]

# did it fail 5 or 6?
results_com[puzzle %in% two_root1, dual_fail1 := sum(fail), by = calculator] %>%
  .[dual_fail1 < 2, fail := FALSE] %>%
  .[puzzle %in% two_root2, dual_fail2 := sum(fail), by = calculator] %>%
  .[dual_fail2 < 2, fail := FALSE] %>%
  .[is.nan(accuracy), accuracy := NA] 

result_com_pre <- copy(results_com)

# apply penalty for failing
results_com[, accuracy := ifelse(fail,
                                     min(accuracy[is.finite(accuracy)], na.rm = TRUE) - h,
                                     accuracy), by = "puzzle"]

# apply bonus for exact answer
results_com[, accuracy := ifelse(accuracy == Inf,
                                     max(accuracy[is.finite(accuracy)]) + h,
                                     accuracy), by = "puzzle"]

# back to wide for imputation
res_com <-
dcast(results_com[, -c("fail", "dual_fail1", "dual_fail2")], calculator ~ puzzle)

# impute using random forest to reduce bias due to missing values
res_com_imputed <-
  rfImpute(calculator ~ ., res_com)

res_com_matrix <-
res_com_imputed %>%
  select(matches("^\\d")) %>%
  as.matrix

# produce table of results
options(digits = 4)

# calculate median and mean score
sum_acc <-
res_com_imputed %>%
  pivot_longer(matches("^\\d"), names_to = "puzzle", values_to = "accuracy") %>%
  group_by(calculator) %>%
  summarise(median = median(accuracy, na.rm = TRUE),
            mean = mean(accuracy, na.rm = TRUE))

# output table of results
result_com_pre %>%
  mutate(accuracy = as.character(round(accuracy,1))) %>%
  mutate(accuracy = ifelse(fail, "ERR", accuracy)) %>%
  select(puzzle, calculator, accuracy) %>%
  pivot_wider(names_from = puzzle, values_from = accuracy) %>%
  left_join(sum_acc) %>%
  arrange(desc(median), desc(mean)) %>%
  kable

# ranking for levels
ranking <-
sum_acc %>% mutate(calculator = as.character(calculator)) %>% arrange(median, mean) %>% pull(calculator)

# plot accuracy results
res_com_imputed %>%
  mutate(calculator = as.character(calculator)) %>%
  left_join(meta_data) %>%
  mutate(calculator = factor(calculator, levels = ranking)) %>%
  pivot_longer(matches("^\\d"), names_to = "puzzle", values_to = "accuracy") %>%
  ggplot(aes(x = calculator, y = accuracy, colour = year))+
  geom_jitter()+
  geom_boxplot(aes(fill = year), alpha = 0.2, outliers = FALSE)+
  scale_colour_gradient(low = "#6C0000", high = "#2A006C")+
  scale_fill_gradient(low = "#6C0000", high = "#2A006C")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("./figures/results.png", width = 12, height = 7.5, dpi = 120)

# prepare data for tSNE
dim1 <-
dim(res_com_matrix) 

# generate small amount of noise for tSNE
noise <-
matrix(data = 0.01 * runif(dim1[1]*dim1[2]),
       nrow = dim1[1],
       ncol = dim1[2])

# add noise to data
res_com_noisy <- res_com_matrix + noise

# calculate tSNE on calculators
res_com_tsne <-
Rtsne(res_com_noisy, perplexity = 12,
      max_iter = 50000, verbose = TRUE)

# tSNE on problems
tsne_problems <-
Rtsne(t(res_com_noisy), dim = 1, perplexity = 4,
      max_iter = 10000, verbose = TRUE)

# one dimensional tSNE result
prob_tsne_arr <-
data.frame(calculator = rownames(t(res_com_noisy)),
           position = tsne_problems$Y) 

# run clustering on problems
louv_problems <-
Rphenograph(t(res_com_matrix), 8)

prob_louv_arr <-
data.frame(calculator = rownames(t(res_com_matrix)),
           position = louv_problems[[2]]$membership) 

# PCA on calculators
res_com_pca <-
prcomp(res_com_matrix, scale = TRUE)

# louvain on calculators
res_com_louvain <-
Rphenograph(res_com_matrix, 8)

res_com_louvain[[2]]$membership

res_com_dr <-
cbind(res_com_imputed, pca = res_com_pca$x[,1:2],
      tsne = res_com_tsne$Y,
      louvain = as.factor(res_com_louvain[[2]]$membership))

res_com_dr_meta <-
res_com_dr %>%
  left_join(meta_data) 

# plot accuracy and clustering
res_com_dr %>%
  left_join(meta_data) %>%
  pivot_longer(matches("^\\d"), names_to = "puzzle", values_to = "accuracy") %>%
  mutate(calculator = fct_reorder(calculator, accuracy, .fun = median)) %>%
  ggplot(aes(x = calculator, y = accuracy, colour = louvain))+
  geom_jitter()+
  geom_boxplot(aes(), alpha = 0.2, outliers = FALSE)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("./figures/results_louvain.png", width = 12, height = 7.5, dpi = 120) 

res_com_dr_meta %>%
  ggplot(aes(x = tsne.1, y = tsne.2, colour = year,
             label = calculator))+
  geom_label_repel(data = res_com_dr_meta, aes(label = calculator),
                   nudge_x = 0.5, nudge_y = 0.15,
                   force = 0.01,
                   alpha = 0.5)+
  labs(x = "TSNE1", y = "TSNE2")+
  scale_colour_gradient(low = "#6C0000", high = "#2A006C")+
  geom_point(size = 4, alpha = 0.6)

ggsave("tsne.png", width = 7.5, height = 7, dpi = 120)

res_com_dr_meta %>%
  ggplot(aes(x = tsne.1, y = tsne.2, colour = louvain,
             label = calculator))+
  geom_label_repel(data = res_com_dr_meta, aes(label = calculator),
                   nudge_x = 0.5, nudge_y = 0.15,
                   force = 0.1,
                   alpha = 0.8)+
  labs(x = "TSNE1", y = "TSNE2")+
  geom_point(size = 4, alpha = 0.6)

ggsave("tsne_louvain.png", width = 8.5, height = 8, dpi = 120)

res_com_dr_meta %>%
  ggplot(aes(x = pca.PC1, y = pca.PC2, colour = year,
             label = calculator))+
  geom_label_repel(data = res_com_dr_meta, aes(label = calculator),
                   nudge_x = -1.5, nudge_y = 0.15,
                   force = 1.01,
                   alpha = 0.5)+
  labs(x = "PC1", y = "PC2", title = "Dimension reduction of financial calculator performance")+
  scale_colour_gradient(low = "#6C0000", high = "#2A006C")+
  geom_point(size = 4, alpha = 0.6)

ggsave("pca.png", width = 7.5, height = 7, dpi = 120)

res_com_dr_meta %>%
  ggplot(aes(x = pca.PC1, y = pca.PC2, colour = louvain,
             label = calculator))+
  geom_label_repel(data = res_com_dr_meta, aes(label = calculator),
                   nudge_x = 0.5, nudge_y = 0.15,
                   force = 0.01,
                   alpha = 0.8)+
  labs(x = "PC1", y = "PC2")+
  geom_point(size = 4, alpha = 0.6)

ggsave("tsne_pca.png", width = 7.5, height = 7, dpi = 120)

res_com %>%
  left_join(meta_data) %>%
  pivot_longer(matches("^\\d"), names_to = "puzzle", values_to = "accuracy") %>%
  mutate(digits = as.numeric(digits)) %>%
  mutate(calculator = fct_reorder(calculator, accuracy, .fun = mean)) %>%
  ggplot(aes(x = digits, y = accuracy))+
  geom_point()+
  geom_smooth()

res_com %>%
  left_join(meta_data) %>%
  pivot_longer(matches("^\\d"), names_to = "puzzle", values_to = "accuracy") %>%
  mutate(digits = as.numeric(digits)) %>%
  mutate(digit_efficiency = accuracy / digits) %>%
  filter(!is.na(digit_efficiency)) %>%
  mutate(calculator = fct_reorder(calculator, digit_efficiency, .fun = median)) %>%
  ggplot(aes(x = calculator, y = digit_efficiency, colour = year))+
  geom_jitter()+
  geom_boxplot(aes(fill = year), alpha = 0.2, outliers = FALSE)+
  scale_colour_gradient(low = "#6C0000", high = "#2A006C")+
  scale_fill_gradient(low = "#6C0000", high = "#2A006C")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = unit(c(0, 0, 0, 0.4), "inches"))+
  labs(x = "calculator", y = "TVM digit efficiency")

ggsave("digit_efficiency.png", width = 12, height = 7.5, dpi = 120)


rownames(res_com_dr_meta) <- res_com_dr_meta$calculator

col_breaks <- c(seq(-5,-0.01,length = 100),
                0,
                seq(0.01,14, length = 100),
                seq(14.1,40, length = 100))

my_palette <- colorRampPalette(c("#3540FF","black","#9C2020","#FF3535"))(n = length(col_breaks)-1)
my_palette <- colorRampPalette(c("#242BAB","black","#990F4E","#BD1360"))(n = length(col_breaks)-1)

col_order <- order(prob_louv_arr$position)

m <-
res_com_dr_meta %>%
  select(matches("^\\d")) %>%
  as.matrix(.) 

m[,col_order] %>%
  heatmap.2(.,
  breaks = col_breaks,
  col = my_palette,
  trace = "none",
  symm = F,
  symkey = F,
  symbreaks = FALSE,
  margins = c(5,14),
  dendrogram = "both",
  cexCol = 1.5,
  cexRow = 1.0,
  keysize = 1,
  key.title = "Key",
  key.xlab = "Accuracy"
  )

png(filename = "heatmap.png", width = 9, height = 9, units = "in", res = 120)
m[,col_order] %>%
  heatmap.2(.,
  breaks = col_breaks,
  col = my_palette,
  trace = "none",
  symm = F,
  symkey = F,
  symbreaks = FALSE,
  margins = c(5,14),
  dendrogram = "both",
  cexCol = 1.5,
  cexRow = 1.0,
  keysize = 1,
  key.title = "Key",
  key.xlab = "Accuracy"
  )
dev.off()
