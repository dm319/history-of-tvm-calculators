#!/bin/bash
rm -f ./intermediate/TVM*
gawk -f split_vimwiki.awk ./data/TVM.wiki
R -f ./script.R
cat intro1.md ./output/results_table.md intro2.md > README.md
