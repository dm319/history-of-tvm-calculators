#!/bin/gawk -f
BEGIN{
  RS = ""
}

{
  split(FILENAME,a,".")
  f="./intermediate/TVM" NR ".wiki" 
  # print f
  print > f
  close(f)
}

END{
}
