#!/bin/bash
rm -f ./intermediate/TVM*
gawk -f split_vimwiki.awk ./data/TVM.wiki
