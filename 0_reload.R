# BATCHTOOLS
# 0: reload registry

foldername <- "multiness_sims"
# reload registry
library(batchtools)
setwd(paste0('~/',foldername))
loadRegistry('experiment',
             writeable=T)

