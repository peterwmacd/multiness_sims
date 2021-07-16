# BATCHTOOLS
# 1: make registry

# required package names
pkgs <- c("batchtools",
          "CISE")
# github package names
gh_pkg <- "multiness"

# install required packages
# install.packages(pkgs)

# install from github
# install.packages('devtools')
# library(devtools)
devtools::install_github(paste0('peterwmacd/',gh_pkg),force=T)

# load required packages
for(p in c(pkgs,gh_pkg)){library(p,character.only=T,verbose=T)}

# set working directory to current file location
home <- '~/multiness_sims/'
setwd(home)

# get required files to source
to_source <- paste0("code_to_source/",list.files(path='code_to_source/',pattern="?.R"))

# set up registry
# working directory will be current working directory
reg <- makeExperimentRegistry("experiment",
                              packages=c(pkgs,gh_pkg),
                              source=to_source)
