#read in data
library(tidyverse)
library(stringr)

data.dir <- c('../Dropbox/___MA/social_RL_git/modelfit_fixed_beta//')
files <- dir(path = data.dir, pattern = "*.txt", full.names = T)
param_data <- NULL
for (i in files) {
  helper <- read.table(i, sep = " ")
  param_data <- rbind(param_data, t(helper))
}
