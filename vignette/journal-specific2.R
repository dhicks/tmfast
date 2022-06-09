library(tidyverse)
library(tictoc)

source('R/generators.R')
source('R/hellinger.R')
source('R/tmfast.R')

## ~70 sec for each round of the simulation
tic()
journal_specific()
toc()