# library(tidyverse)
library(tictoc)

library(tmfast)

## ~70 sec for each round of the simulation w/ default values
tic()
journal_specific(k = 15, Mj = 20)
toc()
