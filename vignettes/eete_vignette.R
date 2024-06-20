## -----------------------------------------------------------------------------
library(tidyverse)
library(haven)
library(eete)

## -----------------------------------------------------------------------------
SIPP1991 = read_dta('../data/Angrist 1991 SIPP.dta')
SIPP1991_edit <- SIPP1991 |>
  mutate(rsncode = ifelse(rsncode == 999, NA, rsncode),
         wage = kwage,
         lwage = log(kwage))

