# ------------------------------- #
# |  EMBRAPA                    | #
# |  MEIO AMBIENTE              | #
# ------------------------------- #
# ------------------------------- #
# |  Brazilian Land Use Change  | #
# |  BRLUC                      | #
# ------------------------------- #

# ------------------------------- #
# |    Uncertainty for BRLUC    | #
# ------------------------------- #

# LIBRARIES -------------------------------------------------------------------
library(dplyr)
library(stringr)

# FILES NAMES -----------------------------------------------------------------
list_res <- list.files('extra/applications/1__tc_microreg/results') %>% 
  magrittr::extract(stringr::str_detect(., 'MC__tc__'))

# LOAD RESULTS ----------------------------------------------------------------
for (k in list_res) {
  
  load(file.path('extra/applications/1__tc_microreg/results', k))
  
}

rm(k, list_res)

# BIND RESULTS ----------------------------------------------------------------
for (k in ls()) {
  k %>% 
    get() %>% 
    readr::write_csv2(
      file = paste0(
        'extra/applications/1__tc_microreg/results/tables/',
        k, '.csv'
      )
    )
}

rm(list = ls())
gc()
