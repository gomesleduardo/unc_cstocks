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
library(purrr)
library(mvtnorm)
library(magrittr)

# READ DATA -------------------------------------------------------------------
load(
  file = file.path(
    'cstocks/1__tc_microreg/ds', 'carbon_stock__sheet.RData'
  )
)

# ---- Remove extra data
rm(list = 
     ls()[
       !(ls() %in% c(
         'unc_socref'
       ))
     ]
)

# SETTINGS --------------------------------------------------------------------
list_name <- 'samples__socref'

# FUNCTIONS -------------------------------------------------------------------

# ---- Functions
u_se <- function(x, u, level = .95) {
  
  em <- u / 100 * x
  se <- em / round(qnorm(1 - ((1 - level) / 2)), 2)
  return(se)
  
}

# SAMPLING --------------------------------------------------------------------

# ---- Sample size
n_samp <- 10

# ---- Labels 
lbls <- 'socref'

# ---- Mean 
mean <- 
  unc_socref %>%
  dplyr::select(SOCref) %>% 
  magrittr::extract2(1)

# ---- Standard deviation
sd <-
  unc_socref %>%
  dplyr::select(4) %>% 
  magrittr::extract2(1)

# ---- Remove NAs
sd[is.na(sd)] <- 0

# ---- Uncertainty
unc <- 1.96 * sd / mean * 100

# ---- Samples
set.seed(4) # 04: 4
s_df <- mvtnorm::rmvnorm(
  n = n_samp,
  mean = mean,
  sigma = diag(sd ^ 2)
)

# ---- List of results
assign(
  list_name, 
  list(
    mean = mean,
    unc = unc,
    sd = sd,
    samples = s_df
  )
)

# EXPORT ----------------------------------------------------------------------

# ---- Save data
save(
  list = list_name,
  file = file.path(
    'cstocks/1__tc_microreg/samples/factors',
    paste0(list_name, '.Rda')
  )
)

# ---- Delete environment
rm(list = ls())
