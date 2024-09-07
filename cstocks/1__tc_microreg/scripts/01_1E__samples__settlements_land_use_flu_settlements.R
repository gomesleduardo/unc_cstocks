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
library(janitor)
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
         'climate__micro',
         'unc_factors__settlements'
       ))
     ]
)

# SETTINGS --------------------------------------------------------------------
type_name <- 'Settlements'
factor_name <- 'Land use (FLU)'
management_name <- 'Settlements'
list_name <- paste0(
  'samples__',
  janitor::make_clean_names(
    paste(
      type_name,
      factor_name,
      management_name
    )
  )
)

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
lbls <- "independent"

# ---- Mean 
mean <- unc_factors__settlements %>%
  dplyr::select(average) %>% 
  t() %>% 
  as.numeric() %>% 
  setNames(lbls)

# ---- Uncertainty
unc <- unc_factors__settlements %>%
  dplyr::select(uncertainty) %>% 
  t() %>% 
  as.numeric() %>% 
  magrittr::multiply_by(100) %>% 
  setNames(lbls)

# ---- Standard deviation
sd <- unc %>% 
  u_se(mean, .)

# ---- Remove NAs
sd[is.na(sd)] <- 0

# ---- Samples
set.seed(115) # 01_1E: 1 15
s_df <- mvtnorm::rmvnorm(
  n = n_samp,
  mean = mean,
  sigma = diag(sd ^ 2, nrow = 1)
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

# EXPORT --------------------------------------------------------------------

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
