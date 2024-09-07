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

# SETTINGS --------------------------------------------------------------------
spreadsheet_name <- "Carbon stock per microregion_BRLUC_2.07_v69.xlsx"

# ---------------------------------- #
## Limites da planilha 'CCalcMicro' ##
# ---------------------------------- #
range_carbon_stock <- "A3:LK561" # <- Verificar a cada nova versao!

# ----------------------------------------------------- #
## Limites da planilha 'AvgMicro': Tabela 'harvesting' ##
# ----------------------------------------------------- #
range_harvesting <- "BB2:BF560" # <- Verificar a cada nova versao!

# ------------------------------------------------------ #
## Limites da planilha 'AvgMicro': Tabela 'degradation' ##
# ------------------------------------------------------ #
range_degradation <- "BU2:CA560" # <- Verificar a cada nova versao!

# ---------------------------------------------------------------- #
## Limites da planilha 'AvgMicro': Tabela 'area occuped cropland' ##
# ---------------------------------------------------------------- #
range_area_ocupped <- "AP2:AY560" # <- Verificar a cada nova versao!

# LIBRARIES -------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(purrr)
library(readxl)
library(stringr)
library(janitor)

# DATA SETS -------------------------------------------------------------------

## Data set: Proportion of climate type occurrence per Micro region ----

# ---- Climate per States
climate__state <- 
  readxl::read_excel(
    path = paste0('data/', spreadsheet_name),
    sheet = "Climates Micro", range = "L3:T29",
    col_names = c(
      "IBGE_code", "name", "abbr", 
      "area__ha", "tropical__dry", "tropical__moist",      
      "tropical__montane", "tropical__wet", "warm_temperate__moist"
    )
  ) %>% 
  dplyr::filter(!is.na(IBGE_code))

# ---- Climate per Micro region
climate__micro <- 
  readxl::read_excel(
    path = paste0('data/', spreadsheet_name),
    sheet = "Climates Micro", range = "A3:J560",
    col_names = c(
      "IBGE_code", "name", "tropical__dry", "tropical__moist", 
      "tropical__montane", "tropical__wet", "warm_temperate__moist",
      "total", "area__ha", "abbr"
    )
  ) %>% 
  dplyr::select(IBGE_code, name, abbr, area__ha,
                tropical__dry, tropical__moist, tropical__montane, 
                tropical__wet, warm_temperate__moist) %>% 
  dplyr::filter(!is.na(IBGE_code))

## Data set: Summary of total carbon stock calculation ----
carbon_stock_temp <- readxl::read_excel(
  path = paste0('data/', spreadsheet_name), 
  sheet = "CCalcMicro", range = range_carbon_stock
)

## Data set: Relative stock change factors for cropland ----
unc_factors__cropland <- readxl::read_excel(
  path = paste0('data/', spreadsheet_name), 
  sheet = "Stock change factors", range = "A2:R15")

## Data set: Relative stock change factors for grassland ----
unc_factors__grassland <- readxl::read_excel(
  path = paste0('data/', spreadsheet_name), 
  sheet = "Stock change factors", range = "A19:R29")

## Data set: Percentage of time of sugarcane land occupancy ----
unc_factors__sugarcane <- readxl::read_excel(
  path = paste0('data/', spreadsheet_name), 
  sheet = "Stock change factors", range = "A33:C34")

## Data set: Percentage of time of settlements land occupancy ----
unc_factors__settlements <- readxl::read_excel(
  path = paste0('data/', spreadsheet_name), 
  sheet = "Stock change factors", range = "A44:C45"
)

names(unc_factors__settlements) <- c("factor", "average", "uncertainty")

unc_factors__settlements[1, 3] <- unc_factors__settlements[1, 3] %>% 
  gsub("[^0-9]", "", .) %>% 
  as.numeric() %>% 
  magrittr::divide_by(100) %>% 
  as.character()

unc_factors__settlements$uncertainty <- as.numeric(unc_factors__settlements$uncertainty)

## Data set: ParametersR (uncertainty for SOCref) ----
unc_socref <- readxl::read_excel(
  path = paste0('data/', spreadsheet_name), 
  sheet = "ParametersR", range = "A62:D620")

## Data set: ParametersR (uncertainty for CVeg) ----
unc_cveg <- readxl::read_excel(
  path = paste0('data/', spreadsheet_name), 
  sheet = "ParametersR", range = "F62:I70", 
  col_types = c("text", "skip", "numeric", 
                "skip")) %>% 
  dplyr::mutate(Cveg = janitor::make_clean_names(Cveg)) 

## Data set: ParametersR (uncertainty for CVeg) ----
unc_cveg_sugarcane <- readxl::read_excel(
  path = paste0('data/', spreadsheet_name), 
  sheet = "CvegSugarcane", range = "A2:K560", 
  col_types = c("numeric", "text", "numeric", 
                "text", "numeric", "numeric", "numeric", 
                "numeric", "numeric", "numeric", 
                "numeric")) %>% 
  janitor::clean_names()

## Data set: AvgMicro (harvesting) ----
harvesting_df <- readxl::read_excel(
  path = paste0('data/', spreadsheet_name), 
  sheet = "AvgMicro", range = range_harvesting) %>% 
  janitor::clean_names()

## Data set: AvgMicro (degradation) ----
degradation_df <- readxl::read_excel(
  path = paste0('data/', spreadsheet_name), 
  sheet = "AvgMicro", range = range_degradation) %>% 
  janitor::clean_names() %>% 
  dplyr::mutate_all(~ tidyr::replace_na(., 0))

## Data set: AvgMicro (area ocupped) ----
area_ocupped_df <- readxl::read_excel(
  path = paste0('data/', spreadsheet_name), 
  sheet = "AvgMicro", range = range_area_ocupped) %>% 
  janitor::clean_names() %>% 
  dplyr::mutate_if(is.numeric, ~ tidyr::replace_na(., 0))

## Split/edit data sets ----

# ---- Carbon stock table
carbon_stock_tbl <- data.frame(
  lu_name = readxl::read_excel(
    path = paste0('data/', spreadsheet_name), 
    sheet = "CCalcMicro", range = range_carbon_stock %>% 
      stringr::str_replace(pattern = "A3:", replacement = "A1:") %>% 
      stringr::str_replace(pattern = "561", replacement = "2")
  ) %>% 
    dplyr::slice(1) %>% 
    unlist() %>% 
    as.character() %>%
    na.exclude() %>% 
    magrittr::extract(- 1) %>% 
    janitor::make_clean_names(),
  start = seq(8, ncol(carbon_stock_temp), 7) - 6,
  end = seq(8, ncol(carbon_stock_temp), 7)
)

# ---- Carbon stock for different land uses
for (i in 1:nrow(carbon_stock_tbl)) {
  assign(
    x = paste0("tc__", carbon_stock_tbl$lu_name[i]), 
    value = carbon_stock_temp %>% 
      dplyr::select(1, (carbon_stock_tbl$start[i]):(carbon_stock_tbl$end[i])) %>% 
      purrr::set_names(
        c("IBGE_code", "SOCref", "FLU", "FMG", "FI", "SOC", "Cveg", "Total")
      )
  )
}

# ---- Uncertainty for stock change factors: cropland 
unc_factors__cropland <-
  unc_factors__cropland %>% 
  purrr::set_names(
    c("factor", 
      "management_option", 
      "warm_temperate__dry__avg", 
      "warm_temperate__moist__avg",            
      "warm_temperate__wet__avg", 
      "tropical__dry__avg",                    
      "tropical__moist__avg", 
      "tropical__wet__avg",                   
      "tropical__montane__avg", 
      "indep_climate_BR__avg", 
      "warm_temperate__dry__unc", 
      "warm_temperate__moist__unc",           
      "warm_temperate__wet__unc", 
      "tropical__dry__unc",                   
      "tropical__moist__unc",
      "tropical__wet__unc",                   
      "tropical__montane__unc", 
      "indep_climate_BR__unc"
    )
  ) %>% 
  dplyr::filter(!is.na(factor)) %>% 
  mutate_at(
    c("indep_climate_BR__avg", "warm_temperate__dry__unc", 
      "warm_temperate__moist__unc", "warm_temperate__wet__unc", 
      "tropical__dry__unc", "tropical__moist__unc", "tropical__wet__unc", 
      "tropical__montane__unc", "indep_climate_BR__unc"
    ), 
    ~ stringr::str_replace_all(., "No", "") %>% 
      stringr::str_replace_all("NA", "") %>% 
      stringr::str_extract("[0-9.]+") %>% 
      as.numeric()
  ) %>% 
  mutate_at(
    c("warm_temperate__dry__unc", 
      "warm_temperate__moist__unc", "warm_temperate__wet__unc", 
      "tropical__dry__unc", "tropical__moist__unc", "tropical__wet__unc", 
      "tropical__montane__unc", "indep_climate_BR__unc"
    ), ~ magrittr::divide_by(., 100))

# ---- Uncertainty for stock change factors: grassland 
unc_factors__grassland  <-
  unc_factors__grassland  %>% 
  purrr::set_names(
    c("factor", 
      "management_option", 
      "warm_temperate__dry__avg", 
      "warm_temperate__moist__avg",            
      "warm_temperate__wet__avg", 
      "tropical__dry__avg",                    
      "tropical__moist__avg", 
      "tropical__wet__avg",                   
      "tropical__montane__avg", 
      "indep_climate_BR__avg", 
      "warm_temperate__dry__unc", 
      "warm_temperate__moist__unc",           
      "warm_temperate__wet__unc", 
      "tropical__dry__unc",                   
      "tropical__moist__unc",
      "tropical__wet__unc",                   
      "tropical__montane__unc", 
      "indep_climate_BR__unc"
    )
  ) %>% 
  dplyr::filter(!is.na(factor)) %>% 
  mutate_at(
    c("indep_climate_BR__avg", "warm_temperate__dry__unc", 
      "warm_temperate__moist__unc", "warm_temperate__wet__unc", 
      "tropical__dry__unc", "tropical__moist__unc", "tropical__wet__unc", 
      "tropical__montane__unc", "indep_climate_BR__unc"
    ), 
    ~ stringr::str_replace_all(., "No", "") %>% 
      stringr::str_replace_all("NA", "") %>% 
      stringr::str_extract("[0-9.]+") %>% 
      as.numeric()
  ) %>% 
  mutate_at(
    c("warm_temperate__dry__unc", 
      "warm_temperate__moist__unc", "warm_temperate__wet__unc", 
      "tropical__dry__unc", "tropical__moist__unc", "tropical__wet__unc", 
      "tropical__montane__unc", "indep_climate_BR__unc"
    ), ~ magrittr::divide_by(., 100))

# ---- Remove temp data sets
rm(carbon_stock_temp)
rm(carbon_stock_tbl)
rm(spreadsheet_name)
rm(range_harvesting)
rm(range_degradation)
rm(range_carbon_stock)
rm(i)

## Validation ----

# # Sum of proportion of climate type
# climate__state %>%
#   dplyr::select(
#     tropical__dry, tropical__moist, tropical__montane,
#     tropical__wet, warm_temperate__moist
#   ) %>%
#   rowSums() # 1 for all
# 
# climate__micro %>%
#   dplyr::select(
#     tropical__dry, tropical__moist, tropical__montane,
#     tropical__wet, warm_temperate__moist
#   ) %>%
#   rowSums() # 1 for all
#   
# # Sum of area
# climate__state %>%
#   dplyr::select(
#     area__ha
#   ) %>%
#   sum() # 850274007
# 
# climate__micro %>%
#   dplyr::select(
#     area__ha
#   ) %>%
#   sum() # 850274007

# SAVE DATA -------------------------------------------------------------------
save.image(
  file = file.path(
    'cstocks/1__tc_microreg/ds', 'carbon_stock__sheet.RData'
  )
)

rm(list = ls())
gc()
