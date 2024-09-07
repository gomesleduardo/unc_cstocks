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
library(tibble)
library(readr)

# FILES NAMES -----------------------------------------------------------------
list_res <- list.files('extra/applications/1__tc_microreg/results') %>% 
  magrittr::extract(stringr::str_detect(., 'MC__tc__'))

# ---- Progress bar
pb <- utils::txtProgressBar(max = length(list_res), style = 3)

# LOAD RESULTS ----------------------------------------------------------------
for (k in 1:length(list_res)) {
  
  load(file.path('extra/applications/1__tc_microreg/results', list_res[k]))
  df <- ls()[!(ls() %in% c("k", "list_res", "pb"))] %>% get()
  rm(list = ls()[!(ls() %in% c("k", "list_res", "df", "pb"))])
  
  # UNC DECOMPOSITION -----------------------------------------------------------
  # https://en.wikipedia.org/wiki/Distribution_of_the_product_of_two_random_variables
  df <- df %>% 
    dplyr::mutate(
      # Squared CV decomposition [1]
      total_cv2 = (total_sd / total_est) ^ 2,
      total_est2 = total_est ^ 2,
      soc_var = soc_sd ^ 2,
      cveg_var = cveg_sd ^ 2,
      # Squared CV decomposition [2]
      socref_cv2 = (socref_sd / socref_est) ^ 2,
      flu_cv2 = (flu_sd / flu_est) ^ 2,
      fmg_cv2 = (fmg_sd / fmg_est) ^ 2,
      fi_cv2 = (fi_sd / fi_est) ^ 2
    ) %>% 
    # Squared CV decomposition [1]
    dplyr::mutate(
      dcv2tot = (soc_var / total_est2) + (cveg_var / total_est2),
      dcv2tot_soc = (soc_var / total_est2) / dcv2tot,
      dcv2tot_cveg = (cveg_var / total_est2) / dcv2tot
    ) %>% 
    # Squared CV decomposition [2]
    dplyr::mutate(
      dcv2soc = (log(1 + ifelse(is.na(socref_cv2), 0, socref_cv2)) + 
        log(1 + ifelse(is.na(flu_cv2), 0, flu_cv2)) + 
          log(1 + ifelse(is.na(fmg_cv2), 0, fmg_cv2)) + 
          log(1 + ifelse(is.na(fi_cv2), 0, fi_cv2))
        ),
      dcv2soc_socref = log(1 + socref_cv2) / dcv2soc,
      dcv2soc_flu = log(1 + flu_cv2) / dcv2soc,
      dcv2soc_fmg = log(1 + fmg_cv2) / dcv2soc,
      dcv2soc_fi  = log(1 + fi_cv2) / dcv2soc,
    ) %>% 
    dplyr::mutate(
      dcv2soc1 = (soc_var / total_est2),
      dcv2soc2 = log(1 + socref_cv2) + log(1 + flu_cv2) + log(1 + fmg_cv2) + log(1 + fi_cv2),
    ) %>% 
    # Squared CV decomposition - Combine [1] and [2]
    dplyr::mutate(
      dcv2tot_socref = dcv2tot_soc * dcv2soc_socref,
      dcv2tot_flu = dcv2tot_soc * dcv2soc_flu,
      dcv2tot_fmg = dcv2tot_soc * dcv2soc_fmg,
      dcv2tot_fi = dcv2tot_soc * dcv2soc_fi
    ) %>% 
    dplyr::select(
      IBGE_code, 
      dcv2tot_cveg, dcv2tot_socref, dcv2tot_flu, dcv2tot_fmg, dcv2tot_fi, 
      dcv2tot_soc, dcv2soc_socref, dcv2soc_flu, dcv2soc_fmg, dcv2soc_fi
    )
  
  # ---- New name
  assign(
    x = paste0(
      'unc_decomp__',
      list_res[k] %>% 
        stringr::str_remove_all("MC__") %>%
        stringr::str_remove_all(".Rda")
    ),
    value = df
  )
  
  # ---- Save data
  save(
    list = paste0(
      'unc_decomp__',
      list_res[k] %>% 
        stringr::str_remove_all("MC__") %>%
        stringr::str_remove_all(".Rda")
    ),
    file = file.path(
      'extra/applications/1__tc_microreg/results/tables/uncertainty_decomposition',
      paste0(
        paste0(
          'unc_decomp__',
          list_res[k] %>% 
            stringr::str_remove_all("MC__") %>%
            stringr::str_remove_all(".Rda")
        ), '.Rda'
      )
    )
  )
  
  # ---- Print progress bar
  utils::setTxtProgressBar(pb, k)
  
  # ---- Remove objects
  rm(list = ls()[!(ls() %in% c("k", "list_res", "pb"))])

}

# ---- Close connection
close(pb)

rm(list = ls())
gc()
