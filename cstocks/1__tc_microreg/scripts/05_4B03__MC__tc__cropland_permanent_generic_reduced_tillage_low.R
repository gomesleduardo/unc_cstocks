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
library(magrittr)
library(tibble)
library(LaplacesDemon)
library(e1071)

# SETTINGS --------------------------------------------------------------------
type_name <- 'tc__cropland_permanent_generic_reduced_tillage_low'
socref_name <- 'samples__socref'
flu_name <- 'samples__cropland_land_use_flu_perennial_cropland'
fmg_name <- 'samples__cropland_tillage_fmg_reduced'
fi_name <- 'samples__cropland_input_f1_low'
cveg_name <- 'permanent_croplands'

# READ DATA -------------------------------------------------------------------
load(
  file = file.path(
    'extra/applications/1__tc_microreg/ds', 'carbon_stock__sheet.RData'
  )
)

# ---- Remove extra data
rm(list = ls()[!(ls() %in% c(
  type_name, 'type_name', 'socref_name', 'flu_name', 
  'fmg_name', 'fi_name', 'cveg_name',
  'climate__micro', 'unc_cveg'
))])

# ---- Reduce and order climate
climate__micro_temp <- 
  climate__micro %>% 
  dplyr::select(IBGE_code,
                tropical__dry,
                tropical__moist,
                tropical__montane,
                tropical__wet,
                warm_temperate__moist)
rm(climate__micro)

# ---- Load weighted factors samples
for (x in c(socref_name, flu_name, fmg_name, fi_name)) {
  load(
    file.path(
      "extra/applications/1__tc_microreg/samples/factors",
      paste0(x, ".Rda")
    )
  )
}
rm(x)

# FUNCTIONS -------------------------------------------------------------------

# ---- Functions
u_se <- function(x, u, level = .95) {
  
  em <- u / 100 * x
  se <- em / round(qnorm(1 - ((1 - level) / 2)), 2)
  return(se)
  
}

# MONTE CARLO -----------------------------------------------------------------

# ---- List with results
Ctotal_samples <- list()

# ---- Message
message("Monte Carlo computation")

# ---- Progress bar
pb <- utils::txtProgressBar(max = nrow(climate__micro_temp), style = 3)

for (i in 1:nrow(climate__micro_temp)) {
  
  # ---- SOCref distribution
  dist_socref <- samples__socref$samples[, i]
  
  # ---- Weighted FLU distribution
  dist_flu <- 
    flu_name %>% 
    get() %>% 
    magrittr::use_series("samples") %>% 
    apply(1, function(x, w){x * w}, 
          w = climate__micro_temp[i, - 1] %>%
            as.numeric()
    ) %>% 
    t() %>% 
    rowSums()
  
  # ---- Weighted FMG distribution
  dist_fmg <- 
    fmg_name %>% 
    get() %>% 
    magrittr::use_series("samples") %>% 
    apply(1, function(x, w){x * w}, 
          w = climate__micro_temp[i, - 1] %>%
            as.numeric()
    ) %>% 
    t() %>% 
    rowSums()
  
  # ---- Weighted FI distribution
  dist_fi <- 
    fi_name %>% 
    get() %>% 
    magrittr::use_series("samples") %>% 
    apply(1, function(x, w){x * w}, 
          w = climate__micro_temp[i, - 1] %>%
            as.numeric()
    ) %>% 
    t() %>% 
    rowSums()
  
  # ---- CVeg distribution
  
  # Sample size
  n_samp <- 1000 
  
  # Mean
  mean <- type_name %>% 
    get() %>% 
    magrittr::use_series("Cveg") %>% 
    magrittr::extract(i)
  
  # Default uncertainty
  unc <- unc_cveg %>% 
    dplyr::filter(Cveg == cveg_name) %>% 
    magrittr::use_series("Uncertainty") %>% 
    magrittr::multiply_by(100)
  
  # Standard deviation
  sd <- unc %>% u_se(mean, .)
  
  # Samples
  if (mean > 0) {
    set.seed(19930107)
    dist_cveg <- rnorm(
      n = n_samp,
      mean = mean,
      sd = sd
    )
  } else {
    dist_cveg <- mean
  }
  
  # ---- SOC distribution
  dist_soc <- (
    # SOCref
    dist_socref *
      # Weighted factors
      dist_flu * dist_fmg * dist_fi
  )
  
  # ---- Total SOC distribution
  if ((type_name %>% 
       get() %>% 
       magrittr::use_series("Total") %>% 
       magrittr::extract(i)) > 0
  ) {
    
    dist_total <- (
      # SOCref   # CVeg
      dist_soc + dist_cveg
    )
    
  } else {
    dist_total <- 0
  }
  
  # ---- Save results
  Ctotal_samples <- c(
    Ctotal_samples,
    setNames(
      list(
        tibble(
          SOCref = 
            (type_name %>% 
               get() %>% 
               magrittr::use_series("SOCref") %>% 
               magrittr::extract(i)),
          FLU = 
            (type_name %>% 
               get() %>% 
               magrittr::use_series("FLU") %>% 
               magrittr::extract(i)),
          FMG = 
            (type_name %>% 
               get() %>% 
               magrittr::use_series("FMG") %>% 
               magrittr::extract(i)),
          FI = 
            (type_name %>% 
               get() %>% 
               magrittr::use_series("FI") %>% 
               magrittr::extract(i)),
          SOC = 
            (type_name %>% 
               get() %>% 
               magrittr::use_series("SOC") %>% 
               magrittr::extract(i)),
          Cveg = 
            (type_name %>% 
               get() %>% 
               magrittr::use_series("Cveg") %>% 
               magrittr::extract(i)),
          Total = 
            (type_name %>% 
               get() %>% 
               magrittr::use_series("Total") %>% 
               magrittr::extract(i)),
          dist_socref = dist_socref,
          dist_flu = dist_flu,
          dist_fmg = dist_fmg,
          dist_fi = dist_fi,
          dist_soc = dist_soc,
          dist_cveg = dist_cveg,
          dist_total = dist_total
        )
      ),
      climate__micro_temp$IBGE_code[i] %>% 
        as.character()
    )
  )
  
  # ---- Print progress bar
  utils::setTxtProgressBar(pb, i)
  
}

# ---- Close connection
close(pb)

# ---- Delete objects
rm(dist_socref, dist_flu, dist_fmg, dist_fi, dist_soc, dist_cveg, dist_total,
   socref_name, flu_name, fmg_name, fi_name, cveg_name)

# RESULTS ---------------------------------------------------------------------

# ---- Consolidated results
Ctotal_MCresults <- type_name %>% 
  get() %>% 
  dplyr::bind_cols(
    tibble(
      socref_est = NA,
      socref_sd = NA,
      socref_skew = NA,
      socref_cilow = NA,
      socref_ciupp = NA,
      socref_uncl = NA,
      socref_uncu = NA,
      flu_est = NA,
      flu_sd = NA,
      flu_skew = NA,
      flu_cilow = NA,
      flu_ciupp = NA,
      flu_uncl = NA,
      flu_uncu = NA,
      fmg_est = NA,
      fmg_sd = NA,
      fmg_skew = NA,
      fmg_cilow = NA,
      fmg_ciupp = NA,
      fmg_uncl = NA,
      fmg_uncu = NA,
      fi_est = NA,
      fi_sd = NA,
      fi_skew = NA,
      fi_cilow = NA,
      fi_ciupp = NA,
      fi_uncl = NA,
      fi_uncu = NA,
      soc_est = NA,
      soc_sd = NA,
      soc_skew = NA,
      soc_cilow = NA,
      soc_ciupp = NA,
      soc_uncl = NA,
      soc_uncu = NA,
      cveg_est = NA,
      cveg_sd = NA,
      cveg_skew = NA,
      cveg_cilow = NA,
      cveg_ciupp = NA,
      cveg_uncl = NA,
      cveg_uncu = NA,
      total_est = NA,
      total_sd = NA,
      total_skew = NA,
      total_cilow = NA,
      total_ciupp = NA,
      total_uncl = NA,
      total_uncu = NA
    )
  )

# ---- Message
message("Summarizing results")

# ---- Progress bar
pb <- utils::txtProgressBar(max = nrow(climate__micro_temp), style = 3)

for (i in 1:nrow(climate__micro_temp)) {
  
  Ctotal_MCresults[i, - (1:8)] <- Ctotal_samples %>%
    magrittr::extract2(
      climate__micro_temp$IBGE_code[i] %>% 
        as.character()
    ) %>% 
    dplyr::select(
      dist_socref, dist_flu, dist_fmg, dist_fi, dist_soc, dist_cveg, dist_total
    ) %>% 
    dplyr::summarise(
      socref_est = mean(dist_socref),
      flu_est    = mean(dist_flu),
      fmg_est    = mean(dist_fmg),
      fi_est     = mean(dist_fi),
      soc_est    = mean(dist_soc),
      cveg_est   = mean(dist_cveg),
      total_est  = mean(dist_total),
      socref_sd = sd(dist_socref),
      flu_sd    = sd(dist_flu),
      fmg_sd    = sd(dist_fmg),
      fi_sd     = sd(dist_fi),
      soc_sd    = sd(dist_soc),
      cveg_sd   = sd(dist_cveg),
      total_sd  = sd(dist_total),
      socref_skew = {
        dist_socref %>% { 
          ifelse(
            sd(., na.rm = TRUE) > 0, 
            e1071::skewness(.),
            NA
          )
        }
      },
      flu_skew = {
        dist_flu %>% { 
          ifelse(
            sd(., na.rm = TRUE) > 0, 
            e1071::skewness(.),
            NA
          )
        }
      },
      fmg_skew = {
        dist_fmg %>% { 
          ifelse(
            sd(., na.rm = TRUE) > 0, 
            e1071::skewness(.),
            NA
          )
        }
      },
      fi_skew = {
        dist_fi %>% { 
          ifelse(
            sd(., na.rm = TRUE) > 0, 
            e1071::skewness(.),
            NA
          )
        }
      },
      soc_skew = {
        dist_soc %>% { 
          ifelse(
            sd(., na.rm = TRUE) > 0, 
            e1071::skewness(.),
            NA
          )
        }
      },
      cveg_skew = {
        dist_cveg %>% { 
          ifelse(
            sd(., na.rm = TRUE) > 0, 
            e1071::skewness(.),
            NA
          )
        }
      },
      total_skew = {
        dist_total %>% { 
          ifelse(
            sd(., na.rm = TRUE) > 0, 
            e1071::skewness(.),
            NA
          )
        }
      },
      socref_cilow = {
        dist_socref %>% { 
          ifelse(
            sd(., na.rm = TRUE) > 0, 
            LaplacesDemon::p.interval(., prob = 0.95, HPD = TRUE, MM = FALSE)[1],
            NA
          )
        }
      },
      flu_cilow = {
        dist_flu %>% { 
          ifelse(
            sd(., na.rm = TRUE) > 0, 
            LaplacesDemon::p.interval(., prob = 0.95, HPD = TRUE, MM = FALSE)[1],
            NA
          )
        }
      },
      fmg_cilow = {
        dist_fmg %>% { 
          ifelse(
            sd(., na.rm = TRUE) > 0, 
            LaplacesDemon::p.interval(., prob = 0.95, HPD = TRUE, MM = FALSE)[1],
            NA
          )
        }
      },
      fi_cilow = {
        dist_fi %>% { 
          ifelse(
            sd(., na.rm = TRUE) > 0, 
            LaplacesDemon::p.interval(., prob = 0.95, HPD = TRUE, MM = FALSE)[1],
            NA
          )
        }
      },
      soc_cilow = {
        dist_soc %>% { 
          ifelse(
            sd(., na.rm = TRUE) > 0, 
            LaplacesDemon::p.interval(., prob = 0.95, HPD = TRUE, MM = FALSE)[1],
            NA
          )
        }
      },
      cveg_cilow = {
        dist_cveg %>% { 
          ifelse(
            sd(., na.rm = TRUE) > 0, 
            LaplacesDemon::p.interval(., prob = 0.95, HPD = TRUE, MM = FALSE)[1],
            NA
          )
        }
      },
      total_cilow = {
        dist_total %>% { 
          ifelse(
            sd(., na.rm = TRUE) > 0, 
            LaplacesDemon::p.interval(., prob = 0.95, HPD = TRUE, MM = FALSE)[1],
            NA
          )
        }
      },
      socref_ciupp = {
        dist_socref %>% { 
          ifelse(
            sd(., na.rm = TRUE) > 0, 
            LaplacesDemon::p.interval(., prob = 0.95, HPD = TRUE, MM = FALSE)[2],
            NA
          )
        }
      },
      flu_ciupp = {
        dist_flu %>% { 
          ifelse(
            sd(., na.rm = TRUE) > 0, 
            LaplacesDemon::p.interval(., prob = 0.95, HPD = TRUE, MM = FALSE)[2],
            NA
          )
        }
      },
      fmg_ciupp = {
        dist_fmg %>% { 
          ifelse(
            sd(., na.rm = TRUE) > 0, 
            LaplacesDemon::p.interval(., prob = 0.95, HPD = TRUE, MM = FALSE)[2],
            NA
          )
        }
      },
      fi_ciupp = {
        dist_fi %>% { 
          ifelse(
            sd(., na.rm = TRUE) > 0, 
            LaplacesDemon::p.interval(., prob = 0.95, HPD = TRUE, MM = FALSE)[2],
            NA
          )
        }
      },
      soc_ciupp = {
        dist_soc %>% { 
          ifelse(
            sd(., na.rm = TRUE) > 0, 
            LaplacesDemon::p.interval(., prob = 0.95, HPD = TRUE, MM = FALSE)[2],
            NA
          )
        }
      },
      cveg_ciupp = {
        dist_cveg %>% { 
          ifelse(
            sd(., na.rm = TRUE) > 0, 
            LaplacesDemon::p.interval(., prob = 0.95, HPD = TRUE, MM = FALSE)[2],
            NA
          )
        }
      },
      total_ciupp = {
        dist_total %>% { 
          ifelse(
            sd(., na.rm = TRUE) > 0, 
            LaplacesDemon::p.interval(., prob = 0.95, HPD = TRUE, MM = FALSE)[2],
            NA
          )
        }
      }
    ) %>% 
    dplyr::mutate(
      socref_uncl = (socref_est - socref_cilow) / socref_est,
      flu_uncl = (flu_est - flu_cilow) / flu_est,
      fmg_uncl = (fmg_est - fmg_cilow) / fmg_est,
      fi_uncl = (fi_est - fi_cilow) / fi_est,
      soc_uncl = (soc_est - soc_cilow) / soc_est,
      cveg_uncl = (cveg_est - cveg_cilow) / cveg_est,
      total_uncl = (total_est - total_cilow) / total_est,
      socref_uncu = (socref_ciupp - socref_est) / socref_est,
      flu_uncu = (flu_ciupp - flu_est) / flu_est,
      fmg_uncu = (fmg_ciupp - fmg_est) / fmg_est,
      fi_uncu = (fi_ciupp - fi_est) / fi_est,
      soc_uncu = (soc_ciupp - soc_est) / soc_est,
      cveg_uncu = (cveg_ciupp - cveg_est) / cveg_est,
      total_uncu = (total_ciupp - total_est) / total_est
    ) %>%
    dplyr::select(
      socref_est, socref_sd, socref_skew, socref_cilow, socref_ciupp, socref_uncl, socref_uncu,
      flu_est, flu_sd, flu_skew, flu_cilow, flu_ciupp, flu_uncl, flu_uncu,
      fmg_est, fmg_sd, fmg_skew, fmg_cilow, fmg_ciupp, fmg_uncl, fmg_uncu,
      fi_est, fi_sd, fi_skew, fi_cilow, fi_ciupp, fi_uncl, fi_uncu,
      soc_est, soc_sd, soc_skew, soc_cilow, soc_ciupp, soc_uncl, soc_uncu,
      cveg_est, cveg_sd, cveg_skew, cveg_cilow, cveg_ciupp, cveg_uncl, cveg_uncu,
      total_est, total_sd, total_skew, total_cilow, total_ciupp, total_uncl, total_uncu
    )
  
  # ---- Print progress bar
  utils::setTxtProgressBar(pb, i)
  
}

# ---- Close connection
close(pb)

# EXPORT ----------------------------------------------------------------------

# ---- Rename objects
assign(
  paste0('samples__', type_name), 
  Ctotal_samples
)

assign(
  paste0('results__', type_name), 
  Ctotal_MCresults
)

# ---- Delete objects
rm(pb, i, Ctotal_samples, Ctotal_MCresults)
gc()

# ---- Save data
save(
  list = paste0('samples__', type_name),
  file = file.path(
    'extra/applications/1__tc_microreg/samples/tc',
    paste0('samples__', type_name, '.Rda')
  )
)

# ---- Save data
save(
  list = paste0('results__', type_name),
  file = file.path(
    'extra/applications/1__tc_microreg/results',
    paste0('MC__', type_name, '.Rda')
  )
)

# ---- Delete environment
rm(list = ls())
gc()
