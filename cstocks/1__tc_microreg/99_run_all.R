library(dplyr)

setwd("C:/Users/b207056565/Desktop/brluc")

rodar_tudo <- function(){
  cat("\n"); cat("05_1A01__MC__tc__natural_land_average.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_1A01__MC__tc__natural_land_average.R"))

  cat("\n"); cat("05_2A01__MC__tc__forest_land_natural.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_2A01__MC__tc__forest_land_natural.R"))

  cat("\n"); cat("05_2B01__MC__tc__forest_land_planted_eucalyptus.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_2B01__MC__tc__forest_land_planted_eucalyptus.R"))

  cat("\n"); cat("05_2B02__MC__tc__forest_land_planted_pinus.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_2B02__MC__tc__forest_land_planted_pinus.R"))

  cat("\n"); cat("05_2B03__MC__tc__forest_land_planted_other_broadleaf.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_2B03__MC__tc__forest_land_planted_other_broadleaf.R"))

  cat("\n"); cat("05_2B04__MC__tc__forest_land_planted_average_species_2013.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_2B04__MC__tc__forest_land_planted_average_species_2013.R"))

  cat("\n"); cat("05_2B05__MC__tc__forest_land_planted_average_species_2022.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_2B05__MC__tc__forest_land_planted_average_species_2022.R"))

  cat("\n"); cat("05_3A01__MC__tc__grassland_natural.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_3A01__MC__tc__grassland_natural.R"))

  cat("\n"); cat("05_3B01__MC__tc__grassland_cultivated_severely_degraded_medium.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_3B01__MC__tc__grassland_cultivated_severely_degraded_medium.R"))

  cat("\n"); cat("05_3B02__MC__tc__grassland_cultivated_high_intensity_grazing_medium.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_3B02__MC__tc__grassland_cultivated_high_intensity_grazing_medium.R"))

  cat("\n"); cat("05_3B03__MC__tc__grassland_cultivated_moderately_degraded_medium.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_3B03__MC__tc__grassland_cultivated_moderately_degraded_medium.R"))

  cat("\n"); cat("05_3B04__MC__tc__grassland_cultivated_non_degraded_medium.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_3B04__MC__tc__grassland_cultivated_non_degraded_medium.R"))

  cat("\n"); cat("05_3B05__MC__tc__grassland_cultivated_improved_medium.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_3B05__MC__tc__grassland_cultivated_improved_medium.R"))

  cat("\n"); cat("05_3B06__MC__tc__grassland_cultivated_average_management_2003_medium.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_3B06__MC__tc__grassland_cultivated_average_management_2003_medium.R"))

  cat("\n"); cat("05_3B07__MC__tc__grassland_cultivated_average_management_2022_medium.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_3B07__MC__tc__grassland_cultivated_average_management_2022_medium.R"))

  cat("\n"); cat("05_4A01__MC__tc__cropland_temporary_generic_full_tillage_low.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_4A01__MC__tc__cropland_temporary_generic_full_tillage_low.R"))

  cat("\n"); cat("05_4A02__MC__tc__cropland_temporary_generic_full_tillage_medium.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_4A02__MC__tc__cropland_temporary_generic_full_tillage_medium.R"))

  cat("\n"); cat("05_4A03__MC__tc__cropland_temporary_generic_reduced_tillage_low.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_4A03__MC__tc__cropland_temporary_generic_reduced_tillage_low.R"))

  cat("\n"); cat("05_4A04__MC__tc__cropland_temporary_generic_reduced_tillage_medium.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_4A04__MC__tc__cropland_temporary_generic_reduced_tillage_medium.R"))

  cat("\n"); cat("05_4A05__MC__tc__cropland_temporary_generic_reduced_tillage_high.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_4A05__MC__tc__cropland_temporary_generic_reduced_tillage_high.R"))

  cat("\n"); cat("05_4A06__MC__tc__cropland_temporary_generic_no_till_low.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_4A06__MC__tc__cropland_temporary_generic_no_till_low.R"))

  cat("\n"); cat("05_4A07__MC__tc__cropland_temporary_generic_no_till_medium.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_4A07__MC__tc__cropland_temporary_generic_no_till_medium.R"))

  cat("\n"); cat("05_4A08__MC__tc__cropland_temporary_generic_no_till_high.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_4A08__MC__tc__cropland_temporary_generic_no_till_high.R"))

  cat("\n"); cat("05_4A09__MC__tc__cropland_temporary_generic_average_tillage_2006_medium.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_4A09__MC__tc__cropland_temporary_generic_average_tillage_2006_medium.R"))

  cat("\n"); cat("05_4A10__MC__tc__cropland_temporary_generic_average_tillage_2017_medium.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_4A10__MC__tc__cropland_temporary_generic_average_tillage_2017_medium.R"))

  cat("\n"); cat("05_4A11__MC__tc__cropland_temporary_sugarcane_low.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_4A11__MC__tc__cropland_temporary_sugarcane_low.R"))

  cat("\n"); cat("05_4A12__MC__tc__cropland_temporary_sugarcane_high.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_4A12__MC__tc__cropland_temporary_sugarcane_high.R"))

  cat("\n"); cat("05_4A13__MC__tc__cropland_temporary_sugarcane_average_2003.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_4A13__MC__tc__cropland_temporary_sugarcane_average_2003.R"))

  cat("\n"); cat("05_4A14__MC__tc__cropland_temporary_sugarcane_average_2023.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_4A14__MC__tc__cropland_temporary_sugarcane_average_2023.R"))

  cat("\n"); cat("05_4A15__MC__tc__cropland_temporary_paddy_rice.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_4A15__MC__tc__cropland_temporary_paddy_rice.R"))

  cat("\n"); cat("05_4B01__MC__tc__cropland_permanent_generic_full_tillage_low.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_4B01__MC__tc__cropland_permanent_generic_full_tillage_low.R"))

  cat("\n"); cat("05_4B02__MC__tc__cropland_permanent_generic_full_tillage_medium.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_4B02__MC__tc__cropland_permanent_generic_full_tillage_medium.R"))

  cat("\n"); cat("05_4B03__MC__tc__cropland_permanent_generic_reduced_tillage_low.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_4B03__MC__tc__cropland_permanent_generic_reduced_tillage_low.R"))

  cat("\n"); cat("05_4B04__MC__tc__cropland_permanent_generic_reduced_tillage_medium.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_4B04__MC__tc__cropland_permanent_generic_reduced_tillage_medium.R"))

  cat("\n"); cat("05_4B05__MC__tc__cropland_permanent_generic_reduced_tillage_high.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_4B05__MC__tc__cropland_permanent_generic_reduced_tillage_high.R"))

  cat("\n"); cat("05_4B06__MC__tc__cropland_permanent_generic_no_till_low.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_4B06__MC__tc__cropland_permanent_generic_no_till_low.R"))
  
  cat("\n"); cat("05_4B07__MC__tc__cropland_permanent_generic_no_till_medium.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_4B07__MC__tc__cropland_permanent_generic_no_till_medium.R"))
  
  cat("\n"); cat("05_4B08__MC__tc__cropland_permanent_generic_no_till_high.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_4B08__MC__tc__cropland_permanent_generic_no_till_high.R"))
  
  cat("\n"); cat("05_4B09__MC__tc__cropland_permanent_generic_average_tillage_2006_high.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_4B09__MC__tc__cropland_permanent_generic_average_tillage_2006_high.R"))
  
  cat("\n"); cat("05_4B10__MC__tc__cropland_permanent_generic_average_tillage_2017_high.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_4B10__MC__tc__cropland_permanent_generic_average_tillage_2017_high.R"))
  
  cat("\n"); cat("05_4B11__MC__tc__cropland_permanent_oil_palm_no_till_high.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_4B11__MC__tc__cropland_permanent_oil_palm_no_till_high.R"))
  
  cat("\n"); cat("05_4B12__MC__tc__cropland_permanent_oil_palm_full_tillage_high.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_4B12__MC__tc__cropland_permanent_oil_palm_full_tillage_high.R"))
  
  cat("\n"); cat("05_4B13__MC__tc__cropland_permanent_rubber_no_till_high.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_4B13__MC__tc__cropland_permanent_rubber_no_till_high.R"))
  
  cat("\n"); cat("05_4B14__MC__tc__cropland_permanent_rubber_full_tillage_high.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_4B14__MC__tc__cropland_permanent_rubber_full_tillage_high.R"))
  
  cat("\n"); cat("05_5A01__MC__tc__wetland_generic.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_5A01__MC__tc__wetland_generic.R"))
  
  cat("\n"); cat("05_6A01__MC__tc__settlements_generic.R"); cat("\n"); cat(as.character(Sys.time())); cat("\n");
  source(file.path('extra/applications/1__tc_microreg', "05_6A01__MC__tc__settlements_generic.R"))
}

rodar_tudo()

rm(list = ls())
gc()

source("C:/Users/b207056565/Desktop/brluc/extra/applications/1__tc_microreg/results/tables/0__export_results.R")

rm(list = ls())
gc()

source("C:/Users/b207056565/Desktop/brluc/extra/applications/1__tc_microreg/results/tables/6__maps_tc_unc.R")

rm(list = ls())
gc()

source("C:/Users/b207056565/Desktop/brluc/extra/applications/1__tc_microreg/results/tables/2__descriptive_analysis_MCdist.R")

rm(list = ls())
gc()

source("C:/Users/b207056565/Desktop/brluc/extra/applications/1__tc_microreg/results/tables/3__correlations_MCdist.R")

rm(list = ls())
gc()

source("C:/Users/b207056565/Desktop/brluc/extra/applications/2__tc_diff_luc_microreg/0__compute_diff_dist.R")

rm(list = ls())
gc()
