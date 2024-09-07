# ------------------------------- #
# |  EMBRAPA                    | #
# |  ENVIRONMENT                | #
# ------------------------------- #
# ------------------------------- #
# |  Brazilian Land Use Change  | #
# |  BRLUC                      | #
# ------------------------------- #

# ------------------------------- #
# |  Uncertainty for BRLUC      | #
# ------------------------------- #

# LIBRARIES -------------------------------------------------------------------
# Function to check and install a package if necessary
install_if_needed <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name)
    library(package_name, character.only = TRUE)
  }
}

# Check packages
install_if_needed("dplyr")
install_if_needed("stringr")
install_if_needed("janitor")
install_if_needed("openxlsx")

# EXPORT ----------------------------------------------------------------------
# Export results
source("cstocks/1__tc_microreg/results/tables/9a__exports_xlsx.R")
message(paste0('Results exported to ', getwd(), '/results')) 
