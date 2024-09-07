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
library(janitor)
library(openxlsx)

# SETTINGS --------------------------------------------------------------------
spreadsheet_name <- "Carbon stock per microregion_BRLUC_2.07_v69.xlsx"

# ------------------------------------------------------------- #
## Limites da planilha 'Legend': Tabela 'BRLUC Cstock classes' ##
# ------------------------------------------------------------- #
range_legend <- "G3:M50" # <- Verificar a cada nova versao!

# DATA SETS -------------------------------------------------------------------

## Data set: Legend (BRLUC Cstock classes) ----
legend_df <- readxl::read_excel(
  path = paste0('planilhas/', spreadsheet_name), 
  sheet = "Legend", range = range_legend) %>% 
  janitor::clean_names() %>% 
  dplyr::filter(
    level_1 %in% c(
      "Natural land", "Forest land", "Grassland", "Cropland",
      "Wetland", "Settlements"
    )
  ) %>% 
  dplyr::select(- examples) %>% 
  dplyr::mutate(label = janitor::make_clean_names(name))

## Data set: Microregions codes and names ----
micro_df <- 
  readxl::read_excel(
    path = paste0('planilhas/', spreadsheet_name),
    sheet = "Climates Micro", range = "A3:J560",
    col_names = c(
      "IBGE_code", "name", "tropical__dry", "tropical__moist", 
      "tropical__montane", "tropical__wet", "warm_temperate__moist",
      "total", "area__ha", "abbr"
    )
  ) %>% 
  dplyr::select(IBGE_code, name) %>% 
  dplyr::filter(!is.na(IBGE_code)) %>% 
  dplyr::rename(
    `Microregion IBGE code` = IBGE_code,
    `Microregion name` = name
  )
rm(range_legend, spreadsheet_name)
  
# EXPORT RESULTS --------------------------------------------------------------

# ---- Create a new spreadsheet
wb <- openxlsx::createWorkbook()

# ---- Add a sheet: "Total Carbon Stocks" ----
openxlsx::addWorksheet(wb, sheetName = "Total Carbon Stocks")

# ---- Add microregions codes and names
openxlsx::writeData(
  wb, sheet = "Total Carbon Stocks", x = micro_df, 
  startCol = 1, startRow = 2, 
  rowNames = FALSE
)

# ---- Add LU name
openxlsx::writeData(
  wb, sheet = "Total Carbon Stocks", x = names(micro_df)[1], 
  startCol = 1, startRow = 1, 
  rowNames = FALSE
)

openxlsx::writeData(
  wb, sheet = "Total Carbon Stocks", x = names(micro_df)[2], 
  startCol = 2, startRow = 1, 
  rowNames = FALSE
)

# ---- Merge names cells
openxlsx::mergeCells(wb, sheet = "Total Carbon Stocks", cols = 1, rows = 1:2)
openxlsx::mergeCells(wb, sheet = "Total Carbon Stocks", cols = 2, rows = 1:2)

# ---- Head style
h_style <- openxlsx::createStyle(
  halign = "center", valign = "center",
  border = "TopBottomLeftRight", textDecoration = "bold",
  fgFill = "#aee6cd"
)

# ---- Add style
openxlsx::addStyle(
  wb, sheet = "Total Carbon Stocks", cols = 1, rows = 1:2,
  style = h_style
)

openxlsx::addStyle(
  wb, sheet = "Total Carbon Stocks", cols = 2, rows = 1:2,
  style = h_style
)

# ---- Search 
for (k in 1:nrow(legend_df)) {
  
  # ---- Load result
  load(file.path(
    'extra/applications/1__tc_microreg/results', 
    paste0("MC__tc__", legend_df$label[k], ".Rda")
  ))
  
  # ---- Create DF with estimate and uncertainty
  df_unc <- get(paste0("results__tc__", legend_df$label[k])) %>% 
    dplyr::select(
      total_est, total_sd, total_cilow, total_ciupp, total_uncl, total_uncu
    ) %>% 
    dplyr::mutate(
      total_est = round(total_est, 1),
      total_sd = round(total_sd, 1),
      total_cilow = round(total_cilow, 1),
      total_ciupp = round(total_ciupp, 1),
      Unc = ifelse(
        test = total_uncu > total_uncl, 
        yes = total_uncu, 
        no = total_uncl
      )
    ) %>% 
    dplyr::select(total_est, total_sd, total_cilow, total_ciupp, Unc) %>% 
    dplyr::rename(
      `TC` = total_est,
      `SE` = total_sd,
      `CI95% Low` = total_cilow,
      `CI95% Upp` = total_ciupp
    )
  rm(list = paste0("results__tc__", legend_df$label[k]))
  
  # ---- Add estimated Carbon
  openxlsx::writeData(
    wb, sheet = "Total Carbon Stocks", x = df_unc, 
    startCol = (5 * (k - 1) + 3), startRow = 2, 
    rowNames = FALSE
  )
  
  # ---- Add LU name
  openxlsx::writeData(
    wb, sheet = "Total Carbon Stocks", x = legend_df$name[k], 
    startCol = (5 * (k - 1) + 3), startRow = 1, 
    rowNames = FALSE
  )
  
  # ---- Merge names cells
  openxlsx::mergeCells(
    wb, sheet = "Total Carbon Stocks", 
    cols = (5 * (k - 1) + 3):(5 * (k - 1) + 7), 
    rows = 1
  )
  
  # ---- Add style
  openxlsx::addStyle(
    wb, sheet = "Total Carbon Stocks", 
    cols = (5 * (k - 1) + 3):(5 * (k - 1) + 7), 
    rows = 1,
    style = h_style
  )
  
  # ---- Add style
  openxlsx::addStyle(
    wb, sheet = "Total Carbon Stocks",
    cols = (5 * (k - 1) + 3):(5 * (k - 1) + 7), 
    rows = 2,
    style = h_style
  )
  
  # ---- Add style
  openxlsx::addStyle(
    wb, sheet = "Total Carbon Stocks", cols = (5 * (k - 1) + 7), rows = 3:560,
    style = openxlsx::createStyle(numFmt = "0.0%")
  )
  
}

#####

# ---- Add a sheet: "Uncertainty Decomposition" ----
openxlsx::addWorksheet(wb, sheetName = "Uncertainty Decomposition")

# ---- Add microregions codes and names
openxlsx::writeData(
  wb, sheet = "Uncertainty Decomposition", x = micro_df, 
  startCol = 1, startRow = 2, 
  rowNames = FALSE
)

# ---- Add LU name
openxlsx::writeData(
  wb, sheet = "Uncertainty Decomposition", x = names(micro_df)[1], 
  startCol = 1, startRow = 1, 
  rowNames = FALSE
)

openxlsx::writeData(
  wb, sheet = "Uncertainty Decomposition", x = names(micro_df)[2], 
  startCol = 2, startRow = 1, 
  rowNames = FALSE
)

# ---- Merge names cells
openxlsx::mergeCells(wb, sheet = "Uncertainty Decomposition", cols = 1, rows = 1:2)
openxlsx::mergeCells(wb, sheet = "Uncertainty Decomposition", cols = 2, rows = 1:2)

# ---- Head style
h_style <- openxlsx::createStyle(
  halign = "center", valign = "center",
  border = "TopBottomLeftRight", textDecoration = "bold",
  fgFill = "#dcb0e8"
)

# ---- Add style
openxlsx::addStyle(
  wb, sheet = "Uncertainty Decomposition", cols = 1, rows = 1:2,
  style = h_style
)

openxlsx::addStyle(
  wb, sheet = "Uncertainty Decomposition", cols = 2, rows = 1:2,
  style = h_style
)

# ---- Search 
for (k in 1:nrow(legend_df)) {
  
  # ---- Load result
  load(file.path(
    'extra/applications/1__tc_microreg/results/tables/uncertainty_decomposition', 
    paste0("unc_decomp__tc__", legend_df$label[k], ".Rda")
  ))
  
  # ---- Create DF with estimate and uncertainty
  df_decomp <- get(paste0("unc_decomp__tc__", legend_df$label[k])) %>% 
    dplyr::select(
      dcv2tot_cveg, dcv2tot_socref, dcv2tot_flu, dcv2tot_fmg, dcv2tot_fi
    ) %>% 
    dplyr::rename(
      `% Unc CVeg` = dcv2tot_cveg, 
      `% Unc SOCref` = dcv2tot_socref, 
      `% Unc FLU` = dcv2tot_flu, 
      `% Unc FMG` = dcv2tot_fmg, 
      `% Unc FI` = dcv2tot_fi
    )
  rm(list = paste0("unc_decomp__tc__", legend_df$label[k]))
  
  # ---- Add estimated % uncertainty
  openxlsx::writeData(
    wb, sheet = "Uncertainty Decomposition", x = df_decomp, 
    startCol = (3 * k + 2 * (k - 1)), startRow = 2, 
    rowNames = FALSE
  )
  
  # ---- Add LU name
  openxlsx::writeData(
    wb, sheet = "Uncertainty Decomposition", x = legend_df$name[k], 
    startCol = (3 * k + 2 * (k - 1)), startRow = 1, 
    rowNames = FALSE
  )
  
  # ---- Merge names cells
  openxlsx::mergeCells(
    wb, sheet = "Uncertainty Decomposition", 
    cols = (3 * k + 2 * (k - 1)):(3 * k + 2 * (k - 1) + 4), rows = 1
  )
  
  # ---- Add style
  openxlsx::addStyle(
    wb, sheet = "Uncertainty Decomposition", 
    cols = (3 * k + 2 * (k - 1)):(3 * k + 2 * (k - 1) + 4), rows = 1,
    style = h_style
  )
  
  # ---- Add style
  openxlsx::addStyle(
    wb, sheet = "Uncertainty Decomposition", 
    cols = (3 * k + 2 * (k - 1)):(3 * k + 2 * (k - 1) + 4), rows = 2,
    style = h_style
  )
  
}

for (j in (3:(3 * k + 2 * (k - 1) + 4))) {
  # ---- Add style
  openxlsx::addStyle(
    wb, sheet = "Uncertainty Decomposition", 
    cols = j, rows = 3:560,
    style = openxlsx::createStyle(numFmt = "0.0%")
  )
}

#####

# ---- Save spreadsheet
openxlsx::saveWorkbook(
  wb, 
  file = paste0(
    'extra/applications/1__tc_microreg/results/tables/tabelas_usuario/',
    'brluc3_stocks_unc',
    # gsub("-", "", as.character(Sys.Date())),
    '.xlsx'
  ),
  overwrite = TRUE
)

# CLEANING ENVIRONMENT --------------------------------------------------------
rm(list = ls())
gc()
