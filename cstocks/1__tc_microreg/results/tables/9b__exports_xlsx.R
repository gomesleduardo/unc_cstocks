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

# ---- Add a sheet
openxlsx::addWorksheet(wb, sheetName = "Total Carbon Stocks")

# ---- Add microregions codes and names
openxlsx::writeData(
  wb, sheet = "Total Carbon Stocks", x = micro_df, 
  startCol = 1, startRow = 7, 
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

# ---- Add level names
openxlsx::writeData(
  wb, sheet = "Total Carbon Stocks", x = c(paste("Level", 1:5), "Land Use", "Stats"), 
  startCol = 3, startRow = 1, 
  rowNames = FALSE
)

# ---- Merge names cells
openxlsx::mergeCells(wb, sheet = "Total Carbon Stocks", cols = 1, rows = 1:7)
openxlsx::mergeCells(wb, sheet = "Total Carbon Stocks", cols = 2, rows = 1:7)

# ---- Head style
h_style <- openxlsx::createStyle(
  halign = "center", valign = "center",
  border = "TopBottomLeftRight", textDecoration = "bold",
  fgFill = "#aee6cd"
)

# ---- Class style
c_style <- openxlsx::createStyle(
  halign = "center", valign = "center",
  border = "TopBottomLeftRight", textDecoration = "bold",
  fgFill = "#aee6cd"
)

# ---- Add style
openxlsx::addStyle(wb, sheet = "Total Carbon Stocks", cols = 1, rows = 1:7, style = h_style)
openxlsx::addStyle(wb, sheet = "Total Carbon Stocks", cols = 2, rows = 1:7, style = h_style)
openxlsx::addStyle(wb, sheet = "Total Carbon Stocks", cols = 3, rows = 1:7, style = h_style)

# ---- Search 
for (k in 1:nrow(legend_df)) {
  
  # ---- Load result
  load(file.path(
    'extra/applications/1__tc_microreg/results', 
    paste0("MC__tc__", legend_df$label[k], ".Rda")
  ))
  
  # ---- Create DF with estimate and uncertainty
  df_unc <- get(paste0("results__tc__", legend_df$label[k])) %>% 
    dplyr::select(total_est, total_uncl, total_uncu) %>% 
    dplyr::mutate(Unc = ifelse(
      test = total_uncu > total_uncl, 
      yes = total_uncu, 
      no = total_uncl
    )) %>%
    dplyr::mutate(
      total_est = round(total_est, 1),
      Unc = round(Unc, 3)
    ) %>% 
    dplyr::select(total_est, Unc) %>% 
    dplyr::rename(
      `TC` = total_est
    )
  rm(list = paste0("results__tc__", legend_df$label[k]))
  
  # ---- Add estimated Carbon
  openxlsx::writeData(
    wb, sheet = "Total Carbon Stocks", x = df_unc, 
    startCol = (2 * k + 2), startRow = 7, 
    rowNames = FALSE
  )
  
  # ---- Add LU name
  openxlsx::writeData(
    wb, sheet = "Total Carbon Stocks", x = legend_df$name[k], 
    startCol = (2 * k + 2), startRow = 6, 
    rowNames = FALSE
  )
  
  # ---- Add level 1 name
  openxlsx::writeData(
    wb, sheet = "Total Carbon Stocks", x = legend_df$level_1[k], 
    startCol = (2 * k + 2), startRow = 1, 
    rowNames = FALSE
  )
  
  # ---- Add level 2 name
  openxlsx::writeData(
    wb, sheet = "Total Carbon Stocks", x = legend_df$level_2[k], 
    startCol = (2 * k + 2), startRow = 2, 
    rowNames = FALSE
  )
  
  # ---- Add level 3 name
  openxlsx::writeData(
    wb, sheet = "Total Carbon Stocks", x = legend_df$level_3[k], 
    startCol = (2 * k + 2), startRow = 3, 
    rowNames = FALSE
  )
  
  # ---- Add level 4 name
  openxlsx::writeData(
    wb, sheet = "Total Carbon Stocks", x = legend_df$level_4[k], 
    startCol = (2 * k + 2), startRow = 4, 
    rowNames = FALSE
  )
  
  # ---- Add level 5 name
  openxlsx::writeData(
    wb, sheet = "Total Carbon Stocks", x = legend_df$level_5[k], 
    startCol = (2 * k + 2), startRow = 5, 
    rowNames = FALSE
  )
  
  # ---- Merge names cells
  openxlsx::mergeCells(
    wb, sheet = "Total Carbon Stocks", cols = (2 * k + 2):(2 * k + 3), rows = 3
  )
  openxlsx::mergeCells(
    wb, sheet = "Total Carbon Stocks", cols = (2 * k + 2):(2 * k + 3), rows = 4
  )
  openxlsx::mergeCells(
    wb, sheet = "Total Carbon Stocks", cols = (2 * k + 2):(2 * k + 3), rows = 5
  )
  openxlsx::mergeCells(
    wb, sheet = "Total Carbon Stocks", cols = (2 * k + 2):(2 * k + 3), rows = 6
  )
  
  # ---- Add style
  openxlsx::addStyle(
    wb, sheet = "Total Carbon Stocks", cols = (2 * k + 2):(2 * k + 3), rows = 3,
    style = c_style
  )
  openxlsx::addStyle(
    wb, sheet = "Total Carbon Stocks", cols = (2 * k + 2):(2 * k + 3), rows = 4,
    style = c_style
  )
  openxlsx::addStyle(
    wb, sheet = "Total Carbon Stocks", cols = (2 * k + 2):(2 * k + 3), rows = 5,
    style = c_style
  )
  openxlsx::addStyle(
    wb, sheet = "Total Carbon Stocks", cols = (2 * k + 2):(2 * k + 3), rows = 6,
    style = h_style
  )
  openxlsx::addStyle(
    wb, sheet = "Total Carbon Stocks", cols = (2 * k + 2):(2 * k + 3), rows = 7,
    style = h_style
  )
  
  # ---- Add style
  openxlsx::addStyle(
    wb, sheet = "Total Carbon Stocks", cols = (2 * k + 3), rows = 8:565,
    style = openxlsx::createStyle(numFmt = "0.0%")
  )
  
}

# ---- Merge names cells
openxlsx::mergeCells(wb, sheet = "Total Carbon Stocks", cols = 4:5, rows = 1)
openxlsx::mergeCells(wb, sheet = "Total Carbon Stocks", cols = 6:17, rows = 1)
openxlsx::mergeCells(wb, sheet = "Total Carbon Stocks", cols = 18:33, rows = 1)
openxlsx::mergeCells(wb, sheet = "Total Carbon Stocks", cols = 34:91, rows = 1)
openxlsx::mergeCells(wb, sheet = "Total Carbon Stocks", cols = 92:93, rows = 1)
openxlsx::mergeCells(wb, sheet = "Total Carbon Stocks", cols = 94:95, rows = 1)
openxlsx::mergeCells(wb, sheet = "Total Carbon Stocks", cols = 4:5, rows = 2)
openxlsx::mergeCells(wb, sheet = "Total Carbon Stocks", cols = 6:7, rows = 2)
openxlsx::mergeCells(wb, sheet = "Total Carbon Stocks", cols = 8:17, rows = 2)
openxlsx::mergeCells(wb, sheet = "Total Carbon Stocks", cols = 18:19, rows = 2)
openxlsx::mergeCells(wb, sheet = "Total Carbon Stocks", cols = 20:33, rows = 2)
openxlsx::mergeCells(wb, sheet = "Total Carbon Stocks", cols = 34:63, rows = 2)
openxlsx::mergeCells(wb, sheet = "Total Carbon Stocks", cols = 64:91, rows = 2)
openxlsx::mergeCells(wb, sheet = "Total Carbon Stocks", cols = 92:93, rows = 2)
openxlsx::mergeCells(wb, sheet = "Total Carbon Stocks", cols = 94:95, rows = 2)

# ---- Add style
for (k in 4:95) {
  openxlsx::addStyle(wb, sheet = "Total Carbon Stocks", cols = k, rows = 1, style = c_style)
  openxlsx::addStyle(wb, sheet = "Total Carbon Stocks", cols = k, rows = 2, style = c_style)
}

# ---- Save spreadsheet
openxlsx::saveWorkbook(
  wb, 
  file = paste0(
    'extra/applications/1__tc_microreg/results/tables/tabelas_usuario/',
    'brluc2_stocks_unc',
    # gsub("-", "", as.character(Sys.Date())),
    '.xlsx'
  ),
  overwrite = TRUE
)

# CLEANING ENVIRONMENT --------------------------------------------------------
rm(list = ls())
gc()
