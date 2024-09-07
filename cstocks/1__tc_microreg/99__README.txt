5INFORMAÇÕES GERAIS:

- MODELO MAIS SIMPLES (INGÊNUO):
	- Distribuição Normal para todos os fatores (FLU, FMG e FI)
	- Independência entre os fatores
	- Sem censura na distribuição dos fatores
	- Climas considerados fixos

- PREDIÇÕES:
	- Estimativa pontual: Média
	- Estimativa intervalar: HPD

------------------------------------------------------------------------

- NUMERAÇÃO DOS SCRIPTS:

# 00: Leitura de dados

# 01: Amostragem FLU - Planilha 'Stock change factors'
      Subnível: 1 - Cropland
                2 - Grassland
      Letras: Ordem do 'management'

# 02: Amostragem FMG - Planilha 'Stock change factors'
      Subnível: 1 - Cropland
                2 - Grassland
      Letras: Ordem do 'management'

# 03: Amostragem FI - Planilha 'Stock change factors'
      Subnível: 1 - Cropland
                2 - Grassland
      Letras: Ordem do 'management'

# 04: Amostragem SOCref - Planilha 'ParametersR'

# 05: Amostragem CVeg e SOC (total) - Complemento da planilha 'CCalcMicro'
      Subníveis: 01 - Natural
                 02 - Forest
                    A - Natural 
                    B - Planted  
                 03 - Grassland
                    A - Natural 
                    B - Cultivated  
                 04 - Cropland
                    A - Temporary 
                    B - Permanent
                 05 - Wetland
                    A - Generic
                 06 - Settlements
                    A - Generic
