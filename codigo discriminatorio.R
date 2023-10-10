# Carga las bibliotecas necesarias

library(readxl)
library(dplyr)
library(openxlsx)

# Define la ruta de tu archivo Excel

datos <- read_xlsx("/Users/leonardoperezvictorino/ZMVM99-19/Bases/BLzm19_final.xlsx")


# Aplicar las condiciones a las variables QLue y PR

datos <- datos %>%
  mutate_at(vars(starts_with("QL")), ~ifelse(. > 1, ., "-")) %>%
  mutate_at(vars(starts_with("PR")), ~ifelse(. > 0.5, ., "-"))

# Guardar los datos procesados en un nuevo archivo Excel

write.xlsx(datos, "/Users/leonardoperezvictorino/ZMVM99-19/Productos/BLzm19_2.xlsx")