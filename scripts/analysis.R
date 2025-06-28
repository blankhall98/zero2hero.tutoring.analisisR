# Analisis Estadistico
# Nombre: Sofia Santillan

library(readxl)      # Leer Excel
library(dplyr)       # Manipulación de datos
library(tableone)    # Tabla 1 automática
library(survival)    # KM y Cox
library(survminer)   # Gráficos de supervivencia
library(broom)       # “tidy” para regresiones

# 1. Leer la base de datos
data <- read_excel("data/data.xlsx")

# 2. Revisar nombres de variables y tipos
glimpse(data)

# 3. Clasificar entre variables categoricas y continuas
categorical = c("Sexo","Tabaquismo","DT2","HTA","ERC","Consumo_alcohol",
                "Grupo","Fractura","Fractura_vertebral","Fractura_cadera",
                "Fractura_huesos_largos")
continual = c("Edad","IMC","Tiempo_fractura","Tiempo_vertebral","Tiempo_cadera","Tiempo_huesos_largos")

# 4. Valores faltantes
sum(is.na(data))

# 5. Tabla
tabla1 <- CreateTableOne(vars = c(categorical, continual),
                         data = data,
                         factorVars = categorical,
                         strata = "Grupo")
print(tabla1, showAllLevels = TRUE)
