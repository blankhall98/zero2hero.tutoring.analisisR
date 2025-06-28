# Analisis Estadistico
# Nombre: Sofia Santillan

library(readxl)      # Leer Excel
library(dplyr)       # Manipulación de datos
library(tableone)    # Tabla 1 automática
library(survival)    # KM y Cox
library(survminer)   # Gráficos de supervivencia
library(broom)       # “tidy” para regresiones

######################### Analisis Basal ###################################
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

# 5. Tabla: Caracteristicas Basales
tabla1 <- CreateTableOne(vars = c(categorical, continual),
                         data = data,
                         factorVars = categorical,
                         strata = "Grupo")
print(tabla1, showAllLevels = TRUE)

################ Analisis de Supervivencia ########################################
surv_obj <- Surv(time = data$Tiempo_fractura, event = data$Fractura) #objeto de supervivencia
fit_km  <- survfit(surv_obj ~ Grupo, data = data) #KM por grupos

#Grafica de KM
ggsurvplot(
  fit_km,
  data            = data,
  risk.table      = TRUE,
  pval            = TRUE,
  conf.int        = TRUE,
  xlab            = "Días de seguimiento",
  legend.title    = "Grupo",
  legend.labs     = c("Control","Intervención"),
  risk.table.height = 0.25
) -> plt_km

ggsave("outputs/Fig1_KM.png", plt_km$plot, width = 6, height = 4)

#Modelo de Cox
fit_cox <- coxph(
  surv_obj ~ Sexo + IMC + Edad + DT2 + HTA + Tabaquismo + Consumo_alcohol,
  data = data
)

tabla_cox <- broom::tidy(fit_cox, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(!term %in% c("(Intercept)")) %>%
  mutate(across(c(estimate, conf.low, conf.high), ~round(.x,2)),
         p.value = round(p.value,3)) %>%
  rename(HR=estimate, IC_2.5=conf.low, IC_97.5=conf.high)

write.csv(tabla_cox, "outputs/Tabla_Cox.csv", row.names = FALSE)
print(tabla_cox)

#grafica de efectos
ggforest(fit_cox, data = data) +
  ggtitle("Forest plot: Cox PH con interacciones")
ggsave("outputs/Fig2_Forest_Cox.png", width = 6, height = 4)

### Supervicencia -- Fractura Vertebral
surv_obj <- Surv(time = data$Tiempo_vertebral, event = data$Fractura_vertebral) #objeto de supervivencia
fit_km  <- survfit(surv_obj ~ Grupo, data = data) #KM por grupos

#Grafica de KM
ggsurvplot(
  fit_km,
  data            = data,
  risk.table      = TRUE,
  pval            = TRUE,
  conf.int        = TRUE,
  xlab            = "Días de seguimiento",
  legend.title    = "Grupo",
  legend.labs     = c("Control","Intervención"),
  risk.table.height = 0.25
) -> plt_km

ggsave("outputs/Fig1_KM_vertebral.png", plt_km$plot, width = 6, height = 4)

#Modelo de Cox -- fractura vertebral
fit_cox <- coxph(
  surv_obj ~ Sexo + IMC + Edad + DT2 + HTA + Tabaquismo + Consumo_alcohol,
  data = data
)

tabla_cox <- broom::tidy(fit_cox, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(!term %in% c("(Intercept)")) %>%
  mutate(across(c(estimate, conf.low, conf.high), ~round(.x,2)),
         p.value = round(p.value,3)) %>%
  rename(HR=estimate, IC_2.5=conf.low, IC_97.5=conf.high)

write.csv(tabla_cox, "outputs/Tabla_Cox_vertebral.csv", row.names = FALSE)
print(tabla_cox)

#grafica de efectos
ggforest(fit_cox, data = data) +
  ggtitle("Forest plot: Cox PH con interacciones")
ggsave("outputs/Fig2_Forest_Cox_vertebral.png", width = 6, height = 4)

### Supervicencia -- Fractura Cadera
surv_obj <- Surv(time = data$Tiempo_cadera, event = data$Fractura_cadera) #objeto de supervivencia
fit_km  <- survfit(surv_obj ~ Grupo, data = data) #KM por grupos

#Grafica de KM
ggsurvplot(
  fit_km,
  data            = data,
  risk.table      = TRUE,
  pval            = TRUE,
  conf.int        = TRUE,
  xlab            = "Días de seguimiento",
  legend.title    = "Grupo",
  legend.labs     = c("Control","Intervención"),
  risk.table.height = 0.25
) -> plt_km

ggsave("outputs/Fig1_KM_cadera.png", plt_km$plot, width = 6, height = 4)

#Modelo de Cox -- fractura cadera
fit_cox <- coxph(
  surv_obj ~ Sexo + IMC + Edad + DT2 + HTA + Tabaquismo + Consumo_alcohol,
  data = data
)

tabla_cox <- broom::tidy(fit_cox, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(!term %in% c("(Intercept)")) %>%
  mutate(across(c(estimate, conf.low, conf.high), ~round(.x,2)),
         p.value = round(p.value,3)) %>%
  rename(HR=estimate, IC_2.5=conf.low, IC_97.5=conf.high)

write.csv(tabla_cox, "outputs/Tabla_Cox_cadera.csv", row.names = FALSE)
print(tabla_cox)

#grafica de efectos
ggforest(fit_cox, data = data) +
  ggtitle("Forest plot: Cox PH con interacciones")
ggsave("outputs/Fig2_Forest_Cox_cadera.png", width = 6, height = 4)

### Supervicencia -- Fractura Huesos Largos
surv_obj <- Surv(time = data$Tiempo_huesos_largos, event = data$Fractura_huesos_largos) #objeto de supervivencia
fit_km  <- survfit(surv_obj ~ Grupo, data = data) #KM por grupos

#Grafica de KM
ggsurvplot(
  fit_km,
  data            = data,
  risk.table      = TRUE,
  pval            = TRUE,
  conf.int        = TRUE,
  xlab            = "Días de seguimiento",
  legend.title    = "Grupo",
  legend.labs     = c("Control","Intervención"),
  risk.table.height = 0.25
) -> plt_km

ggsave("outputs/Fig1_KM_huesos_largos.png", plt_km$plot, width = 6, height = 4)

#Modelo de Cox -- fractura vertebral
fit_cox <- coxph(
  surv_obj ~ Sexo + IMC + Edad + DT2 + HTA + Tabaquismo + Consumo_alcohol,
  data = data
)

tabla_cox <- broom::tidy(fit_cox, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(!term %in% c("(Intercept)")) %>%
  mutate(across(c(estimate, conf.low, conf.high), ~round(.x,2)),
         p.value = round(p.value,3)) %>%
  rename(HR=estimate, IC_2.5=conf.low, IC_97.5=conf.high)

write.csv(tabla_cox, "outputs/Tabla_Cox_huesos_largos.csv", row.names = FALSE)
print(tabla_cox)

#grafica de efectos
ggforest(fit_cox, data = data) +
  ggtitle("Forest plot: Cox PH con interacciones")
ggsave("outputs/Fig2_Forest_Cox_huesos_largos.png", width = 6, height = 4)

################ Regresion Logistica ####################
data <- data %>%
  mutate(
    Grupo = as.factor(Grupo),
    Sexo  = as.factor(Sexo),
    DT2 = as.factor(DT2),
    HTA = as.factor(HTA),
    ERC = as.factor(ERC),
    Consumo_alcohol = as.factor(Consumo_alcohol),
    Tabaquismo = as.factor(Tabaquismo),
    Fractura = as.factor(Fractura),
    Fractura_vertebral = as.factor(Fractura_vertebral),
    Fractura_cadera = as.factor(Fractura_cadera),
    Fractura_huesos_largos = as.factor(Fractura_huesos_largos)
  )

# Variables predictoras comunes
preds <- c("Grupo", "Edad", "Sexo", "IMC", "DT2", "HTA","Consumo_alcohol","Tabaquismo")

# 2. Lista de desenlaces
outcomes <- c(
  Fractura_general      = "Fractura",
  Fractura_vertebral    = "Fractura_vertebral",
  Fractura_cadera       = "Fractura_cadera",
  Fractura_huesos_largos= "Fractura_huesos_largos"
)

for(name in names(outcomes)) {
  outcome <- outcomes[[name]]
  
  # (a) Ajustar el modelo
  fmla <- as.formula(paste0(outcome, " ~ ", paste(preds, collapse = " + ")))
  mod  <- glm(fmla, data = data, family = binomial)
  
  # (b) Extraer tabla tidy con OR, IC95% y p
  tab <- tidy(mod, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    transmute(
      Variable = term,
      OR       = round(estimate, 2),
      IC_2.5   = round(conf.low, 2),
      IC_97.5  = round(conf.high, 2),
      p_value  = round(p.value, 3)
    ) %>%
    # Añadir niveles de significancia
    mutate(Signif = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      TRUE            ~ ""
    ))
  
  # Guardar CSV
  write.csv(
    tab,
    file = paste0("outputs/Tabla3_Logistica_", name, ".csv"),
    row.names = FALSE
  )
  
  # Imprimir por consola
  message("\nTabla para ", name, ":\n")
  print(tab)
  
  # (c) Forest-plot de OR con asteriscos
  plt <- ggplot(tab, aes(x = Variable, y = OR, ymin = IC_2.5, ymax = IC_97.5)) +
    geom_pointrange() +
    geom_hline(yintercept = 1, linetype = "dashed") +
    coord_flip() +
    # Asteriscos
    geom_text(aes(label = Signif), hjust = -0.2, size = 5) +
    # Expandir eje Y para que entran los asteriscos
    scale_y_continuous(expand = expansion(mult = c(0.1, 0.2))) +
    labs(
      title = paste("OR e IC 95% –", name),
      x     = NULL,
      y     = "OR (IC 95%)"
    ) +
    theme_bw() +
    theme(
      panel.grid.major = element_line(color = "grey80"),
      panel.grid.minor = element_blank(),
      plot.background   = element_rect(fill = "white")
    )
  
  # Guardar figura
  ggsave(
    filename = paste0("outputs/Fig3_OR_Logistica_", name, ".png"),
    plot     = plt,
    width    = 6,
    height   = 4
  )
}

############################ Regresion Lineal #############################
time_vars <- c(
  Tiempo_fractura       = "Fractura",
  Tiempo_vertebral      = "Fractura_vertebral",
  Tiempo_cadera         = "Fractura_cadera",
  Tiempo_huesos_largos  = "Fractura_huesos_largos"
)


# Loop para cada variable de tiempo
for(time_var in names(time_vars)) {
  event_var <- time_vars[[time_var]]
  
  # 1) Subconjunto solo con casos del evento
  df_sub <- data %>%
    filter(.data[[event_var]] == 1, !is.na(.data[[time_var]]))
  
  # 2) Fórmula del modelo
  fmla_lin <- as.formula(
    paste0(time_var, " ~ ", paste(preds, collapse = " + "))
  )
  
  # 3) Ajustar lm
  mod_lin <- lm(fmla_lin, data = df_sub)
  
  # 4) Extraer tabla con Betas, IC y p
  tab_lin <- tidy(mod_lin, conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    transmute(
      Variable = term,
      Beta     = round(estimate, 2),
      IC_2.5   = round(conf.low, 2),
      IC_97.5  = round(conf.high, 2),
      p_value  = round(p.value, 3)
    ) %>%
    mutate(Signif = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      TRUE            ~ ""
    ))
  
  # 5) Guardar CSV y mostrar
  write.csv(
    tab_lin,
    file = paste0("outputs/Tabla4_Lineal_", time_var, ".csv"),
    row.names = FALSE
  )
  message("\nTabla de regresión lineal para ", time_var, ":\n")
  print(tab_lin)
  
  # 6) Forest‐plot de Betas
  plt_lin <- ggplot(tab_lin, 
                    aes(x = Variable, y = Beta, ymin = IC_2.5, ymax = IC_97.5)) +
    geom_pointrange() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    coord_flip() +
    geom_text(aes(label = Signif), hjust = -0.2, size = 5) +
    scale_y_continuous(expand = expansion(mult = c(0.1, 0.2))) +
    labs(
      title = paste("β e IC 95% – Regresión Lineal (", time_var, ")", sep = ""),
      x     = NULL,
      y     = expression(beta~"(IC 95%)")
    ) +
    theme_bw() +
    theme(
      panel.grid.major = element_line(color = "grey80"),
      panel.grid.minor = element_blank(),
      plot.background   = element_rect(fill = "white")
    )
  
  # 7) Guardar figura
  ggsave(
    filename = paste0("outputs/Fig4_Lineal_", time_var, ".png"),
    plot     = plt_lin,
    width    = 6,
    height   = 4
  )
}
