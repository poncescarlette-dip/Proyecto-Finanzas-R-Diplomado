############################################################
# PROYECTO: Conciliación Inteligente en R
# Diplomado Ciencia de Datos para las Finanzas - U. de Chile
# Parte 2: MVP, Limpieza, Transformación y Primer Modelo
############################################################

# ==========================================================
# 0. Librerías
# ==========================================================

library(tidyverse)
library(lubridate)
library(data.table)
library(readxl)
library(tidymodels)
library(vetiver)
library(pins)

# ==========================================================
# 1. Importación de datos
# ==========================================================

cartola <- read_excel("Cartola.xlsx", skip = 6)
contab  <- read_excel("Contabilidad.xlsx")

# ==========================================================
# 2. Normalización de columnas
# ==========================================================

names(cartola) <- toupper(trimws(names(cartola)))
names(contab)  <- toupper(trimws(names(contab)))

# ==========================================================
# 3. Limpieza y transformación
# ==========================================================

## 3.1 Cartola bancaria
cartola <- cartola %>%
  mutate(
    DOCUMENTO = as.character(DOCUMENTO),
    CARGOS    = suppressWarnings(as.numeric(CARGOS)),
    ABONOS    = suppressWarnings(as.numeric(ABONOS)),
    CARGOS    = replace_na(CARGOS, 0),
    ABONOS    = replace_na(ABONOS, 0),
    MONTO_NETO = ABONOS - CARGOS
  )

if ("FECHA" %in% names(cartola)) {
  cartola <- cartola %>% mutate(FECHA = suppressWarnings(as_date(FECHA)))
}

## 3.2 Contabilidad
contab <- contab %>%
  mutate(
    FOLIO     = as.character(FOLIO),
    CARGOS    = suppressWarnings(as.numeric(CARGOS)),
    ABONOS    = suppressWarnings(as.numeric(ABONOS)),
    CARGOS    = replace_na(CARGOS, 0),
    ABONOS    = replace_na(ABONOS, 0),
    MONTO_NETO = CARGOS - ABONOS
  )

if ("FECHA CONTABLE" %in% names(contab)) {
  contab <- contab %>% mutate(FECHA_CONTABLE = suppressWarnings(as_date(`FECHA CONTABLE`)))
}

# ==========================================================
# 4. Reglas automáticas de conciliación
# ==========================================================

conciliados <- cartola %>%
  inner_join(
    contab,
    by = c("DOCUMENTO" = "FOLIO", "MONTO_NETO" = "MONTO_NETO"),
    suffix = c("_CARTOLA", "_CONTAB")
  )

# ==========================================================
# 5. Partidas no conciliadas (sin warnings)
# ==========================================================

## CARTOLA
if ("DOCUMENTO" %in% names(conciliados)) {
  cartola_nc <- cartola %>% 
    filter(!(DOCUMENTO %in% conciliados$DOCUMENTO))
} else {
  cartola_nc <- cartola
}

## CONTAB
if ("FOLIO" %in% names(conciliados)) {
  contab_nc <- contab %>% 
    filter(!(FOLIO %in% conciliados$FOLIO))
} else {
  contab_nc <- contab
}

# ==========================================================
# 6. Dataset para modelamiento
# ==========================================================

data_model <- bind_rows(
  conciliados %>% mutate(ESTADO = "CONCILIADA"),
  cartola_nc %>% mutate(ESTADO = "NO_CONCILIADA")
)

data_model <- data_model %>%
  mutate(
    DIA = if ("FECHA" %in% names(.)) day(FECHA) else NA_integer_,
    TIPO = case_when(
      ABONOS > 0 ~ "ABONO",
      CARGOS > 0 ~ "CARGO",
      TRUE       ~ "OTRO"
    ),
    ESTADO = factor(ESTADO)
  ) %>%
  filter(!is.na(ESTADO))

# ==========================================================
# 7. División de datos
# ==========================================================

set.seed(123)
split <- initial_split(data_model, prop = 0.8)
train <- training(split)
test  <- testing(split)

# ==========================================================
# 8. Modelo de clasificación
# ==========================================================

modelo_arbol <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("classification")

workflow_arbol <- workflow() %>%
  add_model(modelo_arbol) %>%
  add_formula(ESTADO ~ MONTO_NETO + DIA + TIPO)

fit_arbol <- fit(workflow_arbol, train)

# ==========================================================
# 9. Métricas del modelo
# ==========================================================

pred <- predict(fit_arbol, test) %>% bind_cols(test)
metrics <- yardstick::metrics(pred, truth = ESTADO, estimate = .pred_class)
print(metrics)

# ==========================================================
# 10. Versionado del modelo (compatible con tu vetiver)
# ==========================================================

v <- vetiver_model(fit_arbol, "modelo_conciliacion_bancaria")

dir.create("pins", showWarnings = FALSE)

vetiver_pin_write(board = board_folder("pins"), v)

# ==========================================================
# ==========================================================
# 11. Visualización amigable de resultados
# ==========================================================

View(conciliados)
View(cartola_nc)
View(contab_nc)
############################################################
# FIN DEL SCRIPT
############################################################

