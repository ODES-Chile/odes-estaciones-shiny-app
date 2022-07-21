# packages ----------------------------------------------------------------
# SHINY
library(shiny)
library(leaflet)
library(leaflet.providers)
library(highcharter) # remotes::install_github("jbkunst/highcharter")
library(shinyWidgets)
library(bslib)

# DATA
library(tidyverse)
library(lubridate)
library(RPostgres)
library(pool)

# OTHER
library(cli)

cli::cli_h1("Start global.R")

source("R/helpers.R")

# options -----------------------------------------------------------------
parametros <- list(
  color = "#236478",
  font_family = "IBM Plex Sans",
  tabla_datos = "estaciones_datos",
  tabla_estaciones = "estaciones"
)

theme_odes <-  bs_theme(
  # version = version_default(),
  version = 5,
  # bg = "white",
  # fg = "#236478",
  # primary = "black",
  # bootswatch = "yeti",
  base_font = font_google(parametros$font_family)
)

langs <- getOption("highcharter.lang")

# langs$loading <- "<i class='fas fa-circle-notch fa-spin fa-4x'></i>"
langs$loading <- "Cargando información"

options(
  highcharter.lang = langs,
  highcharter.theme = hc_theme_smpl(
    color = parametros$color,
    chart = list(style = list(fontFamily = parametros$font_family))
    # colors = parametros$color
  )
)

# data --------------------------------------------------------------------
# dtiempo     <- readRDS("data/dummy/dtiempo.rds")
# dtiempo     <- readRDS("data/data_inia_chile.rds")
# dtiempo     <- mutate(dtiempo, tiempo = lubridate::ymd_hm(tiempo))
# dtiempo     <- rename(dtiempo, identificador = cod)
# destaciones <- readRDS("data/dummy/estaciones.rds")

# data        <- readRDS("data/data_diaria.rds")
# data        <- readRDS("data/data_diaria_202X.rds")
# data        <- readRDS("data/data_diaria_2022.rds")
ddefvars    <- readRDS("data/definicion_variables.rds")

# destaciones <- readRDS("data/estaciones.rds")
destaciones <- tbl(sql_con(), parametros$tabla_estaciones) |>
  collect()

destaciones <- destaciones |>
  arrange(desc(latitud)) |>
  mutate(
    latitud  = round(latitud, 3),
    longitud = round(longitud, 3),
    nombre_estacion_largo = str_glue(" {region} - {nombre_estacion} - ({latitud}, {longitud}) ")
  )

# data        |> count(red)
# destaciones |> count(red)


# inputs main -------------------------------------------------------------
opt_variable <- c(
  "temp_promedio_aire",
  "temp_minima",
  "temp_maxima",
  "precipitacion_horaria",
  "humed_rel_promedio",
  "presion_atmosferica",
  "radiacion_solar_max",
  "veloc_max_viento"
  ) |>
  as_tibble() |>
  mutate(desc = snakecase::to_sentence_case(value)) |>
  select(desc, value) |>
  deframe()

opt_variable

fun_group <- list(
  # "Horaria" = partial(lubridate::ceiling_date, unit = "hour"),
  # "Diaria"  = partial(lubridate::ceiling_date, unit = "day"),
  "Diaria"  = identity,
  "Semanal" = partial(lubridate::ceiling_date, unit = "week"),
  "Mensual" = partial(lubridate::ceiling_date, unit = "month"),
  "Sin agrupar" = identity
  )

opt_group <- names(fun_group)

opt_group

fun_stat <- list(
  "Promedio"  = partial(mean, na.rm = TRUE),
  "Mínimo"    = partial(min, na.rm = TRUE),
  "Máximo"    = partial(max, na.rm = TRUE),
  "Acumulado (suma)" = partial(sum, na.rm = TRUE)
)

opt_stat <- names(fun_stat)

opt_stat

opt_estaciones <- destaciones |>
  select(nombre_estacion, station_id) |>
  deframe()

# inputs salón ------------------------------------------------------------
opt_estaciones_nyt <- destaciones |>
  filter(station_id %in% unique(pull(readRDS("data/nyt_temp_historicos.rds"), station_id))) |>
  select(nombre_estacion_largo, station_id) |>
  deframe()

# inputs descarga datos ---------------------------------------------------
opt_estaciones_datos <- destaciones |>
  select(nombre_estacion_largo, station_id) |>
  deframe()

fechas_min_max <- tbl(sql_con(), parametros$tabla_datos) |>
  summarise(min(fecha_hora), max(fecha_hora)) |>
  collect() |>
  gather() |>
  pull(value) |>
  lubridate::ceiling_date(unit = "month")

opt_estaciones_meses <- seq.Date(fechas_min_max[1], fechas_min_max[2], by = "month") |>
  format("%Y/%m")

opt_estaciones_meses

cli::cli_h1("End global.R")
