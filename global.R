# packages ----------------------------------------------------------------
# SHINY
library(shiny)
library(leaflet)
library(leaflet.providers)
library(highcharter) # remotes::install_github("jbkunst/highcharter")
library(shinyWidgets)
library(bslib)
library(classInt)

# DATA
library(tidyverse)
library(lubridate)
library(RPostgres)
library(pool)

# OTHERS
library(cli)

cli::cli_h1("Start global.R")

pool <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = "shiny",
  host = Sys.getenv("HOST"),
  user = "shiny",
  password = Sys.getenv("SHINY_PSQL_PWD")
)

onStop(function() {
  poolClose(pool)
})


source("R/helpers.R")

# options -----------------------------------------------------------------
parametros <- list(
  color = "#236478",
  font_family = "Raleway",
  font_family_code = "Source Code Pro",
  tabla_datos = "estaciones_datos",
  tabla_estaciones = "estaciones"
)

theme_odes <-  bs_theme(
  version = 5,
  primary = parametros$color,
  base_font = font_google(parametros$font_family),
  code_font = font_google(parametros$font_family_code)
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
    nombre_estacion_largo        = str_glue("{region} - {nombre_estacion} ({latitud}, {longitud}), red {red}"),
    nombre_estacion_no_tan_largo = str_glue("{nombre_estacion} ({latitud}, {longitud}), red {red}")
  )

fechas_min_max <- tbl(sql_con(), parametros$tabla_datos) |>
  summarise(min(fecha_hora), max(fecha_hora)) |>
  collect() |>
  gather() |>
  pull(value) |>
  lubridate::ceiling_date(unit = "month")


# data        |> count(red)
# destaciones |> count(red)


# inputs main -------------------------------------------------------------
opt_variable <- c(
  "Temperatura promedio aire" = "temp_promedio_aire",
  "Temperatura mínima"        = "temp_minima",
  "Temperatura máxima"        = "temp_maxima",
  "Precipitación"             = "precipitacion_horaria",
  "Humedad relativa promedio" = "humed_rel_promedio",
  "Presión atomsférica"       = "presion_atmosferica",
  "Radiación solar máxima"    = "radiacion_solar_max",
  "Velocidad máxima del viento" = "veloc_max_viento"
  )

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
  select(nombre_estacion_no_tan_largo, station_id) |>
  deframe()

# inputs salón ------------------------------------------------------------
las_63 <- unique(pull(readRDS("data/nyt_temp_historicos.rds"), station_id))

opt_estaciones_nyt <- destaciones |>
  filter(station_id %in% las_63) |>
  select(nombre_estacion_largo, station_id) |>
  deframe()

fechas_min_max_63 <- tbl(sql_con(), parametros$tabla_datos) |>
  filter(station_id %in% las_63) |>
  summarise(min(fecha_hora), max(fecha_hora)) |>
  collect() |>
  gather() |>
  pull(value) |>
  lubridate::ceiling_date(unit = "month")

opt_salon_yrs <- seq(min(year(fechas_min_max_63)), max(year(fechas_min_max_63)))

# inputs descarga datos ---------------------------------------------------
opt_estaciones_datos <- destaciones |>
  select(nombre_estacion_largo, station_id) |>
  deframe()

opt_estaciones_meses <- seq.Date(fechas_min_max[1], fechas_min_max[2], by = "month") |>
  format("%Y/%m")

opt_estaciones_meses


# input opciones ----------------------------------------------------------
opt_opts_leafletproviders <- c("CartoDB.Positron", "Esri.WorldImagery", "Esri.WorldTopoMap")

opt_opts_yrsdata <- seq(year(fechas_min_max[1]), year(fechas_min_max[2]))

# end ---------------------------------------------------------------------
cli::cli_h1("End global.R")
