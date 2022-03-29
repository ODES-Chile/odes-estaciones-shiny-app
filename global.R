# packages ----------------------------------------------------------------
library(shiny)
library(tidyverse)
library(bslib)
library(leaflet)
library(leaflet.providers)
library(highcharter)
# remotes::install_github("jbkunst/highcharter")

# options -----------------------------------------------------------------
parametros <- list(
  color = "#236478",
  font_family = "IBM Plex Sans"
)

theme_obbsa <-  bs_theme(
  # bg = "white",
  # fg = "#236478",
  # primary = "black",
  # bootswatch = "yeti",
  base_font = font_google(parametros$font_family)
)

langs <- getOption("highcharter.lang")

# langs$loading <- "<i class='fas fa-circle-notch fa-spin fa-4x'></i>"
langs$loading <- "Cargando información"

options()

options(
  highcharter.lang = langs,
  highcharter.theme = hc_theme_smpl(
    color = parametros$color,
    chart = list(style = list(fontFamily = parametros$font_family))
    # colors = parametros$color
  )
)

# theme <- theme_obbsa |>
#   bs_add_rules("")


# bslib::bs_theme_preview(theme_obbsa)

# data --------------------------------------------------------------------
# dtiempo     <- readRDS("data/dummy/dtiempo.rds")
dtiempo     <- readRDS("data/data_inia_chile.rds")
dtiempo     <- mutate(dtiempo, tiempo = lubridate::ymd_hm(tiempo))
dtiempo     <- rename(dtiempo, identificador = cod)

destaciones <- readRDS("data/dummy/estaciones.rds")

ddefvars    <- readRDS("data/definicion_variables.rds")

# inputs options ----------------------------------------------------------
opt_variable <- dtiempo |>
  count(var) |>
  pull(var)

opt_variable <- dplyr::filter(ddefvars, Name %in% opt_variable) |>
  select(Description, Name) |>
  deframe()

opt_variable


fun_group <- list(
  "Horaria" = partial(lubridate::ceiling_date, unit = "hour"),
  "Diaria"  = partial(lubridate::ceiling_date, unit = "day"),
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
  select(nombre, identificador) |>
  deframe()

