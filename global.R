# packages ----------------------------------------------------------------
library(tidyverse)
library(bslib)
library(leaflet)
library(highcharter)
# remotes::install_github("jbkunst/highcharter")

# options -----------------------------------------------------------------
parametros <- list(
  color = "#236478"
)

theme_obbsa <-  bs_theme(
  # bg = "white",
  # fg = "#236478",
  # primary = "black",
  # bootswatch = "yeti",
  base_font = font_google("IBM Plex Sans")
)

options(
  highcharter.theme = hc_theme_smpl(
    # colors = parametros$color
  )
)

# theme <- bs_add_rules(
#   theme_obbsa,
#   ".bg-dark, .navbar.navbar-inverse { background-color: #236478 !important;}"
#   )

# bslib::bs_theme_preview(theme_obbsa)

# data --------------------------------------------------------------------
dtiempo     <- readRDS("data/dummy/dtiempo.rds")

destaciones <- readRDS("data/dummy/estaciones.rds")

ddefvars    <- readRDS("data/definicion_variables.rds")

# inputs options ----------------------------------------------------------
opt_variable <- dtiempo %>%
  count(var) %>%
  pull(var)

opt_variable <- dplyr::filter(ddefvars, Name %in% opt_variable) %>%
  select(Description, Name) %>%
  deframe()

opt_variable

opt_valores <- list(
  "Ultimo registro",
  "Promedio últimos 5 días"
)

opt_estaciones <- destaciones %>%
  select(nombre, identificador) %>%
  deframe()
