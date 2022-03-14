library(dplyr)
library(bslib)
library(leaflet)
library(highcharter)

theme_obbsa <-  bs_theme(
  # bg = "#1A4645",
  # fg = "#FDFDFD",
  # primary = "#22B455",
  # bootswatch = "yeti",
  base_font = font_google("IBM Plex Sans"),

)

dtiempo     <- readRDS("data/dummy/dtiempo.rds")

destaciones <- readRDS("data/dummy/estaciones.rds")

vars <- dtiempo %>%
    count(var) %>%
    pull(var)

values <- list(
  "Ultimo registro",
  "Promedio últimos 5 días"
)
