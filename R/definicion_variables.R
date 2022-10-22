library(tidyverse)

ddefvars <-
  structure(
    list(
      Name = c("T", "HR", "RS", "P", "VV", "VVmx", "DV", "PA", "TS", "TS10", "ET"),
      Description = c(
        "Temperatura del Aire Media",
        "Humedad Relativa Media",
        "Radiación Media",
        "Precipitación",
        "Velocidad Viento Media",
        "Velocidad Viento Máxima",
        "Dirección del Viento",
        "Presión Atmosférica",
        "Temperatura Superficie Media",
        "Temperatura Suelo 10cm Media",
        "Evapotranspiración"
      ),
      Units = c(
        "Grados Celcius",
        "Porcentaje",
        "Watts por metro cuadrado",
        "Milímetros",
        "Metros por Segundo",
        "Metros por Segundo",
        "Grados",
        "milibares",
        "Grados Celcius",
        "Grados Celcius",
        "Milí­metros"
      ),
      Symbol = c(
        "°C",
        "%",
        "W/m2",
        "mm",
        "m/seg",
        "m/seg",
        "°",
        "mbar",
        "°C",
        "°C",
        "mm"
      )
    ),
    row.names = c(NA,-11L),
    class = c("tbl_df", "tbl", "data.frame")
  )

ddefvars |>
  filter(row_number() %in% c(1, 2, 3, 4, 5, 8, 11)) |>
  dput()

ddefvars <-
  structure(
    list(
      Name = c("T", "HR", "RS", "P", "VV", "PA", "ET"),
      Description = c(
        "Temperatura del Aire Media",
        "Humedad Relativa Media",
        "Radiación Media",
        "Precipitación",
        "Velocidad Viento Media",
        "Presión Atmosférica",
        "Evapotranspiración"
      ),
      Units = c(
        "Grados Celcius",
        "Porcentaje",
        "Watts por metro cuadrado",
        "Milímetros",
        "Metros por Segundo",
        "milibares",
        "Milímetros"
      ),
      Symbol = c("°C", "%", "W/m2",
                 "mm", "m/seg", "mbar", "mm")
    ),
    row.names = c(NA,-7L),
    class = c("tbl_df",
              "tbl", "data.frame")
  )


# colors ------------------------------------------------------------------
color_temp <- c("#2f93c4", "#9abf9d", "#fafa66", "#fa8932", "#ea1a15")
color_temp <- highcharter::colorize(1:(6*6), color_temp)
scales::show_col(color_temp)

color_prec <- c("#d3d3fc", "#959de6", "#5d71cc", "#264fad", "#013a94")
color_prec <- highcharter::colorize(1:(6*6), color_prec)
scales::show_col(color_prec)

color_pres <- c("#0a00fc", "#aa00ce", "#f90096", "#ff0062", "#ff2934")
color_pres <- highcharter::colorize(1:(6*6), color_pres)
scales::show_col(color_pres)

color_velv <- c("#f3fc72", "#4ee75e", "#33d5d1", "#4c12b9", "#a80290")
color_velv <- highcharter::colorize(1:(6*6), color_velv)
scales::show_col(color_velv)

ddefvars <- ddefvars |>
  mutate(
    cols = list(
      color_temp,
      "viridis",
      "viridis",
      color_prec,
      color_velv,
      color_pres,
      "viridis"
      )
    )


# export ------------------------------------------------------------------
saveRDS(ddefvars, "data/definicion_variables.rds")



