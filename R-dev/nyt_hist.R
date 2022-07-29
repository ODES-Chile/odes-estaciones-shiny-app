# setup -------------------------------------------------------------------
library(tidyverse)
library(lubridate)

# parametros --------------------------------------------------------------
SPAN <- 0.5

pth <- "G:/.shortcut-targets-by-id/1LvK5d819CaVAoTqNO2kBF3ZkZCk71Wkm/proyecto_sequia/data/tabular/data_processed/RAN/"

# data temperatura --------------------------------------------------------
dt <- readRDS(file.path(pth, "data_temp_con_normal_estaciones_dias_completos_2022.rds"))

dt <- ungroup(dt)

glimpse(dt)

dt |>
  filter(station_id == 1) |>
  count(day)

# agrupacion --------------------------------------------------------------
dt_hist <- dt |>
  # filtrar por periodo climatologico
  # 30 anios
  group_by(
    nombre_ema,
    station_id,
    m = month(day),
    d = day(day)
    ) |>
  summarise(

    # tmax_hist = quantile(tmax, 1,  na.rm = TRUE),
    # tmin_hist = quantile(tmin, 0, na.rm = TRUE),

    tmax_hist = quantile(tmax, .95,  na.rm = TRUE),
    tmin_hist = quantile(tmin, .05, na.rm = TRUE),

    tmax_norm = quantile(tmax, .5,na.rm = TRUE),
    tmin_norm = quantile(tmin, .5, na.rm = TRUE),

    # tmax_norm = quantile(tasmax, .5,na.rm = TRUE),
    # tmin_norm = quantile(tasmin, .5, na.rm = TRUE),


    .groups  = "drop"

  ) |>
  mutate(fecha = ymd(paste(year(Sys.Date()), m, d, sep = "-")))

# predict(loess(dt_hist$tmax_hist ~ dt_hist$fecha))
dt_hist <- dt_hist |>
  filter(!is.na(fecha))

dt_hist |>
  filter(is.na(fecha))

dt_hist |>
  filter(is.na(tmax_hist))

dt_hist <- dt_hist |>
  filter(!is.na(tmax_hist))

dt_hist |>
  filter(is.na(tmax_hist))

dt_hist |>
  filter(is.infinite(tmax_hist))

predict(
  loess(
    tmax_hist ~ as.numeric(fecha),
    span = SPAN,
    data =  dt_hist |> filter(nombre_ema == "Andacollo")
    )
  )

# suavizamiento -----------------------------------------------------------
dt_hist <- dt_hist |>
  group_by(nombre_ema, station_id) |>
  mutate(
    tmax_hist_smooth  =  predict(loess(tmax_hist ~ as.numeric(fecha), span = SPAN)),
    tmin_hist_smooth  =  predict(loess(tmin_hist ~ as.numeric(fecha), span = SPAN)),

    tmax_norm_smooth  =  predict(loess(tmax_norm ~ as.numeric(fecha), span = SPAN)),
    tmin_norm_smooth  =  predict(loess(tmin_norm ~ as.numeric(fecha), span = SPAN)),

  )

glimpse(dt_hist)

dt_hist |>
  distinct(station_id)

dt_hist |>
  # filter(station_id %in% c(1, 676, 353, 98)) |>
  ggplot() +
  geom_ribbon(aes(fecha, ymin = tmin_hist, ymax = tmax_hist), fill = "gray80") +
  geom_ribbon(aes(fecha, ymin = tmin_norm, ymax = tmax_norm), fill = "gray50") +


  geom_smooth(aes(fecha, tmin_hist), span = SPAN, color = "red", se = FALSE) +
  geom_line(aes(fecha, tmin_hist_smooth), color = "blue") +


  geom_smooth(aes(fecha, tmax_hist), span = 0.5) +

  geom_smooth(aes(fecha, tmax_norm), span = 0.5) +
  geom_smooth(aes(fecha, tmin_norm), span = 0.5) +

  facet_wrap(vars(nombre_ema), scales = "free_y")

# test --------------------------------------------------------------------
dt1_v2 <- dt |>
  ungroup() |>
  select(-region, nombre_ema) |>
  filter(station_id == 1, year(day) < 2015)

dt1_v2

glimpse(dt1_v2)

ggplot(dt1_v2) + geom_line(aes(day, tmean))
ggplot(dt1_v2) + geom_line(aes(day, tmin))
ggplot(dt1_v2) + geom_line(aes(day, tmax))

ggplot(dt1_v2) +
  geom_smooth(aes(day, tmean), color = "yellow") +
  geom_smooth(aes(day, tmin), color = "blue") +
  geom_smooth(aes(day, tmax), color = "red")


ggplot(dt1_v2) + geom_line(aes(day, tas))
ggplot(dt1_v2) + geom_line(aes(day, tasmax))
ggplot(dt1_v2) + geom_line(aes(day, tasmin))

ggplot(dt1_v2) +
  geom_smooth(aes(day, tas), color = "yellow") +
  geom_smooth(aes(day, tasmax), color = "blue") +
  geom_smooth(aes(day, tasmin), color = "red")


# export ------------------------------------------------------------------
dt_hist <- dt_hist |>
  ungroup() |>
  select(-fecha, -nombre_ema)

dt_hist |>
  count(station_id) |>
  count(n)

saveRDS(dt_hist,  "data/nyt_temp_historicos.rds")

rm(dt, dt_hist, dt1_v2)

# data precipitacion ------------------------------------------------------
dp <- readRDS(file.path(pth, "data_precipitacion_filtrada_dias_completos_2022.rds"))

dp <- ungroup(dp)

dp_calera <-  dp |>
  filter(str_detect(nombre_ema, "Calera"))

dp_calera |>
  count(month)

dp_calera |>
  count(day)

dp_calera <- dp_calera |>
  arrange(day) |>
  mutate(day = as_date(day)) |>
  filter(year(day) == min(year(day) + 5)) |>
  mutate(prec_day_cum_tot = cumsum(prec_day_cum))

dp_calera |>
  distinct()

p1 <- ggplot(dp_calera) +
  geom_area(aes(day, prec_day_cum, group = month(day)), fill = "blue", alpha = 0.5) +
  geom_line(aes(day, prec_norm, group = month(day)), color = "darkred", size = 2) +
  scale_x_date(breaks = "month", minor_breaks = NULL)

p1

p2 <- ggplot(dp_calera) +
  geom_area(aes(day, prec_day_cum_tot), fill = "blue", alpha = 0.5) +
  scale_x_date(breaks = "month", minor_breaks = NULL)

p2


# fix
dp |> count(year(day))

dp_hist <- dp |>
  filter(year(day) == max(year(day)) - 1)

dp_hist <- dp_hist |>
  distinct(day, station_id, prec_norm)

dp_hist |>
  count(station_id) |>
  count(n)

dp_hist <- dp_hist |>
  complete(day, station_id) |>
  arrange(station_id, day) |>
  mutate(m = month(day), d = day(day)) |>
  group_by(station_id, m) |>
  fill(prec_norm, .direction =  "downup") |>
  ungroup()

dp_hist |>
  count(station_id) |>
  count(n)


# export ------------------------------------------------------------------
dp_hist <- dp_hist |>
  select(-day)

saveRDS(dp_hist,  "data/nyt_prec_historicos.rds")
