library(tidyverse)

dvariables <- readr::read_delim(
  "https://raw.githubusercontent.com/frzambra/agrometChile/62c23e60021e956e23945f41a79f32583fc3cef1/man/get_data.Rd?token=GHSAT0AAAAAABMWW3A6OL66ED7IFTCSMGF4YRZ5IZA",
  skip = 14,
  n_max = 11,
  delim = "\\tab"
  )

dvariables

dvariables <- dvariables  %>%

  mutate(across(everything(), str_trim)) %>%
  rename_with(str_trim) %>%

  mutate(across(everything(), str_remove, "\\s+\\\\cr")) %>%
  rename_with( ~ str_remove(.x, "\\s+\\\\cr"))

dvariables

saveRDS(dvariables, "data/definicion_variables.rds")
