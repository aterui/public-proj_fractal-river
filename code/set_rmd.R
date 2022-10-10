
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/library.R"))
source(here::here("code/set_functions.R"))


# data --------------------------------------------------------------------

knitr::opts_chunk$set(echo = FALSE)

list.files(here::here("code"),
           pattern = "analysis",
           full.names = TRUE) %>%
  lapply(function(x) source(here::here(x)))

df_m <- read_csv(here::here("data_raw/TableS1_draft.csv"))
