
# setup -------------------------------------------------------------------

rm(list = ls())
list.files(here::here("code"),
           pattern = "analysis",
           full.names = TRUE) %>% 
  lapply(function(x) source(here::here(x)))

