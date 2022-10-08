
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/library.R"))
source(here::here("code/set_functions.R"))

regex_key <- paste(c("ussw", "usnw", "use", "eu"), collapse = "|")

# merge -------------------------------------------------------------------

df_point <- list.files("data_fmt",
                       full.names = T,
                       pattern = "outlet.rds") %>% 
  lapply(function(x) {
    readRDS(x) %>% 
      mutate(wsd = row_number())
  }) %>% 
  bind_rows() %>% 
  as_tibble()

sf_polygon <- list.files("data_fmt",
                         full.names = T,
                         pattern = "wsd_") %>% 
  lapply(function(x) {
    print(x)
    print(str_extract(x, regex_key))
    
    readRDS(x) %>%
      mutate(area = units::set_units(area, "km^2"),
             region = str_extract(x, regex_key)) %>%
      left_join(df_point %>% dplyr::select(-geometry),
                by = c("wsd"))
  }) %>% 
  bind_rows() %>% 
  rmapshaper::ms_simplify()


# join --------------------------------------------------------------------

## join & merge
sf_line <- list.files("data_fmt",
                      full.names = T,
                      pattern = "str_") %>% 
  lapply(function(x) {
    readRDS(x) %>% 
      mutate(a_t = str_extract(x, "a\\d{1,}") %>%
               str_extract("\\d{1,}") %>%
               as.numeric()) %>%
      st_join(sf_polygon) %>% 
      drop_na(wsd) %>% 
      filter(!duplicated(FID))
  }) %>% 
  bind_rows()

sf_line <- sf_line %>% 
  st_intersection(sf_polygon) %>% 
  mutate(length = st_length(.) %>%
           units::set_units("km") %>% 
           round(1))

## export
### complete stream network
saveRDS(sf_line,
        "data_fmt/epsg4326_strnet.rds")

### complete watershed polygons
saveRDS(sf_polygon,
        "data_fmt/epsg4326_wsd.rds")


# check -------------------------------------------------------------------

tmap_mode("view")

tm_shape(sf_polygon) +
  tm_polygons(alpha = 0.3) +
  tm_shape(sf_line %>% 
             filter(a_t == 1000)) +
  tm_lines(alpha = 0.3)

