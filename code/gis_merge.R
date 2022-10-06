
# setup -------------------------------------------------------------------

source(here::here("code/library.R"))
source(here::here("code/set_functions.R"))


# outlet ------------------------------------------------------------------

## data from Carraro & Altermatt
df0 <- read_csv(here::here("data_raw/TableS1_draft.csv")) %>% 
  rename(id = ...1) %>% 
  filter(between(A_km2, 5000, 10000)) %>% 
  select(name, A_km2, lat, long)

list_outlet <- list(df0 %>% filter(long < 0) %>% mutate(region = "w"),
                    df0 %>% filter(long > 0) %>% mutate(region = "e"))


lapply(list_outlet, function(x) {
  print(paste0("data_raw/outlet_", unique(x$region), ".rds"))  
  
  x %>% 
    sf::st_as_sf(coords = c("long", "lat"),
               crs = st_crs(4326)) %>%
    saveRDS(paste0("data_raw/outlet_", unique(x$region), ".rds"))
  
  x %>% 
    sf::st_as_sf(coords = c("long", "lat"),
                 crs = st_crs(4326)) %>%
    st_write(paste0("data_raw/outlet_", unique(x$region), ".gpkg"),
             append = FALSE)
  
})

# raster ------------------------------------------------------------------

## west rasters
list.files(here::here("data_raw"),
           full.names = T,
           pattern = "w\\d{1,3}_upa") %>%
  lapply(rast) %>%
  reduce(terra::merge) %>%
  terra::writeRaster(here::here("data_fmt/epsg4326_upa_w.tif"),
                     overwrite = TRUE)

list.files(here::here("data_raw"),
           full.names = T,
           pattern = "w\\d{1,3}_dir") %>% 
  lapply(rast) %>% 
  reduce(terra::merge) %>% 
  arc2d8() %>% 
  terra::writeRaster(here::here("data_fmt/epsg4326_dir_d8_w.tif"),
                     overwrite = TRUE)


## east rasters
list.files(here::here("data_raw"),
           full.names = T,
           pattern = "e\\d{1,3}_upa") %>%
  lapply(rast) %>%
  reduce(terra::merge) %>%
  terra::writeRaster(here::here("data_fmt/epsg4326_upa_e.tif"),
                     overwrite = TRUE)

list.files(here::here("data_raw"),
           full.names = T,
           pattern = "e\\d{1,3}_dir") %>% 
  lapply(rast) %>% 
  reduce(terra::merge) %>% 
  arc2d8() %>% 
  terra::writeRaster(here::here("data_fmt/epsg4326_dir_d8_e.tif"),
                     overwrite = TRUE)
