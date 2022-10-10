
# setup -------------------------------------------------------------------

source(here::here("code/library.R"))
source(here::here("code/set_functions.R"))

key_ussw <- paste(c("n30w120",
                    "n30w125",
                    "n35w120",
                    "n35w125",
                    "n40w115",
                    "n40w120",
                    "n40w125",
                    "n45w110",
                    "n45w115",
                    "n45w120",
                    "n45w125",
                    "n45w130",
                    "n50w120",
                    "n50w125"), collapse = "|")

key_usnw <- paste(c("n50w130",
                    "n50w135",
                    "n55w130",
                    "n55w135",
                    "n60w130",
                    "n60w135"), collapse = "|")

key_use <- paste(c("n30w080",
                   "n30w085",
                   "n35w080",
                   "n35w085"), collapse = "|")

key_eu <- paste(c("n40e000",
                  "n40e005",
                  "n40e010",
                  "n40e015",
                  "n45e000",
                  "n45e005",
                  "n45e010",
                  "n45e015",
                  "n50e005",
                  "n50e010"), collapse = "|")

list_key <- list(ussw = key_ussw,
                 usnw = key_usnw,
                 use = key_use,
                 eu = key_eu)

# outlet ------------------------------------------------------------------

## data from Carraro & Altermatt
## coordinates for Stikine is slightly modified for watershed delineation
## original: 56.50995498, -132.412977
## new: 56.693446, -132.206884

df0 <- read_csv(here::here("data_raw/TableS1_draft.csv")) %>% 
  rename(id = ...1) %>% 
  select(name, A_km2, lat, long) %>% 
  sf::st_as_sf(coords = c("long", "lat"),
               crs = st_crs(4326)) %>%
  saveRDS(paste0("data_fmt/outlet.rds"))


# raster ------------------------------------------------------------------

## Perform the following only if re-merge raters

foreach(i = 1:length(list_key)) %do% {
  
  ## watershed area
  list.files(here::here("data_raw/upa"),
             full.names = T,
             pattern = list_key[[i]]) %>%
    lapply(rast) %>%
    terra::sprc() %>% ## spatRasterCollection
    merge() %>% 
    terra::writeRaster(here::here(paste0("data_fmt/epsg4326_upa_",
                                         names(list_key)[i],
                                         ".tif")),
                       overwrite = TRUE)
  
  ## flow direction
  list.files(here::here("data_raw/dir"),
             full.names = T,
             pattern = list_key[[i]]) %>%
    lapply(rast) %>%
    lapply(arc2d8) %>% 
    terra::sprc() %>% ## spatRasterCollection
    merge() %>% 
    terra::writeRaster(here::here(paste0("data_fmt/epsg4326_dir_",
                                         names(list_key)[i],
                                         ".tif")),
                       overwrite = TRUE)
}

gc(reset = TRUE)

## Download from Google drive
# 
# fname <- c("epsg4326_upa.tif", "epsg4326_dir_d8.tif")
# 
# foreach(i = seq_len(length(fname))) %do% {
#   googledrive::drive_download(
#     fname[i],
#     path = paste0("data_fmt/", fname[i]),
#     overwrite = TRUE
#   )
# }
