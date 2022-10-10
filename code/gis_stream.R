
# setup -------------------------------------------------------------------

source(here::here("code/library.R"))
source(here::here("code/set_functions.R"))

regex_key <- paste(paste(rep(c("dir", "upa")),
                         rep(c("ussw", "usnw", "use", "eu"), each = 2),
                         sep = "_"), collapse = "|")


# data --------------------------------------------------------------------

## temporary file names
fname <- c(paste0(tempdir(), "\\dir_", c("ussw", "usnw", "use", "eu"), ".tif"),
           paste0(tempdir(), "\\upa_", c("ussw", "usnw", "use", "eu"), ".tif"),
           paste0(tempdir(), "\\stream_", c("ussw", "usnw", "use", "eu"), ".tif"),
           paste0(tempdir(), "\\stream_", c("ussw", "usnw", "use", "eu"), ".shp"))

## call raster
list_rast <- list.files(here::here("data_fmt"),
                        full.names = T,
                        pattern = "(upa)|(dir)") %>% 
  lapply(rast)

## save to temporary folder
lapply(seq_len(length(list_rast)),
       FUN = function(i) {
         
         key <- str_extract(terra::sources(list_rast[[i]]),
                            pattern = regex_key)
         
         terra::writeRaster(list_rast[[i]],
                            filename = fname[str_detect(fname, key)],
                            overwrite = TRUE)
       })


# whitebox ----------------------------------------------------------------

a <- rev(round(10^seq(log(1, 10),
                      log(1000, 10),
                      length = 20),
               1))

## extract streams with different A_t values
df_x <- expand.grid(a_t = a,
                    region = c("ussw", "usnw", "use", "eu")) %>% 
  mutate(region = as.character(region),
         label = str_pad(a_t * 10,
                         width = 5,
                         pad = "0"))

foreach(x = iterators::iter(df_x, by = "row")) %do% {
  ## select/define file names
  sf_line <- paste0("data_fmt/epsg4326_",
                    "str_a",
                    x$label,
                    x$region,
                    ".rds")
  print(sf_line)
  
  ## stream delineation
  whitebox::wbt_extract_streams(flow_accum = fname[str_detect(fname, paste0("upa_", x$region, ".tif"))],
                                output = fname[str_detect(fname, paste0("stream_", x$region, ".tif"))],
                                threshold = x$a_t)
  
  whitebox::wbt_raster_streams_to_vector(streams = fname[str_detect(fname, paste0("stream_", x$region, ".tif"))],
                                         d8_pntr = fname[str_detect(fname, paste0("dir_", x$region, ".tif"))],
                                         output = fname[str_detect(fname, paste0("stream_", x$region, ".shp"))])
  
  ## st save
  sf::st_read(fname[str_detect(fname, paste0("stream_", x$region, ".shp"))]) %>%
    sf::st_set_crs(4326) %>%
    saveRDS(sf_line)
  
  remove(list = ls()[which(ls() != "fname")])
  gc(reset = TRUE)
}

file.remove(list.files("C:\\Users\\a_terui\\AppData\\Local\\Temp",
                       full.names = TRUE,
                       recursive = TRUE))

