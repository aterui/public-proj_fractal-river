
# setup -------------------------------------------------------------------

source(here::here("code/library.R"))
source(here::here("code/set_functions.R"))


# data --------------------------------------------------------------------

fname <- c(paste0(tempdir(), "\\dir_d8_", c("e", "w"), ".tif"),
           paste0(tempdir(), "\\upa_", c("e", "w"), ".tif"),
           paste0(tempdir(), "\\stream_", c("e", "w"), ".tif"),
           paste0(tempdir(), "\\stream_", c("e", "w"), ".shp"),
           paste0(tempdir(), "\\outlet", ".shp"))

list_rast <- list.files(here::here("data_fmt"),
                        full.names = T,
                        pattern = "upa|dir") %>% 
  lapply(rast)

lapply(seq_len(length(list_rast)),
       FUN = function(i) {
         
         key <- str_extract(terra::sources(list_rast[[i]]),
                            pattern = "dir_d8_e|dir_d8_w|upa_e|upa_w") 
         
         terra::writeRaster(list_rast[[i]],
                            filename = fname[str_detect(fname, key)],
                            overwrite = TRUE)
       })


# whitebox ----------------------------------------------------------------

df_x <- expand.grid(a = round(10^seq(0, log(500, 10), length = 10)),
                    region = c("_e", "_w")) %>% 
  mutate(region = as.character(region))

foreach(x = iterators::iter(df_x, by = "row")) %do% {
  
  ## select/define file names
  z <- fname[str_detect(fname, (x$region))]
  sf_line <- paste0("data_fmt/epsg4326_",
                    "str_a",
                    x$a,
                    x$region,
                    ".rds")
  print(sf_line)
  
  ## stream delineation
  wbt_extract_streams(flow_accum = z[str_detect(z, "upa")],
                      output = z[str_detect(z, paste0("stream",
                                                      x$region,
                                                      ".tif"))],
                      threshold = x$a)

  wbt_raster_streams_to_vector(streams = z[str_detect(z, paste0("stream",
                                                                x$region,
                                                                ".tif"))],
                               d8_pntr = z[str_detect(z, "dir")],
                               output = z[str_detect(z, paste0("stream",
                                                               x$region,
                                                               ".shp"))])
  
  ## st save
  st_read(z[str_detect(z, paste0("stream",
                                 x$region,
                                 ".shp"))]) %>%
    st_set_crs(4326) %>%
    saveRDS(sf_line)
}

file.remove(fname)


