
# setup -------------------------------------------------------------------

source(here::here("code/library.R"))
source(here::here("code/set_functions.R"))

regex_key <- paste(paste(rep(c("dir", "upa")),
                         rep(c("ussw", "usnw", "use", "eu"), each = 2),
                         sep = "_"), collapse = "|")

key_rm <- paste(paste0(c("_ussw", "_usnw", "_use", "_eu"), ".tif"),
                collapse = "|")


# data --------------------------------------------------------------------

## temporary file names
fname <- c(paste0(tempdir(), "\\dir_", c("ussw", "usnw", "use", "eu"), ".tif"),
           paste0(tempdir(), "\\upa_", c("ussw", "usnw", "use", "eu"), ".tif"),
           paste0(tempdir(), "\\stream_", c("ussw", "usnw", "use", "eu"), ".tif"),
           paste0(tempdir(), "\\outlet.shp"),
           paste0(tempdir(), "\\pour.shp"),
           paste0(tempdir(), "\\wsd_", c("ussw", "usnw", "use", "eu"), ".tif"))

## save to temporary folder
### raster
list_rast <- list.files(here::here("data_fmt"),
                        full.names = T,
                        pattern = "(upa)|(dir)") %>% 
  lapply(rast)

lapply(seq_len(length(list_rast)),
       FUN = function(i) {
         
         key <- str_extract(terra::sources(list_rast[[i]]),
                            pattern = regex_key)
         
         terra::writeRaster(list_rast[[i]],
                            filename = fname[str_detect(fname, key)],
                            overwrite = TRUE)
       })

### outlet
list.files(here::here("data_fmt"),
           full.names = T,
           pattern = "outlet.rds") %>% 
  readRDS() %>%
  st_write(fname[str_detect(fname, "outlet")],
           append = FALSE)


# snapping ----------------------------------------------------------------

df_x <- expand.grid(a = 200,
                    region = c("ussw", "usnw", "use", "eu")) %>% 
  mutate(region = as.character(region))

foreach(x = iterators::iter(df_x, by = "row")) %do% {
  
  ## select/define file names
  z <- c(fname[str_detect(fname, x$region)],
         fname[str_detect(fname, "outlet|pour")])
  print(z)
  
  ## stream delineation
  wbt_extract_streams(flow_accum = z[str_detect(z, "upa")],
                      output = z[str_detect(z, paste0("stream_",
                                                      x$region,
                                                      ".tif"))],
                      threshold = x$a)
  
  ## pour point snapping to stream cells
  wbt_jenson_snap_pour_points(pour_pts = z[str_detect(z, "outlet")],
                              streams = z[str_detect(z, paste0("stream_",
                                                               x$region,
                                                               ".tif"))],
                              output = z[str_detect(z, "pour")],
                              snap_dist = 0.05) # unit in degree
  
  ## watershed delineation
  wbt_watershed(d8_pntr = z[str_detect(z, "dir")],
                output = z[str_detect(z, "wsd")],
                pour_pts = z[str_detect(z, "pour")])
  
  ## raster to vector
  rast(z[str_detect(z, "wsd")]) %>% 
    st_as_stars() %>% 
    st_as_sf(merge = TRUE,
             as_points = FALSE) %>% 
    rename_with(.fn = function(x) str_remove_all(x, key_rm),
                .cols = starts_with("wsd")) %>% 
    mutate(area = st_area(.)) %>% 
    group_by(wsd) %>% 
    slice(which.max(area)) %>% 
    ungroup() %>% 
    saveRDS(paste0("data_fmt/epsg4326_wsd_", x$region, ".rds"))
}

file.remove(list.files("C:\\Users\\a_terui\\AppData\\Local\\Temp",
                       full.names = TRUE,
                       recursive = TRUE))
