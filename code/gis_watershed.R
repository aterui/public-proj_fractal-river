
# setup -------------------------------------------------------------------

source(here::here("code/library.R"))
source(here::here("code/set_functions.R"))


# data --------------------------------------------------------------------

fname <- c(paste0(tempdir(), "\\dir_d8_", c("e", "w"), ".tif"),
           paste0(tempdir(), "\\upa_", c("e", "w"), ".tif"),
           paste0(tempdir(), "\\stream_", c("e", "w"), ".tif"),
           paste0(tempdir(), "\\outlet_", c("e", "w"), ".shp"),
           paste0(tempdir(), "\\pour_", c("e", "w"), ".shp"),
           paste0(tempdir(), "\\wsd_", c("e", "w"), ".tif"))

## raster
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

## outlet
list.files(here::here("data_raw"),
           full.names = T,
           pattern = "outlet_.{1,}.rds") %>% 
  lapply(function(x) {
    print(fname[str_detect(fname, str_extract(x, "outlet_."))])
    
    readRDS(x) %>%
      st_write(fname[str_detect(fname, str_extract(x, "outlet_."))],
               append = FALSE)
  })


# snapping ----------------------------------------------------------------

df_x <- expand.grid(a = 5,
                    region = c("_e", "_w")) %>% 
  mutate(region = as.character(region))

foreach(x = iterators::iter(df_x, by = "row")) %do% {
  
  ## select/define file names
  z <- fname[str_detect(fname, (x$region))]
  print(z)
  
  ## stream delineation
  wbt_extract_streams(flow_accum = z[str_detect(z, "upa")],
                      output = z[str_detect(z, paste0("stream",
                                                      x$region,
                                                      ".tif"))],
                      threshold = x$a)
  
  ## pour point snapping to stream cells
  wbt_jenson_snap_pour_points(pour_pts = z[str_detect(z, "outlet")],
                              streams = z[str_detect(z, paste0("stream",
                                                               x$region,
                                                               ".tif"))],
                              output = z[str_detect(z, "pour")],
                              snap_dist = 1)
  
  ## watershed delineation
  wbt_watershed(d8_pntr = z[str_detect(z, "dir")],
                output = z[str_detect(z, "wsd")],
                pour_pts = z[str_detect(z, "pour")])
  
  ## raster to vector
  rast(z[str_detect(z, "wsd")]) %>% 
    st_as_stars() %>% 
    st_as_sf(merge = TRUE,
             as_points = FALSE) %>% 
    rename_with(.fn = function(x) str_remove_all(x, "_e.tif|_w.tif"),
                .cols = starts_with("wsd")) %>% 
    mutate(area = st_area(.)) %>% 
    group_by(wsd) %>% 
    slice(which.max(area)) %>% 
    ungroup() %>% 
    saveRDS(paste0("data_fmt/epsg4326_wsd", x$region, ".rds"))
}

file.remove(fname)
