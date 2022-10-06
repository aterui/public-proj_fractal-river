
lapply(paste0("code/gis_",
              c("merge", "stream", "watershed", "intersect_str_wsd"),
              ".R"),
       source)
