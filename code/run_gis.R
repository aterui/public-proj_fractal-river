
lapply(paste0("code/gis_",
              c("stream", "intersect_str_wsd"),
              #c("merge", "stream", "watershed", "intersect_str_wsd"),
              ".R"),
       source)
