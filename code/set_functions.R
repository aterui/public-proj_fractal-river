library(tidyverse)


# round digits ------------------------------------------------------------

op <- function(x, d = 2) sprintf(paste0("%1.", d, "f"), x) 

# safe conversion from matrix to vector -----------------------------------

m2v <- function(x) {
  
  row <- rownames(x)
  col <- colnames(x)
  
  if(is.null(row)) row <- as.character(1:nrow(x))
  if(is.null(col)) col <- as.character(1:ncol(x))
  
  m_name <- outer(row, col,
                  paste, sep="_by_" )
  
  df_x <- dplyr::tibble(value = c(x),
                        id = c(m_name)) %>% 
    tidyr::separate(col = id,
                    into = c("row", "col"),
                    sep = "_by_",
                    convert = TRUE)
  
  return(df_x)
}


# remove brackets ---------------------------------------------------------

fn_brrm <- function(x) {
  y <- lapply(str_extract_all(x, pattern = "\\[.{1,}\\]"),
              FUN = function(z) ifelse(identical(z, character(0)),
                                       NA,
                                       z))
  str_remove_all(y, pattern = "\\[|\\]")
}


# convert Arc pointer to D8 pointer ---------------------------------------

arc2d8 <- function(x) {
  z <- 0:7
  fdir_d8 <- 2^z
  fdir_arc <- fdir_d8[c(8, 1:7)] * 3
  
  # unique pointer values for ArcGIS flow direction
  x <- x * 3
  
  for(i in seq_len(length(fdir_d8))) {
    x[x == fdir_arc[i]] <- fdir_d8[i]
  }
  
  # remove unassigned cells
  us <- c(247, 255, 0) * 3
  x[x %in% us] <- NA
  
  return(x)  
}


# get mean or NA ----------------------------------------------------------

f_num <- function(x) {
  y <- unique(na.omit(x))
  
  if (length(y) == 0) {
    y <- NA
  }
  
  if (length(y) > 1) {
    y <- mean(x, na.rm = T)
  }
  
  return(round(y, 1))
}


# species abbreviation ----------------------------------------------------

spabb <- function(x, sep = "\\s") {
  
  y <- str_split(x, sep)
  
  z <- lapply(y, FUN = function(q) {
    
    if (any(is.na(q)) | any(is.null(q))) z <- NA
    
    if (length(q) > 1) {
      ## length > 1
      if (any(str_detect(q, "(spp$)|(sp$)"))) {
        ## sp or spp
        z <- paste(str_to_sentence(q[1]),
                   paste0(na.omit(str_extract(q, "(^sp{1,2})$")),
                          "."))
      } else {
        ## full name      
        q[1:(length(q) - 1)] <- sapply(1:(length(q) - 1),
                                       FUN = function(i) str_extract(q[i], "^."))
        
        z <- q[1]
        
        for (i in 2:length(q)) {
          z <- paste(z, q[i], sep = ". ")
        }
        
      }
    } else {
      ## length <= 1
      z <- q
    }
    
    return(z)
  })
  
  z <- str_to_sentence(z)
  
  return(z)
}
