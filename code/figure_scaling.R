
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/analysis_pr_all.R"))

# data --------------------------------------------------------------------

df_str <- readRDS(here::here("data_fmt/epsg4326_strnet.rds")) %>% 
  mutate(length = as.numeric(length)) %>% 
  filter(length > 0.09) ## greater than minimum stream length (km) possible (DEM resl = 90m)


# plot --------------------------------------------------------------------

v_a <- unique(df_str$a_t)
A <- c(v_a[1], v_a[14])

g_str <- foreach(i = 1:length(A)) %do% {
  df_eel <- df_str %>% 
    filter(name == "Eel",
           a_t == A[i]) %>% 
    ggplot() +
    geom_sf(color = "steelblue") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggtitle(bquote(list(A[T]==.(A[i])~km^2)))
}

layout <- 
"ADE
 BDE"
 
g_all <- g_str[[1]] + g_str[[2]] + g_p + guide_area() + 
  plot_layout(design = layout,
              guides = "collect",
              widths = c(1, 2, 1)) +
  plot_annotation(tag_levels = "A")

ggsave(g_all,
       filename = "output/figure_scaling.pdf",
       width = 12,
       height = 6)
