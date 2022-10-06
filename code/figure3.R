
# setup -------------------------------------------------------------------

rm(list = ls())

pacman::p_load(tidyverse,
               patchwork,
               lme4)

df_resl <- read_csv(here::here("results/TableS1_draft.csv")) %>% 
  rename(id = ...1)

# data --------------------------------------------------------------------

files <- list.files(path = here::here("results"),
                    pattern = "data",
                    full.names = T)

for(i in 1:3) load(files[i])

list_df0 <- list(data20, data100, data500)
a_t <- c(20, 100, 500)

df_m <- lapply(1:length(list_df0), function(i) {
  
  x <- list_df0[[i]]
  
  df_n <- x$nNodes %>% 
    rename_with(.cols = everything(),
                .fn = paste0,
                "_n") %>% 
    mutate(id = as.numeric(rownames(.)))
  
  df_link <- x$nLinks %>% 
    rename_with(.cols = everything(),
                .fn = paste0,
                "_nl") %>% 
    mutate(id = as.numeric(rownames(.)))
  
  df_dist <- x$mean_dist %>% 
    rename_with(.cols = everything(),
                .fn = paste0,
                "_dist") %>% 
    mutate(id = as.numeric(rownames(.)))
  
  df_resl %>% 
    left_join(df_n,
              by = "id") %>% 
    left_join(df_link,
              by = "id") %>% 
    left_join(df_dist,
              by = "id") %>% 
    rename_with(.cols = everything(), .fn = str_to_lower) %>% 
    mutate(a_t_pixel = a_t[i],
           a_t_km2 = (a_t_pixel * (0.001 * l_m)^2),
           p_r = rivers_nl / rivers_n)
}) %>% 
  bind_rows()

# figure ------------------------------------------------------------------

df_sub <- df_m %>% 
  filter(name %in% c("KleineEmme",
                     "Chisone",
                     "Tanaro",
                     "Magra",
                     "Piave",
                     "Eel",
                     "Thompson",
                     "Willamette",
                     "Marias",
                     "Klamath",
                     "Owyhee"))

g_pixel <- df_sub %>% 
  ggplot(aes(y = p_r,
             x = a_t_pixel,
             color = name)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(trans = "log10") +
  labs(x = expression(A[T]~"[no. pixels]"),
       y = expression(p[r]~"["*pixel~length^{-1}*"]"),
       color = "River name") +
  theme_bw()

g_km <- df_sub %>% 
  ggplot(aes(y = p_r,
             x = a_t_km2,
             color = name)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(trans = "log10") +
  labs(x = expression(A[T]~"["*km^{2}*"]"),
       y = expression(p[r]~"["*pixel~length^{-1}*"]"),
       color = "River name") +
  theme_bw()

g0 <- g_pixel + g_km + plot_layout(guides = "collect") + plot_annotation(tag_levels = "A")

ggsave(g0,
       filename = here::here("output/fig3_replot.pdf"),
       height = 5,
       width = 12)
