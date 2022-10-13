
# setup -------------------------------------------------------------------

source(here::here("code/library.R"))
source(here::here("code/set_functions.R"))


# data --------------------------------------------------------------------

df_str <- readRDS(here::here("data_fmt/epsg4326_strnet.rds")) %>% 
  as_tibble() %>% 
  dplyr::select(-geometry,
                -which(str_detect(colnames(.), pattern = "\\.\\d{1,}"))) %>% 
  mutate(length = as.numeric(length)) %>% 
  filter(length > 0.09) ## greater than minimum stream length (km) possible


# pr ----------------------------------------------------------------------

df_pr <- df_str %>% 
  mutate(length = as.numeric(length)) %>% 
  filter(a_t == 1) %>%
  group_by(name) %>% 
  summarize(n = n(),
            tl = sum(length),
            area = as.numeric(unique(area))) %>% 
  ungroup() %>% 
  mutate(pr = n / tl) %>% 
  rename(river = name)

cor_pa <- cor.test(df_pr$pr, df_pr$area, method = "spearman")


# plot --------------------------------------------------------------------

g_pra <- df_pr %>% 
  ggplot(aes(x = area,
             y = pr)) +
  geom_point() +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  labs(color = "River",
       alpha = expression(log[10]~italic(N[L])),
       y = expression("Branching ratio"~p[r]~"[km"^"-1"*"]"),
       x = expression("Watershed area"~A~"[km"^"2"*"]")) +
  theme_bw() +
  theme(panel.grid = element_blank())

ggsave(g_pra,
       filename = here::here("output/figure_pra.pdf"),
       height = 4, width = 5)
