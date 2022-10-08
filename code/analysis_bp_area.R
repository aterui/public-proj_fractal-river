
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/library.R"))
source(here::here("code/set_functions.R"))


# data --------------------------------------------------------------------

df_str <- readRDS("data_fmt/epsg4326_strnet.rds") %>% 
  as_tibble() %>% 
  dplyr::select(-geometry,
                -which(str_detect(colnames(.), pattern = "\\.\\d{1,}"))) %>% 
  mutate(length = as.numeric(length)) %>% 
  filter(length > 0.09) ## greater than minimum stream length (km) possible


# bp ----------------------------------------------------------------------

df_bp <- df_str %>% 
  mutate(length = as.numeric(length)) %>% 
  group_by(name, a_t) %>% 
  summarize(n = n(),
            tl = sum(length),
            area = as.numeric(unique(area))) %>% 
  ungroup() %>% 
  mutate(prop_a = a_t / area,
         bp = n / tl) %>% 
  filter(a_t == 1) %>%
  rename(river = name)


# plot --------------------------------------------------------------------

g_bp_area <- df_bp %>% 
  ggplot(aes(y = bp,
             x = area)) +
  geom_point() +
  scale_x_continuous(trans = "log10") +
  labs(y = expression(p[r]~"[km"^"-1"*"]"),
       x = expression("Watershed area"~"[km"^"2"*"]")) +
  theme_bw()
