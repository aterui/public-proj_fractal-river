
# setup -------------------------------------------------------------------

source(here::here("code/library.R"))
source(here::here("code/set_functions.R"))


# data --------------------------------------------------------------------

df_str <- readRDS(here::here("data_fmt/epsg4326_strnet.rds")) %>% 
  as_tibble() %>% 
  dplyr::select(-geometry,
                -which(str_detect(colnames(.), pattern = "\\.\\d{1,}"))) %>% 
  mutate(length = as.numeric(length)) %>% 
  filter(length > 0.09) ## greater than minimum stream length (km) possible (DEM resl = 90 m)


# pr ----------------------------------------------------------------------

df_pr <- df_str %>% 
  mutate(length = as.numeric(length)) %>% 
  group_by(name, a_t) %>% 
  summarize(n = n(),
            tl = sum(length),
            area = as.numeric(unique(area))) %>% 
  ungroup() %>% 
  mutate(prop_a = a_t / area,
         pr = n / tl,
         n = as.numeric(n)) %>% 
  filter(area > 5000) %>% # 50% a_t to total area
  rename(river = name)


# model fitting -----------------------------------------------------------

fit0 <- MASS::rlm(log(pr, 10) ~ log(a_t, 10) + river - 1,
                  data = df_pr)

fit1 <- MASS::rlm(log(pr, 10) ~ log(a_t, 10) * river - 1,
                  df_pr)

BF_sub <- exp((BIC(fit1) - BIC(fit0))/2)

