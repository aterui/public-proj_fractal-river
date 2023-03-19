
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/library.R"))
source(here::here("code/set_functions.R"))


# data --------------------------------------------------------------------

df_str <- readRDS(here::here("data_fmt/epsg4326_strnet.rds")) %>% 
  as_tibble() %>% 
  dplyr::select(-geometry,
                -which(str_detect(colnames(.), pattern = "\\.\\d{1,}"))) %>% 
  mutate(length = as.numeric(length)) %>% 
  filter(length > 0.09) ## greater than minimum stream length (km) possible (DEM resl = 90m)


# pr ----------------------------------------------------------------------

df_p <- df_str %>% 
  mutate(length = as.numeric(length)) %>% 
  group_by(name, a_t) %>% 
  summarize(n = n(),
            tl = sum(length),
            area = as.numeric(unique(area))) %>% 
  ungroup() %>% 
  mutate(prop_a = a_t / area,
         pr = n / tl,
         p = 1 - exp(-pr),
         n = as.numeric(n)) %>% 
  rename(river = name)


# model fitting -----------------------------------------------------------

fit0 <- MASS::rlm(log(p, 10) ~ log(a_t, 10) + river - 1,
                  data = df_p)

## data frame for pediction
X <- df_p %>% 
  group_by(river) %>% 
  summarize(a_t = seq(min(a_t), max(a_t), length = 10)) %>% 
  ungroup()

y <- predict(fit0, X)

df_pred <- X %>% 
  mutate(p = 10^y,
         p_prime = 10^y / a_t^coef(fit0)[1]) %>% 
  left_join(df_p %>% 
              distinct(river, prop_a),
            by = "river")

df_p <- df_p %>% 
  mutate(p_prime = p / a_t^coef(fit0)[1])


# plot --------------------------------------------------------------------

## color setup
river <- unique(df_p$river)

ex_river <- c("KleineEmme",
              "Chisone",
              "Tanaro",
              "Magra",
              "Piave",
              "Eel",
              "Thompson",
              "Willamette",
              "Marias",
              "Klamath",
              "Owyhee") %>% sort()

v_river <- c(river[river %in% ex_river],
             river[!(river %in% ex_river)])

cols <- c(rainbow(length(ex_river)),
          rep(grey(0.75, 0.4), length(river) - length(ex_river)))

names(cols) <- v_river

r <- 0.75
lwd <- 0.2

## plot: raw p
g_p <- df_p %>%
  ggplot(aes(x = a_t,
             y = p,
             color = river)) +
  geom_point(data = . %>% filter(!(river %in% ex_river)),
             aes(alpha = log(n, 10)),
             size = r) +
  geom_line(data = df_pred %>% filter(!(river %in% ex_river)),
            size = lwd) +
  geom_point(data = . %>% filter(river %in% ex_river),
             aes(alpha = log(n, 10)),
             size = r) +
  geom_line(data = df_pred %>% filter(river %in% ex_river),
            size = lwd) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  scale_color_manual(values = cols,
                     breaks = ex_river,
                     labels = ex_river) +
  labs(color = "River",
       alpha = expression(log[10]~italic(N[L])),
       y = expression("Branching probability"~p~"[km"^"-1"*"]"),
       x = expression("Observation scale"~A[T]~"[km"^"2"*"]")) +
  theme_bw() +
  theme(panel.grid = element_blank())


# export ------------------------------------------------------------------

ggsave(g_p,
       filename = here::here("output/figure_p.pdf"),
       height = 6,
       width = 8)
