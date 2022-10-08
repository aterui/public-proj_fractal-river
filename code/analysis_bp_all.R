
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
  filter(prop_a < 0.0125) %>% # 1.25% a_t to total area
  rename(river = name)


# model fitting -----------------------------------------------------------

fit0 <- lm(log(bp, 10) ~ log(a_t, 10) + river - 1,
           df_bp)

fit1 <- lm(log(bp, 10) ~ log(a_t, 10) * river - 1,
           df_bp)

BF <- exp((BIC(fit1) - BIC(fit0))/2)

## data frame for prediction
X <- df_bp %>% 
  group_by(river) %>% 
  summarize(a_t = seq(min(a_t), max(a_t), length = 10)) %>% 
  ungroup()

y <- predict(fit0, X)

df_pred <- X %>% 
  mutate(bp = 10^y,
         bp_prime = 10^y / a_t^coef(fit0)[1])

df_bp <- df_bp %>% 
  mutate(bp_prime = bp / a_t^coef(fit0)[1])


# plot --------------------------------------------------------------------

## color setup
river <- unique(df_bp$river)

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
          rep(grey(0.8, 0.4), length(river) - length(ex_river)))

names(cols) <- v_river

## plot: raw bp
g_bp <- df_bp %>%
  ggplot(aes(x = a_t,
             y = bp,
             color = river)) +
  geom_point(alpha = 0.3) +
  geom_line(data = df_pred) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  scale_color_manual(values = cols,
                     breaks = ex_river,
                     labels = ex_river) +
  labs(color = "River",
       y = expression(p[r]~"[km"^"-1"*"]"),
       x = expression(A[t]~"[km"^"2"*"]")) +
  theme_bw() +
  theme(panel.grid = element_blank())

## plot: raw bp
g_bpp <- df_bp %>%
  ggplot(aes(x = a_t,
             y = bp_prime,
             color = river)) +
  geom_point(alpha = 0.3) +
  geom_line(data = df_pred) +
  scale_x_continuous(trans = "log10") +
  scale_color_manual(values = cols,
                     breaks = ex_river,
                     labels = ex_river) +
  labs(color = "River",
       y = expression(bar(p)[r]~"[-]"),
       x = expression(A[t]~"[km"^"2"*"]")) +
  theme_bw() +
  theme(panel.grid = element_blank())

g_all <- g_bp + g_bpp + plot_layout(guides = "collect")
