
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/library.R"))
source(here::here("code/set_functions.R"))


# data --------------------------------------------------------------------

df_str <- readRDS("data_fmt/epsg4326_strnet.rds") %>% 
  as_tibble() %>% 
  dplyr::select(-geometry)


# bp ----------------------------------------------------------------------

df_bp <- df_str %>% 
  mutate(length = as.numeric(length)) %>% 
  group_by(name, a_t) %>% 
  summarize(n = n(),
            tl = sum(length),
            area = as.numeric(unique(area))) %>% 
  ungroup() %>% 
  mutate(bp = n / tl) %>% 
  filter(n > 3)

fit1 <- lme4::lmer(log(bp, 10) ~ log(a_t, 10) * name + (1 | name),
                   df_bp,
                   REML = F)

fit0 <- lme4::lmer(log(bp, 10) ~ log(a_t, 10) + (1 | name),
                   df_bp,
                   REML = F)

BF <- exp((BIC(fit1) - BIC(fit0))/2)

X <- model.matrix(~seq(0, log(max(df_bp$a_t), 10), length = 10))

df_pred <- X %*% t(coef(fit2)$name) %>% 
  as_tibble() %>% 
  mutate(log_a_t = seq(0, log(max(df_bp$a_t), 10), length = 10)) %>% 
  pivot_longer(cols = !starts_with("log_a_t")) %>% 
  mutate(bp = 10^(value),
         a_t = 10^(log_a_t))


# plot --------------------------------------------------------------------

df_bp %>% 
  ggplot(aes(x = a_t,
             y = bp,
             color = name)) +
  geom_point(alpha = 0.3) +
  geom_line(data = df_pred,
            aes(x = a_t,
                y = bp)) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  labs(color = "River",
       y = expression(p[r]~"[km"^"-1"*"]"),
       x = expression(A[t]~"[km"^"2"*"]")) +
  theme_bw()


  

