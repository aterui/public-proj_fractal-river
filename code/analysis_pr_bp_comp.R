
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


# bp ----------------------------------------------------------------------

df_bp <- df_str %>% 
  filter(a_t == 1) %>% 
  mutate(length = as.numeric(length)) %>% 
  group_by(name) %>% 
  summarize(n = n(),
            tl = sum(length),
            area = as.numeric(unique(area)),
            rate = fitdistrplus::fitdist(length, "exp") %>% 
              .$estimate) %>% 
  ungroup() %>% 
  mutate(pr = n / tl,
         bp = pexp(1, rate = rate)) %>%
  rename(river = name)


# plot --------------------------------------------------------------------

g_bpr1 <- df_bp %>% 
  ggplot(aes(x = pr,
             y = bp)) +
  geom_point(alpha = 0.2) +
  geom_line(data = tibble(pr = 0:50*0.1,
                          bp = pexp(1, pr)),
            alpha = 0.5) +
  geom_vline(xintercept = c(0.5, 1),
             linetype = "dotted") +
  labs(y = expression("Branching probability"~italic(p)~"[km"^"-1"*"]"),
       x = expression("Branching ratio"~italic(p)[r]~"(="~theta*")"~"[km"^"-1"*"]")) +
  theme_bw() +
  theme(panel.grid = element_blank())

g_bpr2 <- df_bp %>% 
  ggplot(aes(x = pr,
             y = bp)) +
  geom_point(alpha = 0.5) +
  geom_line(data = tibble(pr = 5:10*0.1,
                          bp = pexp(1, pr))) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank())

g_rate <- df_bp %>% 
  ggplot(aes(x = pr,
             y = rate)) +
  geom_abline(intercept = 0,
              slope = 1,
              color = "lightgray") +
  geom_point(alpha = 0.5) +
  labs(y = expression("Rate parameter"~theta~"[km"^"-1"*"]"),
       x = expression("Branching ratio"~italic(p)[r]~"[km"^"-1"*"]")) +
  theme_bw() +
  theme(panel.grid = element_blank())


# layout ------------------------------------------------------------------

g_bpr <- g_rate + (g_bpr1 + inset_element(g_bpr2,
                                          0.4, 0.1, 0.95, 0.6))

ggsave(g_bpr,
       filename = "output/figure_bpr.pdf",
       height = 4,
       width = 9)
