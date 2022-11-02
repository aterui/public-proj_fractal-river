
# setup -------------------------------------------------------------------

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
  rename(river = name)


# model fitting -----------------------------------------------------------

fit0 <- MASS::rlm(log(pr, 10) ~ log(a_t, 10) + river - 1,
                  data = df_pr)

fit1 <- MASS::rlm(log(pr, 10) ~ log(a_t, 10) * river - 1,
                  df_pr)

BF_all <- exp((BIC(fit1) - BIC(fit0))/2)
  
## data frame for prediction
X <- df_pr %>% 
  group_by(river) %>% 
  summarize(a_t = seq(min(a_t), max(a_t), length = 10)) %>% 
  ungroup()

y <- predict(fit0, X)

df_pred <- X %>% 
  mutate(pr = 10^y,
         pr_prime = 10^y / a_t^coef(fit0)[1]) %>% 
  left_join(df_pr %>% 
              distinct(river, prop_a),
            by = "river")

df_pr <- df_pr %>% 
  mutate(pr_prime = pr / a_t^coef(fit0)[1])


# plot --------------------------------------------------------------------

## color setup
river <- unique(df_pr$river)

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

## plot: raw pr
g_pr <- df_pr %>%
  ggplot(aes(x = a_t,
             y = pr,
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
       y = expression("Branching ratio"~p[r]~"[km"^"-1"*"]"),
       x = expression("Observation scale"~A[T]~"[km"^"2"*"]")) +
  theme_bw() +
  theme(panel.grid = element_blank())

g_pr_facet <- g_pr +
  facet_wrap(facets = ~river, 10, 5) +
  guides(color = "none") +
  theme(strip.background = element_blank())

## plot: non-dimensional pr
g_prp <- df_pr %>%
  ggplot(aes(x = prop_a * 100,
             y = pr_prime,
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
  scale_x_continuous(trans = "log10", labels = scales::comma) +
  scale_y_continuous(trans = "log10") +
  scale_color_manual(values = cols,
                     breaks = ex_river,
                     labels = ex_river) +
  labs(color = "River",
       alpha = expression(log[10]~italic(N[L])),
       y = expression("Rescaled branching ratio"~bar(p)[r]~"[-]"),
       x = expression("% observation scale"~A[T]~"/A [%]")) +
  theme_bw() +
  theme(panel.grid = element_blank())

g_prp_facet <- g_prp +
  facet_wrap(facets = ~river, 10, 5) +
  guides(color = "none") +
  theme(strip.background = element_blank())


# export ------------------------------------------------------------------

g_all <- g_pr + g_prp + 
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave(g_pr,
       filename = here::here("output/figure_pr_all.pdf"),
       height = 6,
       width = 8)

ggsave(g_pr_facet,
       filename = here::here("output/figure_pr_facet.pdf"),
       height = 11,
       width = 11)

ggsave(g_prp_facet,
       filename = here::here("output/figure_prp_facet.pdf"),
       height = 11,
       width = 11)
