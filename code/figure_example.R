# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/library.R"))
source(here::here("code/set_functions.R"))


# figure ------------------------------------------------------------------

x <- exp(seq(log(0.1), log(500), length = 100))
y <- (10^3.5)*x^-0.25

df0 <- tibble(x = x,
              y = y)

g1 <- df0 %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_line() +
  geom_line(data = tibble(x = seq(0.1, 500, by = 0.1),
                          y = 2000),
            color = grey(0.5, 0.5)) +
  labs(y = "Length y",
       x = "Ruler length x") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

g2 <- g1 +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  geom_line(data = tibble(x = seq(0.1, 500, by = 0.1),
                          y = 2000),
            color = grey(0.5, 0.5)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

g0 <- g1 + ggtitle("Ordinary scale") + g2 + ggtitle("Log-log scale")

ggsave(g0,
       filename = "output/figure_example.pdf",
       width = 8,
       height = 4)