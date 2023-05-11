# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/library.R"))
source(here::here("code/set_functions.R"))


# data --------------------------------------------------------------------

## scale

xx <- c(outer(2^(0:2), 5^(0:3)))
x <- c(seq(1, 500, by = 1),
       xx)

x <- x[!duplicated(x)]
x <- x[x > 1]

## coastline
y <- (10^3.5)*x^-0.25

## square
n <- floor(500 / x)
y0 <- n * x * 4

## data frame
df0 <- tibble(x = x,
              y0 = y0,
              y = y)

# figure ------------------------------------------------------------------


## 
g1 <- df0 %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_line() +
  geom_line(aes(y = y0),
            color = grey(0.5, 0.5)) +
  geom_line(data = tibble(x = x,
                          y = 2000),
            color = grey(0.5, 0.5),
            linetype = "dotted") +
  geom_point(data = df0 %>% 
               filter(y0 == 2000),
             aes(y = y0),
             size = 0.75,
             color = "salmon") +
  labs(y = "Length y",
       x = "Ruler length x") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

g2 <- g1 +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log") +
  theme_bw() +
  labs(y = "ln Length y",
       x = "ln Ruler length x") +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

g0 <- g1 + ggtitle("Ordinary scale") + g2 + ggtitle("Log-log scale")

ggsave(g0,
       filename = "output/figure_example.pdf",
       width = 8,
       height = 4)