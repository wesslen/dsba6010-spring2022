# https://github.com/GuangchuangYu/hexSticker
library(hexSticker)
library(ggplot2)
library(tibble)
library(magrittr)
library(dplyr)

d <-
  tibble(position = c((1:4^1) / 4^0, 
                      (1:4^2) / 4^1, 
                      (1:4^3) / 4^2),
         draw     = rep(1:3, times = c(4^1, 4^2, 4^3)),
         fill     = rep(c("b", "w"), times = c(1, 3)) %>% 
           rep(., times = c(4^0 + 4^1 + 4^2)))

lines_1 <-
  tibble(x    = rep((1:4), each = 4),
         xend = ((1:4^2) / 4),
         y    = 1,
         yend = 2)

lines_1 <-
  lines_1 %>% 
  mutate(remain = c(rep(0:1, times = c(1, 3)),
                    rep(0,   times = 4 * 3)))

lines_2 <-
  tibble(x    = rep(((1:4^2) / 4), each = 4),
         xend = (1:4^3) / (4^2),
         y    = 2,
         yend = 3)

## remain

lines_1 <-
  lines_1 %>% 
  mutate(remain = c(rep(0:1, times = c(1, 3)),
                    rep(0,   times = 4 * 3)))
lines_2 <-
  lines_2 %>% 
  mutate(remain = c(rep(0,   times = 4),
                    rep(1:0, times = c(1, 3)) %>% rep(., times = 3),
                    rep(0,   times = 12 * 4)))
d <-
  d %>% 
  mutate(remain = c(rep(1:0, times = c(1, 3)),
                    rep(0:1, times = c(1, 3)),
                    rep(0,   times = 4 * 4),
                    rep(1:0, times = c(1, 3)) %>% rep(., times = 3),
                    rep(0,   times = 12 * 4))) 


# finally, the plot:
p <- d %>% 
  ggplot(aes(x = position, y = draw)) +
  geom_segment(data = lines_1,
               aes(x = x, xend = xend,
                   y = y, yend = yend,
                   alpha = remain %>% as.character()),
               size = 1/3) +
  geom_segment(data = lines_2,
               aes(x = x, xend = xend,
                   y = y, yend = yend,
                   alpha = remain %>% as.character()),
               size = 1/3) +
  geom_point(aes(fill = fill, alpha = remain %>% as.character()),
             shape = 21, size = 2) +
  # it's the alpha parameter that makes elements semitransparent
  scale_fill_manual(values = c("navy", "white")) +
  scale_alpha_manual(values = c(1/5, 1)) +
  scale_x_continuous(NULL, limits = c(0, 4), breaks = NULL) +
  scale_y_continuous(NULL, limits = c(0.75, 3), breaks = NULL) +
  coord_polar() + 
  theme(legend.position = "none", panel.grid = element_blank()) +
  theme_transparent()
  #geom_url(url = "dsba6010.com")



sticker(p, package=" ", p_size=0, s_x=1, s_y=0.94, s_width=2, s_height=2.3, dpi = 32,
        p_color = "#B0BBBF", h_color = "#02394A",
        filename="./static/img/icon-32.png")

sticker(p, package=" ", p_size=0, s_x=1, s_y=0.94, s_width=2, s_height=2.3, dpi = 192,
        p_color = "#B0BBBF", h_color = "#02394A",
        filename="./static/img/icon-192.png")

sticker(p, package=" ", p_size=0, s_x=1, s_y=0.94, s_width=2, s_height=2.3, dpi = 512,
        p_color = "#B0BBBF", h_color = "#02394A",
        filename="./static/img/icon-512.png")

sticker(p, package=" ", p_size=0, s_x=1, s_y=0.94, s_width=2, s_height=2.3, dpi = 32,
        p_color = "#B0BBBF", h_color = "#02394A",        
        filename="./assets/media/icon-32.png")

sticker(p, package=" ", p_size=0, s_x=1, s_y=0.94, s_width=2, s_height=2.3, dpi = 192,
        p_color = "#B0BBBF", h_color = "#02394A",
        filename="./assets/media/icon-192.png")

sticker(p, package=" ", p_size=0, s_x=1, s_y=0.94, s_width=2, s_height=2.3, dpi = 512,
        p_color = "#B0BBBF", h_color = "#02394A",
        filename="./assets/media/icon-512.png")

sticker(p, package=" ", p_size=0, s_x=1, s_y=0.94, s_width=2, s_height=2.3, dpi = 384,
        p_color = "#B0BBBF", h_color = "#02394A",
        filename="./assets/media/icon-384.png")
