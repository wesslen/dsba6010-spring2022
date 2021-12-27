# https://github.com/GuangchuangYu/hexSticker
library(hexSticker)
library(ggplot2)
library(tibble)
library(magrittr)
library(dplyr)

library(showtext)
## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Dosis", "dosis")
## Automatically use showtext to render text for future devices
showtext_auto()

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



######

## R code 13.1
library(rethinking)
data(reedfrogs)
d <- reedfrogs
str(d)

## R code 13.2
# make the tank cluster variable
d$tank <- 1:nrow(d)

dat <- list(
  S = d$surv,
  N = d$density,
  tank = d$tank )

## R code 13.3
m13.2 <- ulam(
  alist(
    S ~ dbinom( N , p ) ,
    logit(p) <- a[tank] ,
    a[tank] ~ dnorm( a_bar , sigma ) ,
    a_bar ~ dnorm( 0 , 1.5 ) ,
    sigma ~ dexp( 1 )
  ), data=dat , chains=4 , log_lik=TRUE, cmdstan = TRUE )


## R code 13.5
# extract Stan samples
post <- extract.samples(m13.2)

# see tidybayes
library(tidybayes)
library(tidybayes.rethinking)
str(rethinking::extract.samples(m13.2))


get_variables(m13.2)

recover_types(m13.2)

tidy_draws(m13.2, a_bar)
# 
m13.2 %>%
  recover_types(dat) %>%
  spread_draws(a_bar) %>%
  head(10)

m13.2 %>%
  spread_draws(sigma, a_bar) %>%
  head(10)


m13.2 %>%
  spread_draws(a_bar, sigma) %>%
  median_qi() %>%
  ggplot(aes(y = 1, x = a_bar, xmin = .lower, xmax = .upper)) +
  geom_pointinterval()



p2 <- m13.2 %>%
  spread_draws(a_bar, sigma) %>%
  sample_draws(30) %>%
  ggplot(aes(y = 1)) +
  stat_dist_slab(aes(dist = "norm", arg1 = a_bar, arg2 = sigma), 
                 slab_color = "#F4B942", alpha = 7/10, fill = NA, size = 0.2
  ) +
  theme(legend.position = "none", panel.grid = element_blank()) +
  theme_transparent() +
  scale_x_continuous(NULL, breaks = NULL) +
  scale_y_continuous(NULL, breaks = NULL)



# plot( NULL , xlim=c(-3,4) , ylim=c(0,0.35) ,
#       xlab="log-odds survive" , ylab="Density" )
# for ( i in 1:100 )
#   curve( dnorm(x,post$a_bar[i],post$sigma[i]) , add=TRUE ,
#          col=col.alpha("black",0.2) )





######

pcolor = "#F4B942" #c73c41
hcolor = "#ffffff" #335F70
border_color = "#335F70"

sticker(p2,  package="DSBA 6010 | STAT 7027\nBayesian Statistics", p_size=3.2, p_color = hcolor, p_x = 1, p_y = 0.5,
        s_x=1, s_y=1.3, s_width=1.8, s_height=1.4,dpi = 32,
        h_fill = border_color, h_color = hcolor, h_size = 1.3,
        p_family = "dosis",
        filename="./static/img/icon-32.png")

# sticker(p2, package=" ", p_size=0, s_x=1, s_y=0.94, s_width=2, s_height=2.3, dpi = 32,
#         h_fill = hcolor, h_color = "#000000", h_size = 1.3,
#         filename="./static/img/icon-32.png")

sticker(p2,  package="DSBA 6010 | STAT 7027\nBayesian Statistics", p_size=3.2, p_color = hcolor, p_x = 1, p_y = 0.5,
        s_x=1, s_y=1.3, s_width=1.8, s_height=1.4,dpi = 192,
        h_fill = border_color, h_color = hcolor, h_size = 1.3, 
        p_family = "dosis",
        filename="./static/img/icon-192.png")

sticker(p2,  package="DSBA 6010 | STAT 7027\nBayesian Statistics", p_size=3.2, p_color = hcolor, p_x = 1, p_y = 0.5,
        s_x=1, s_y=1.3, s_width=1.8, s_height=1.4,dpi = 512,
        h_fill = border_color, h_color = hcolor, h_size = 1.3,
        p_family = "dosis",
        filename="./static/img/icon-512.png")

sticker(p2,  package="DSBA 6010 | STAT 7027\nBayesian Statistics", p_size=3.2, p_color = hcolor, p_x = 1, p_y = 0.5,
        s_x=1, s_y=1.3, s_width=1.8, s_height=1.4,dpi = 32,
        h_fill = border_color, h_color = hcolor, h_size = 1.3, 
        p_family = "dosis",
        filename="./assets/media/icon-32.png")

sticker(p2,  package="DSBA 6010 | STAT 7027\nBayesian Statistics", p_size=3.2,
        p_color = hcolor, p_x = 1, p_y = 0.5,
        s_x=1, s_y=1.3, s_width=1.8, s_height=1.4, dpi = 32,
        h_fill = border_color, h_color = hcolor, h_size = 1.3,
        p_family = "dosis",
        filename="./assets/media/icon.png")




sticker(p2, package="DSBA 6010 | STAT 7027\nBayesian Statistics", p_size=3.2, 
        p_color = hcolor, p_x = 1, p_y = 0.5,
        s_x=1, s_y=1.3, s_width=1.8, s_height=1.4, dpi = 192,
        h_fill = border_color, h_color = hcolor, h_size = 1.3,
        p_family = "dosis",
        filename="./assets/media/icon-192.png")

sticker(p2, package="DSBA 6010 | STAT 7027\nBayesian Statistics", p_size=3.2, 
        p_color = hcolor, p_x = 1, p_y = 0.5,
        s_x=1, s_y=1.3, s_width=1.8, s_height=1.4, dpi = 192,
        h_fill = border_color, h_color = hcolor, h_size = 1.3,
        p_family = "dosis",
        filename="./static/media/dsba6010-hex.png")

sticker(p2, package="DSBA 6010 | STAT 7027\nBayesian Statistics", p_size=3.2, 
        p_color = hcolor, p_x = 1, p_y = 0.5,
        s_x=1, s_y=1.3, s_width=1.8, s_height=1.4, dpi = 192,
        h_fill = border_color, h_color = hcolor, h_size = 1.3,
        p_family = "dosis",
        filename="./static/img/dsba6010-hex.png")



sticker(p2,  package="DSBA 6010 | STAT 7027\nBayesian Statistics", p_size=3.2, p_color = hcolor, p_x = 1, p_y = 0.5,
        s_x=1, s_y=1.3, s_width=1.8, s_height=1.4, dpi = 512,
        h_fill = border_color, h_color = hcolor, h_size = 1.3,
        p_family = "dosis",p_fontface = "bold",
        filename="./assets/media/icon-512.png")

sticker(p2,  package="DSBA 6010 | STAT 7027\nBayesian Statistics", p_size=3.2, p_color = hcolor, p_x = 1, p_y = 0.5,
        s_x=1, s_y=1.3, s_width=1.8, s_height=1.4, dpi = 384,
        h_fill = border_color, h_color = hcolor, h_size = 1.3,
        p_family = "dosis",
        filename="./assets/media/icon-384.png")
