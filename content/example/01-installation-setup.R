# step 1: install R and RStudio
# what is your OS? linux, Mac or Windows

# need to install rethinking: https://github.com/rmcelreath/rethinking#quick-installation

# install rstan first
# go to http://mc-stan.org
# https://github.com/rmacoslib/r-macos-rtools/releases/tag/v4.0.0
library(rstan)

# https://github.com/stan-dev/rstan/wiki/Configuring-C---Toolchain-for-Mac

# https://mc-stan.org/cmdstanr/
library(cmdstanr)

#install.packages(c("coda","mvtnorm","devtools","loo","dagitty","shape"))
#devtools::install_github("rmcelreath/rethinking")
library(rethinking)

f <- alist(
  y ~ dnorm( mu , sigma ),
  mu ~ dnorm( 0 , 10 ),
  sigma ~ dexp( 1 )
)

fit <- quap( 
  f , 
  data=list(y=c(-1,1)) , 
  start=list(mu=0,sigma=1)
)

precis(fit)
# mean   sd  5.5% 94.5%
#   mu    0.00 0.59 -0.95  0.95
# sigma 0.84 0.33  0.31  1.36

# check if cmdstan runs
fit_stan <- ulam( f , data=list(y=c(-1,1)), cmdstan=TRUE)
# check if rstan run
fit_stan <- ulam( f , data=list(y=c(-1,1)), cmdstan=FALSE)


# if doesn't work then cmdstan=TRUE, or set forever: set_ulam_cmdstan(TRUE)



library(rethinking)
data(chimpanzees)

# don't want any variables with NAs
# also recode condition to an index {1,0} -> {1,2}
d <- list( 
  pulled_left = chimpanzees$pulled_left ,
  prosoc_left = chimpanzees$prosoc_left ,
  condition = as.integer( 2 - chimpanzees$condition ) ,
  actor = as.integer( chimpanzees$actor ) ,
  blockid = as.integer( chimpanzees$block )
)

# simple logistic regression
m1 <- ulam(
  alist(
    pulled_left ~ bernoulli(theta),
    logit(theta) <- a + bp[condition]*prosoc_left  ,
    a ~ normal(0,4),
    bp[condition] ~ normal(0,1)
  ) ,
  data=d, chains=2, cores=1 , sample=TRUE )

precis(m1,depth=2)
plot(m1,depth=2)
pairs(m1)


# faq:

# cmdstan note: please rebuild precompiled header
# run cmdstan::rebuild_cmdstan()
