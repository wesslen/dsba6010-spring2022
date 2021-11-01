# library(rethinking)
# 
# data(reedfrogs)
# 
# d <- reedfrogs
# 
# head(d)
# 
# # make the tank cluster variable
# d$tank <- 1:nrow(d)
# 
# dat <- list(
#   S = d$surv,
#   N = d$density,
#   tank = d$tank)
# 
# # approximate posterior
# m13.1 <- ulam(
#   alist(
#     S ~ dbinom( N , p ) ,
#     logit(p) <- a[tank] ,
#     a[tank] ~ dnorm( 0 , 1.5 )
#   ), data=dat , chains=4 , log_lik=TRUE )
# 
# 
# 
# library(rethinking)
# data(chimpanzees)
# 
# # don't want any variables with NAs
# # also recode condition to an index {1,0} -> {1,2}
# d <- list( 
#   pulled_left = chimpanzees$pulled_left ,
#   prosoc_left = chimpanzees$prosoc_left ,
#   condition = as.integer( 2 - chimpanzees$condition ) ,
#   actor = as.integer( chimpanzees$actor ) ,
#   blockid = as.integer( chimpanzees$block )
# )
# 
# # simple logistic regression
# m1 <- ulam(
#   alist(
#     pulled_left ~ bernoulli(theta),
#     logit(theta) <- a + bp[condition]*prosoc_left  ,
#     a ~ normal(0,4),
#     bp[condition] ~ normal(0,1)
#   ) ,
#   data=d, chains=2, cores=1 , sample=TRUE )
# 
# precis(m1,depth=2)
# plot(m1,depth=2)
# pairs(m1)
