# 
#	Demo de "bayesmpp_alpha_d.R"
#

rm(list=ls())

source("C:/Users/SONY/Documents/GitHub/Tesis_APAS/Code/slice.sampler/uni.slice.R")

alpha_d <- 1
alpha_theta <- 2
d <- 5
beta_theta <- 1
theta <- 3
alpha_0 = 2
beta_0 = 0.3 
alpha_d_sim = 2


g<-function(alpha_d=1,alpha_theta=2, d=5,beta_theta=1, theta=3, alpha_0=2,beta_0=0.3){
  g <- (alpha_d+ alpha_theta)*log(d+beta_theta)-log(gamma(alpha_d+ alpha_theta))+alpha_d*(log(theta))+(alpha_0-1)*log(alpha_d)-(alpha_d*beta_0)
}


bayesmpp_alpha_d <- function(x0=2){
  x <- uni.slice(x0, g, w=1, m=Inf, lower=-Inf, upper=+Inf, gx0=NULL)
  return(x)
}


