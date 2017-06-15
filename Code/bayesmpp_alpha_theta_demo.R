#rm(list=ls())

source("C:/Users/SONY/Documents/GitHub/Tesis_APAS/Code/slice.sampler/uni.slice.R")

#Definicion de variables
#alpha_d <- 1
#alpha_theta <- 2
#d <- 5
#beta_theta <- 1
#theta <- 3
#alpha_0 = 2
#beta_0 = 0.3

#alpha_theta_sim = 2


bayesmpp_alpha_theta <- function(x0=2, alpha_d, d){
  
  g<- function(alpha_theta,alpha_d=1,d=5,beta_theta=1,theta=3,alpha_0=2,beta_0=0.3){
    g<- (alpha_d+alpha_theta)*log(d+beta_theta)-log(gamma(alpha_d+alpha_theta))+alpha_theta*log(theta)+(alpha_0-1)*log(alpha_theta)-alpha_theta*beta_0
  }
  
  
  bat <- uni.slice(x0=2, g, w=1, m=Inf, lower=0, upper=+Inf, gx0=NULL)
  return(bat)
}
