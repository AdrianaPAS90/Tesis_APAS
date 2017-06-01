rm(list=ls())

source("C:/Users/SONY/Documents/GitHub/Tesis_APAS/Code/slice.sampler/uni.slice.R")

#Definicion de variables
alpha_gamma <- 2
gamma_ <- 3
alpha_0 = 2
beta_0 = 0.3

alpha_gamma_sim = 2


g<- function(alpha_gamma, gamma_=3,alpha_0=2,beta_0=0.3){
  (-alpha_gamma)*log(gamma_)+(alpha_0-1)*log(alpha_gamma)-(alpha_gamma*beta_0)
}

bayesmpp_alpha_gamma <- function(x0=2){
  bag <- uni.slice(x0, g, w=1, m=Inf, lower=0, upper=+Inf, gx0=NULL)
  return(bag)
}
