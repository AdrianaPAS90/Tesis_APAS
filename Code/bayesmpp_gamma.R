#rm(list=ls())

source("C:/Users/SONY/Documents/GitHub/Tesis_APAS/Code/slice.sampler/uni.slice.R")

#Definicion de variables
#d <- 5
#alpha_gamma <- 2
#gamma_ <- 3
#beta_gamma <- 2
#c <- 4

#gamma_sim = 2

bayesmpp_gamma_ <- function(x0=2,alpha_gamma_sim, beta_gamma_sim, d, c){
  
  g<- function(gamma_, d=5, c=4, alpha_gamma_sim=2, beta_gamma_sim=2){
    -(d+alpha_gamma_sim+1)*log(gamma_)-((beta_gamma_sim/gamma_)+(c/gamma_)^d)
  }
  
  bg <- uni.slice(x0=2, g, w=1, m=Inf, lower=0, upper=+Inf, gx0=NULL)
  return(bg)
}
