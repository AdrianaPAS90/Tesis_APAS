#rm(list=ls())

source("C:/Users/SONY/Documents/GitHub/Tesis_APAS/Code/slice.sampler/uni.slice.R")

#Definicion de variables
#alpha_d <- 1
#alpha_theta <- 2
#d <- 5
#beta_theta <-1
#theta <- 3
#alpha_0 = 2
#beta_0 = 0.3

#beta_theta_sim = 1


bayesmpp_beta_theta <- function(x0=3, alpha_d, alpha_theta, d){
  
  if(j == 1){
    d_var <- d[j,]
    
    g<- function(beta_theta,alpha_d=1,alpha_theta=2,d_var=5,theta=3,alpha_0=2,beta_0=0.3){
      g<-(alpha_d+alpha_theta)*log(d_var+beta_theta)+(alpha_0-1)*log(beta_theta)-beta_theta*(theta+beta_0)
  }
  
  bbt <- uni.slice(x0=3, g, w=1, m=Inf, lower=0, upper=+Inf, gx0=NULL)
  return(bbt)
  
  }else{
    d_var_2<-d[j-1,]
    
    g<- function(beta_theta,alpha_d=1,alpha_theta=2,d_var_2=5,theta=3,alpha_0=2,beta_0=0.3){
      g<-(alpha_d+alpha_theta)*log(d_var_2+beta_theta)+(alpha_0-1)*log(beta_theta)-beta_theta*(theta+beta_0)
    }
    
    bbt <- uni.slice(x0=3, g, w=1, m=Inf, lower=0, upper=+Inf, gx0=NULL)
    return(bbt)
  }
}