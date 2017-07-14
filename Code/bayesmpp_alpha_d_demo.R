# 
#	Demo de "bayesmpp_alpha_d.R"
#

#rm(list=ls())

source("C:/Users/SONY/Documents/GitHub/Tesis_APAS/Code/slice.sampler/uni.slice.R")

#alpha_d <- 1
#alpha_theta <- 2
#t<-c(2,4,6,8,10)
#t1<-t(t)
#d <- t(t1)
#n<-nrow(d)
#beta_theta <- 1
#theta <- 3
#alpha_0 = 2
#beta_0 = 0.3 
#alpha_d_sim = 2

#j<-1
#for(j in 1:n){
  bayesmpp_alpha_d <- function(x0=2,d){
    if(j == 1){
      d_var<-d[j,]
    g<-function(alpha_d,d_var= 5,alpha_theta=2,beta_theta=1, theta=3, alpha_0=2,beta_0=0.3){
      (alpha_d+ alpha_theta)*log(d[j,]+beta_theta)-log(gamma(alpha_d+ alpha_theta))+alpha_d*(log(theta))+(alpha_0-1)*log(alpha_d)-(alpha_d*beta_0)
    }
  
    bad <- uni.slice(x0=2, g, w=1, m=Inf, lower=0, upper=+Inf, gx0=NULL)
    return(bad)
  
    }else{
      d_var_2<-d[j-1,]
    g <- function(alpha_d, d_var_2= 5, alpha_theta=2, beta_theta=1, theta=3, alpha_0=2, beta_0=0.3){
      (alpha_d+alpha_theta)*log(d_var_2+beta_theta)-log(gamma(alpha_d+ alpha_theta))+alpha_d*(log(theta))+(alpha_0-1)*log(alpha_d)-(alpha_d*beta_0)     
    }
   
    bad<-uni.slice(x0=2, g, w=1, m=Inf, lower=0, upper=+Inf, gx0=NULL)
    return(bad)
    }
  #}
}

