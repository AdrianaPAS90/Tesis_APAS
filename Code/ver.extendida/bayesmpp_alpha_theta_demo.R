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

I<-max(datos$paciente)

bayesmpp_alpha_theta <- function(x0=2, alpha_d){
  
  g <- function(alpha_theta,alpha_d=1,beta_theta=1,theta=3,alpha_0=2,beta_0=0.3){
    
    adi <- 0
    for(i in 1:I){
      adi <- adi + T_n(i)
    }
    
    a <- adi*(((alpha_0-1)*log(alpha_theta))-(alpha_theta*beta_0)-(log(gamma(alpha_d+alpha_theta))))
    v_1 <- 0
    v_2 <- 0
    for (i in 1:I){
      v_1<- v_1 + v_2
      v_2<-0
      for(j in 2:T_n(i)){
        v_2 <- v_2 + (((alpha_d+alpha_theta)*log(d(i,j-1)+beta_theta))+((alpha_theta)*log(theta)))
      }
    }
    b <- v_1
    g <- a + b
    return(g)
  }
  
  bat<-uni.slice(x0=2, g, w=1, m=10, lower=0, upper=+Inf, gx0=NULL)
  return(bat)
}




#bayesmpp_alpha_theta <- function(x0=2, alpha_d, d){
  #if(j == 1){
    #d_var <- d[j,]
    
    #g<- function(alpha_theta,alpha_d=1,d_var=5,beta_theta=1,theta=3,alpha_0=2,beta_0=0.3){
      #g<- (alpha_d+alpha_theta)*log(d_var+beta_theta)-log(gamma(alpha_d+alpha_theta))+alpha_theta*log(theta)+(alpha_0-1)*log(alpha_theta)-alpha_theta*beta_0
  #}
  
  
  #bat <- uni.slice(x0=2, g, w=1, m=Inf, lower=0, upper=+Inf, gx0=NULL)
  #return(bat)
  
  #}else{
    #d_var_2<-d[j-1,]
    
    #g <- function(alpha_theta,alpha_d=1,d_var_2=5,beta_theta=1,theta=3,alpha_0=2,beta_0=0.3){
      #g<-(alpha_d+alpha_theta)*log(d_var_2+beta_theta)-log(gamma(alpha_d+alpha_theta))+alpha_theta*log(theta)+(alpha_0-1)*log(alpha_theta)-alpha_theta*beta_0
    #}
    
    #bat<-uni.slice(x0=2, g, w=1, m=Inf, lower=0, upper=+Inf, gx0=NULL)
    #return(bat)
  #}
#}
