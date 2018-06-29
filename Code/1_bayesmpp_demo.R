#bayesmpp_demo.R

rm(list = ls())
library(ggplot2)
library(gridExtra)


datos<-read.csv("./Diabetes.Marked/datos_agregados.csv", header = TRUE)
#head(datos)

source("./Code/bayesmpp.R")
source('./Code/slice.sampler/uni.slice.R')

# Prior
alpha_0 <- 2
beta_0 <- 0.3

# MCMC sampler
M.sim <- 10

bayesmpp_out <- bayesmpp(datos,M.sim,alpha_0=2,beta_0=0.3)
  
save(datos,
     alpha_0,
     beta_0,
     M.sim,
     bayesmpp_out,
     file="./Diabetes.Marked/bayesmpp_out.RData")

adi<- bayesmpp_out

analisis_estimacion<-function(z){
  g1 <- ggplot(data.frame(est=1:length(z),z=z),aes(x=est,y=z))+
    geom_line() 
  g2 <- ggplot(data.frame(est=1:length(z),z=z),aes(x=est,
                                                   y=cumsum(z)/(1:length(z))))+
    geom_line()
  grid.arrange(g1,g2,ncol=2,nrow=2)
}

#---Lo que queremos es que tome cada uno de los parámetros de bayesmpp_out, para hacer estas
#gráficas y determinar los valores estimados de los parámetros y las variables latentes.
analisis_estimacion(datos['alpha_d'][[1]])
mean(datos['alpha_d'][[1]])

analisis_estimacion(datos['alpha_theta'][[1]])
mean(datos['alpha_theta'][[1]])

analisis_estimacion(datos['beta_theta'][[1]])
mean(datos['beta_theta'][[1]])

analisis_estimacion(datos['alpha_gamma'][[1]])
mean(datos['alpha_gamma'][[1]])

analisis_estimacion(datos['beta_gamma'][[1]])
mean(datos['beta_gamma'][[1]])