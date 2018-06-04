#bayesmpp_demo.R

rm(list = ls())
source("./Code/slice.sampler/uni.slice.R")

datos<-read.csv("./Diabetes.Marked/diabetes_marked_datos_sim.csv", header = TRUE)

source("./Code/bayesmpp.R")

# Prior
alpha_0 = 2
beta_0 = 0.3

# MCMC sampler
M.sim <- 10

bayesmpp_out <- bayesmpp(datos,M.sim,alpha_0=2,beta_0=0.3)
  
###


