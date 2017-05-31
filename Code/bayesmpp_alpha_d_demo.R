# 
#	Demo de "bayesmpp_alpha_d.R"
#

rm(list=ls())

source("/run/media/jmartineov/JC.ITAM/JCMO.Trabajo/@Estudiantes/Adriana Perez-Arciniega/Tesis_APAS/Code/slice.sampler/uni.slice.R")

alpha_d <- 1
alpha_theta <- 2
d <- 5
beta_theta <- 1
theta <- 3
alpha_0 = 2
beta_0 = 0.3 
alpha_d_sim = 2

g(alpha_d,alpha_theta, d,beta_theta, theta, alpha_0,beta_0) <- (alpha_d+alpha_theta)*ln(d+beta_theta)-ln(gamma(alpha_d+alpha_theta))+alpha_d*(ln(theta))+(alpha_0-1)*ln(alpha_d)-(alpha_d*beta_0) 

alpha_d_sim <- uni.slice(alpha_d_sim, g, w=1, m=Inf, lower=-Inf, upper=+Inf, gx0=NULL)


bayesmpp_alpha_d(alpha_d,alpha_theta, d,beta_theta, theta, alpha_0,beta_0, alpha_d_sim)
