#
#	Implementacion del Gibbs sampler
#
#
rm(list = ls())
source("Code/slice.sampler/uni.slice.R")
source("Code/bayesmpp_alpha_d_demo.R")
source("Code/bayesmpp_alpha_theta_demo.R")
source("Code/bayesmpp_beta_theta_demo.R")
source("Code/bayesmpp_alpha_gamma_demo.R")
source("Code/bayesmpp_gamma.R")

#datos<-read.csv...
#n <-nrow(datos)
bayesmpp <- function(alpha_0 =2,beta_0 = 0.3,  d, c, n, M){
	# datos - arreglo de nx3 (col1-individuo, col2-duraciones, col3-costos)
 	# M - numero de simulacion del gibbs sampler
	#

	#	Inicio-Repositorios
	#	Parametros
	alpha_d_rep <- array(NaN,M)
	alpha_theta_rep <- array(NaN,M)
	beta_theta_rep <- array(NaN,M)
	alpha_gamma_rep <- array(NaN,M)
	beta_gamma_rep <- array(NaN,M)

	# Latentes
	theta_rep <- matrix(NaN,M,n)
	gamma_rep <- matrix(NaN,M,n)

	#	Valores iniciales
	alpha_d_sim <- 1 # (o cualquiera)
	alpha_theta_sim <- 1 # (o cualquiera)
	beta_theta_sim <- 1
	alpha_gamma_sim <- 1
	beta_gamma_sim <- 1
	
	theta_sim <- 1
	gamma_sim <- 1

	#	Gibbs sampler, per se
	m <- 1	
	for(m in 1:M){
		#	Simular de la final completa de los parametros
		alpha_d_sim <- bayesmpp_alpha_d(x0, d) 
	
		alpha_theta_sim <- bayesmpp_alpha_theta(x0, alpha_d_sim, d)

		beta_theta_sim <- bayesmpp_beta_theta(x0, alpha_d_sim, alpha_theta_sim, d)
	
		alpha_gamma_sim <- bayesmpp_alpha_gamma(x0)
		
		beta_gamma_sim <- rgamma(1, alpha_0,(1/gamma_sim)+beta_0)
		
		#Simular la final completa de variables latentes
		theta_sim <- bayesmpp_theta(d, alpha_d_sim, alpha_theta_sim, beta_theta_sim)
		
		gamma_sim <- bayesmpp_gamma(alpha_gamma_sim, beta_gamma_sim, d, c)
		
		
		#	Almacenamos en el repositorio
		alpha_d_rep[m] <- alpha_d_sim
		alpha_theta_rep[m] <- alpha_theta_sim
		beta_theta_rep[m] <- beta_theta_sim
		alpha_gamma_rep[m] <- alpha_gamma_sim
		beta_gamma_rep[m] <- beta_gamma_sim
		
		j <- 1
		  for (j in 1:n){
		theta_rep[m,j] <- theta_sim
    gamma_rep[m,j] <- gamma_sim
		  }
		
	}

	# Output
	bayesmpp_out <- list(alpha_d_rep,alpha_theta_rep,beta_theta_rep,alpha_gamma_rep,beta_gamma_rep,theta_rep,gamma_rep)
	
}


