#
#	Implementacion del Gibbs sampler
#
#
bayesmpp <- function(...,datos, M){
	# datops - arreglo de nx3 (col1-individuo, col2-duraciones, col3-costos)
 	# M - numero de simulacion del gibbs sampler
	#
	
	n <- nrow(datos)
	#	Inicio-Repositorios
	#	Parametros
	alpha_d_rep <- array(NaN,M,1)
  alpha_theta_rep <- array(NaN,M,1)
  beta_theta_rep <- array(NaN,M,1)
  alpha_gamma_rep <- array(NaN,M,1)
  beta_gamma_rep <- array(NaN,M,1)

	# Latentes
	theta_rep <- matrix(NaN,n,M)
	gamma_rep <- matrix(NaN,n,M)

	#	Valores iniciales
	alpha_d_sim <- 1 # (o cualquiera)
	alpha_theta_sim <- 1 # (o cualquiera)
	beta_theta_sim <- 1
	alpha_gamma_sim <- 1
	beta_gamma_sim <- 1

	#	Gibbs sampler, per se
	m <- 1	
	for(m in 1:M){
		#	Simular de la final completa de los parametros
		alpha_d_sim <- bayesmpp_alpha_d(alpha_d,alpha_theta, d,beta_theta, theta, alpha_0,beta_0, alpha_d_sim) 
	
		alpha_theta_sim <- bayesmpp_alpha_theta(..., alpha_theta_sim)

		beta_theta_sim <- bayesmpp_beta_theta(...,beta_theta_sim)
	
		alpha_gamma_sim <- bayesmpp_alpha_gamma(...,alpha_gamma_sim)
		
		beta_gamma_sim <- rgamma(1, alpha_0,(1/gamma_sim)+beta_0)

		
		#Simular la final completa de variables latentes
		theta_sim <- rgamma(1, alpha_d_sim + alpha_theta_sim, d + beta_theta_sim)
		
		gamma_sim <- bayesmpp_gamma_sim(...,gamma_sim)
		
		
		#	Almacenamos en el repositorio
		alpha_d_rep[m,1] <- alpha_d_sim
		alpha_theta_rep[m,1] <- alpha_theta_sim
		beta_theta_rep[m,1] <- beta_theta_sim
		alpha_gamma_rep[m,1] <- alpha_gamma_sim
		beta_gamma_rep[m,1] <- beta_gamma_sim
		
		theta_rep[n,m] <- theta_sim
    gamma_rep[n,m] <- gamma_sim
    
	}

	# Output
	bayesmpp_out <- list(alpha_d_rep,alpha_theta_rep,beta_theta_rep,alpha_gamma_rep,beta_gamma_rep,theta_rep,gamma_rep)
	return(bayesmpp_out)
}
