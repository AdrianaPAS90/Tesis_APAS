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
	...

	# Latentes
	theta_rep <- array(NaN,M,n)
	...

	#	Valores iniciales
	alpha_d_sim <- 1 # (o cualquiera)
	alpha_theta_sim <- 1 # (o cualquiera)

	#	Gibbs sampler, per se
	m <- 1	
	for(m in 1:M){
		#	Simular de la final completa de "alpha_d"
		alpha_d_sim <- bayesmpp_alpha_d(..., alpha_theta_sim) 
		
		#	Simular de la final completa de "alpha_theta"
		alpha_theta_sim <- bayesmpp_alpha_theta(..., alpha_d_sim)

		...

		#	Almacenamos en el repositorio
		alpha_d_rep[m,1] <- alpha_d_sim
		alpha_theta_rep[m,1] <- alpha_theta_sim
		...

	}

	# Output
	bayesmpp_out <- list(alpha_d_rep,alpha_theta_rep, ...)
	return(bayesmpp_out)
}
