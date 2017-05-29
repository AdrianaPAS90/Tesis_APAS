bayesmpp_alpha_d <- function(..., alpha_d_sim){
	#
	# Simula datos de la distribucion condicional completa de "alpha_d"
	#
	
	g(alpha, ...) <- ... # Kernel

	alpha_d_sim <- uni.slice(alpha_d_sim, g, w=1, m=Inf, lower=-Inf, upper=+Inf, gx0=NULL)

}
