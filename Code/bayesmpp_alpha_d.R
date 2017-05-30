bayesmpp_alpha_d <- function(alpha_d,alpha_theta, d,beta_theta, theta, alpha_0,beta_0, alpha_d_sim){
	#
	# Simula datos de la distribucion condicional completa de "alpha_d"
	#
	
	g(alpha_d,alpha_theta, d,beta_theta, theta, alpha_0,beta_0) <- (alpha_d+alpha_theta)*ln(d+beta_theta)-ln(gamma(alpha_d+alpha_theta))+alpha_d*(ln(theta))+(alpha_0-1)*ln(alpha_d)-(alpha_d*beta_0) 
	  
	alpha_d_sim <- uni.slice(alpha_d_sim, g, w=1, m=Inf, lower=-Inf, upper=+Inf, gx0=NULL)

}
