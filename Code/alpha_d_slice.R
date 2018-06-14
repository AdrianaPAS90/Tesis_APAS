alpha_d_slice <- function(alpha_theta_sim,alpha_d_sim,beta_theta_sim, theta_sim,
                          alpha_0,beta_0,
                          d,N.observaciones){
  #
  #
  #
  
  # (a)
  I <- max(d$paciente)
#  factor_a1 <- log(gamma(alpha_d_sim + alpha_theta_sim))
  n.sum <- sum(N.observaciones$num.cambios)
#  factor_a2 <- log(gamma(2*alpha_d_sim + alpha_theta_sim))
#  -(I*factor_a1+n.sum*factor_a2)
  
  # (b)
#  (alpha_0-1)*log(alpha_d_sim)+alpha_d_sim*beta_0
  
  los.unos <- which(d$num.cambio==1)
  los.no.unos <- which(d$num.cambio>=2)

  # (c)
#  (alpha_d_sim+alpha_theta_sim) * sum( log( d[los.unos,"duration"]+beta_theta_sim ) ) + 
#  (alpha_d_sim+alpha_theta_sim-1) * sum( log( theta_sim[los.unos,1,1] ) )
  
  # (d)
#  (2*alpha_d_sim+alpha_theta_sim) * sum( log( d[los.no.unos,"duration"] + d[los.no.unos-1,"duration"] + beta_theta_sim) ) + 
#  (alpha_d_sim+alpha_theta_sim-1) * sum( log( theta_sim[los.no.unos,1,1] ) )
  
  uni <- uni.slice(alpha_d_sim, function(alpha_d) -(I*log(gamma(alpha_d + alpha_theta_sim))+n.sum*log(gamma(2*alpha_d + alpha_theta_sim))) +
                     (alpha_0-1)*log(alpha_d)-alpha_d*beta_0 + 
                     (alpha_d+alpha_theta_sim) * sum( log( d[los.unos,"duration"]+beta_theta_sim ) ) + 
                     (alpha_d+alpha_theta_sim-1) * sum( log( theta_sim[los.unos,1,1] ) ) + 
                     (2*alpha_d+alpha_theta_sim) * sum( log( d[los.no.unos,"duration"] + d[los.no.unos-1,"duration"] + beta_theta_sim) ) + 
                     (alpha_d+alpha_theta_sim-1) * sum( log( theta_sim[los.no.unos,1,1] ) ),
            w=1, m=Inf, lower=0, upper=+Inf)
  alpha_d_sim <- attr(uni,"uni.slice.evals")
  
  # Output
  return(alpha_d_sim)
}