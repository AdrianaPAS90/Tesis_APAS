alpha_theta_slice<-function( alpha_d_sim, alpha_theta_sim, beta_theta_sim, theta_sim,
                            alpha_0,beta_0,
                            d,N.observaciones
                            ){
  
  I<-max(datos$paciente)
  n.sum<-sum(N.observaciones$num.cambios)
  los.unos <- which(d$num.cambio==1)
  los.no.unos <- which(d$num.cambio>=2)
  
  
  #a
  #(-I*log(gamma(alpha_d_sim+alpha_theta_sim)))-(n.sum*log(gamma(2*alpha_d_sim+alpha_theta_sim)))

  #b
  #((alpha_0-1)*log(alpha_theta_sim))-(alpha_theta_sim*beta_0)
  
  #c
  #((alpha_d_sim+alpha_theta_sim)*sum(log(d[los.unos,"duration"]+beta_theta_sim))+((2*alpha_d_sim+alpha_theta_sim)*sum(log(d[los.no.unos,"duration"]+ d[los.no.unos-1,"duration"]+ beta_theta_sim))
    #+(alpha_d_sim+alpha_theta_sim-1)*log(theta_sim[los.unos,1,1])) 
  
  #d
  #((2*alpha_d_sim+alpha_theta_sim)*sum(log(d[los.no.unos,"duration"]+ d[los.no.unos-1,"duration"]+ beta_theta_sim))+
  #(2*alpha_d_sim+alpha_theta_sim-1)*log(theta_sim[los.no.unos,1,1]))
  
  uni_alpha_theta<-uni.slice(alpha_theta_sim, function(alpha_theta) (-I*log(gamma(alpha_d_sim+alpha_theta)))-(n.sum*log(gamma(2*alpha_d_sim+alpha_theta)))
                            +((alpha_0-1)*log(alpha_theta))-(alpha_theta*beta_0)
                            +((alpha_d_sim+alpha_theta)*sum(log(d[los.unos,"duration"]+beta_theta_sim))
                            +(alpha_d_sim+alpha_theta-1)*log(theta_sim[los.unos,1,1])) 
                            +((2*alpha_d_sim+alpha_theta)*sum(log(d[los.no.unos,"duration"]+ d[los.no.unos-1,"duration"]+ beta_theta_sim))
                            +(2*alpha_d_sim+alpha_theta_sim-1)*log(theta_sim[los.no.unos,1,1])),
                            w=1, m=Inf, lower=0, upper=+Inf)
  
  alpha_theta_sim <- attr(uni_alpha_theta,"uni.slice.evals")
  
  # Output
  return(alpha_theta_sim)
  }