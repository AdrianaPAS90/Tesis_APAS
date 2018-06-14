beta_theta_slice<- function(alpha_d_sim, alpha_theta_sim, beta_theta_sim, theta_sim,
                            alpha_0, beta_0,
                            d,N.observaciones){
  
  los.unos <- which(d$num.cambio==1)
  los.no.unos <- which(d$num.cambio>=2)
  
  #a
  #((alpha_d_sim+alpha_theta_sim)*sum(log(d[los.unos,"duration"]+beta_theta_sim)))
  
  #b
  #((2*alpha_d_sim+alpha_theta_sim)*sum(log(d[los.no.unos,"duration"]+d[los.no.unos-1,"duration"]+beta_theta_sim)))
  
  #c
  #((alpha_0-1)*log(beta_theta_sim))-(beta_theta_sim*(beta_0+sum(theta_sim[,1,1])))
  
  
  uni_beta_theta<-uni.slice(beta_theta_sim, function(beta_theta) ((alpha_d_sim+alpha_theta_sim)*sum(log(d[los.unos,"duration"]+beta_theta)))
                          + ((2*alpha_d_sim+alpha_theta_sim)*sum(log(d[los.no.unos,"duration"]+d[los.no.unos-1,"duration"]+beta_theta)))
                          +((alpha_0-1)*log(beta_theta))-(beta_theta*(beta_0+sum(theta_sim[,1,1]))),
                          w=1, m=Inf, lower=0, upper=+Inf)
  
  
  beta_theta_sim <- attr(uni_beta_theta,"uni.slice.evals")
  
  # Output
  return(beta_theta_sim)
  
}