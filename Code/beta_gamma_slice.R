beta_gamma_slice<-function(beta_gamma_sim, gamma_sim,
                           alpha_0,beta_0){
  
  #a
  (alpha_0-1)*log(beta_gamma_sim)
  
  #b
  beta_gamma_sim*(beta_0+sum(1/gamma_sim[,1,1]))
  
  uni_beta_gamma<-uni.slice(beta_gamma_sim, function(beta_gamma) (alpha_0-1)*log(beta_gamma)
                            -beta_gamma*(beta_0+sum(1/gamma_sim[,1,1])),
                            w=1, m=Inf, lower=0, upper=+Inf)
  
  beta_gamma_sim<- attr(uni_beta_gamma,"uni.slice.evals")
  
  #Output
  return(beta_gamma_sim)
}