alpha_gamma_slice<-function(alpha_gamma_sim, gamma_sim,
                            alpha_0,beta_0){
  
  #a
  -(alpha_gamma_sim)*sum(log(gamma_sim[,1,1]))
  
  #b
  (alpha_0-1)*log(alpha_gamma_sim)
  
  #c
  (alpha_gamma_sim*beta_0)
  
  uni_alpha_gamma <- uni.slice(alpha_gamma_sim, function(alpha_gamma) -(alpha_gamma)*sum(log(gamma_sim[,1,1]))
                              + (alpha_0-1)*log(alpha_gamma)
                              -(alpha_gamma*beta_0),
                              w=1, m=Inf, lower=0, upper=+Inf)
  
  alpha_gamma_sim<- attr(uni_alpha_gamma,"uni.slice.evals")
  
  #Output
  return(alpha_gamma_sim)
}