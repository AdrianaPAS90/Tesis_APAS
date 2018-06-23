gamma2_slice<-function(punto_inicial2, 
                       alpha_gamma_sim,beta_gamma_sim,
                       d_aux_ij,c_aux_ij,d_aux_ijm,c_aux_ijm){

  #a
  #-(d[los.no.unos,"duration"]+  d[los.no.unos-1,"duration"] + alpha_gamma_sim)*log(gamma_sim[los.no.unos.c,1,1])
  
  #b
  #(beta_gamma_sim/gamma_sim[los.no.unos.c,1,1])+ ((c[los.no.unos.c,"costs"])/gamma_sim[los.no.unos.c,1,1])^(d[los.no.unos-1,"duration"]) +((c[los.no.unos.c,"costs"]/gamma_sim[los.no.unos.c,1,1])^(d[los.no.unos,"duration"]))
  
  uni_gamma2 <- uni.slice(punto_inicial2, function(gamma2) {-((d_aux_ij + d_aux_ijm + alpha_gamma_sim+1)*log(gamma2))
                          -( (beta_gamma_sim/gamma2) + ((c_aux_ijm)/gamma2)^(d_aux_ijm) + (c_aux_ij/gamma2)^(d_aux_ij) )},
                          w=1, m=Inf, lower=-Inf, upper=+Inf)
  
  
  gamma_sim_2 <- attr(uni_gamma2,"uni.slice.evals")
  
  # Output
  return(gamma_sim_2)
}