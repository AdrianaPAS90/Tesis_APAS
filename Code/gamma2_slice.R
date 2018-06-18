gamma2_slice<-function(punto_inicial2, alpha_gamma_sim,beta_gamma_sim,
                       d,c){
  los.unos.c <- which(c$num.cambio==1)
  los.unos <- which(d$num.cambio==1)
  los.no.unos <-which(d$num.cambio>=2)
  los.no.unos.c <- which(c$num.cambio>=2)
  
  #a
  #-(d[los.no.unos,"duration"]+  d[los.no.unos-1,"duration"] + alpha_gamma_sim)*log(gamma_sim[los.no.unos.c,1,1])
  
  #b
  #(beta_gamma_sim/gamma_sim[los.no.unos.c,1,1])+ ((c[los.no.unos.c,"costs"])/gamma_sim[los.no.unos.c,1,1])^(d[los.no.unos-1,"duration"]) +((c[los.no.unos.c,"costs"]/gamma_sim[los.no.unos.c,1,1])^(d[los.no.unos,"duration"]))
  
  uni_gamma2 <- uni.slice(punto_inicial2, function(gamma2) -(d[los.no.unos[i],"duration"]+  d[los.no.unos[i]-1,"duration"] + alpha_gamma_sim)*log(gamma2)
                          -(beta_gamma_sim/gamma2)+ ((c[los.no.unos.c[i],"costs"])/gamma2)^(d[los.no.unos[i]-1,"duration"]) +((c[los.no.unos.c[i],"costs"]/gamma2)^(d[los.no.unos[i],"duration"])),
                          w=1, m=Inf, lower=0, upper=+Inf)
  
  
  gamma_sim_2 <- attr(uni_gamma2,"uni.slice.evals")
  
  # Output
  return(gamma_sim_2)
}