gamma_ij_slice<-function(alpha_gamma_sim,beta_gamma_sim,d,c){
  
  
  #Variable latente de costos
  
  gamma_ij_sim <- c
  colnames(gamma_ij_sim) <- c("paciente","num.cambio","gamma_ij_sim")
  
  source('./Code/gamma1_slice.R')
  source('./Code/gamma2_slice.R')
  

  for(i in 1:I){
    gamma_aux<-gamma_ij_sim[which(c$paciente==i),]
    c_aux<-c[which(c$paciente==i),]
    n_i<-nrow(gamma_aux)
    
    n<-1
    for(n in 1:n_i){
      if(n==1){
        gamma_aux[n,"gamma_ij_sim"] <- gamma1_slice(gamma_sim[los.unos.c[i],1,1],alpha_gamma_sim,beta_gamma_sim,d,c)
      }else{
        gamma_aux[n,"gamma_ij_sim"] <- gamma2_slice(gamma_sim[los.no.unos.c[i],1,1], alpha_gamma_sim,beta_gamma_sim,d,c)
      }
    }
    
    gamma_ij_sim[which(c$paciente==i),] <- gamma_aux
  }
  #Output
  return(gamma_ij_sim)
}