gamma_ij_slice<-function(alpha_gamma_sim,beta_gamma_sim,d,c){
  
  
  #Variable latente de costos
  
  gamma_ij_sim <- c
  colnames(gamma_ij_sim) <- c("paciente","num.cambio","gamma_ij_sim")
  
  source('./Code/gamma1_slice.R')
  source('./Code/gamma2_slice.R')
  
  i <- 1
  for(i in 1:I){
    gamma_aux <- gamma_ij_sim[which(c$paciente==i),]
    n_i<-nrow(gamma_aux)
    
    c_aux <- c[which(c$paciente==i),]
    d_aux <- d[which(c$paciente==i),]
    
    n<-1
    for(n in 1:n_i){
      if(n==1){
        d_aux_ij <- d_aux[n,3]
        c_aux_ij <- c_aux[n,3]
        gamma_aux[n,"gamma_ij_sim"] <- gamma1_slice(gamma_aux[n,1,1],
                                                    alpha_gamma_sim,beta_gamma_sim,
                                                    d_aux_ij,c_aux_ij)
      }else{
        d_aux_ij <- d_aux[n,3]
        c_aux_ij <- c_aux[n,3]
        d_aux_ijm <- d_aux[(n-1),3]
        c_aux_ijm <- c_aux[(n-1),3]
        # Por revisar
        gamma_aux[n,"gamma_ij_sim"] <- gamma2_slice(gamma_aux[n,1,1], 
                                                    alpha_gamma_sim,beta_gamma_sim,
                                                    d_aux_ij,c_aux_ij,d_aux_ijm,c_aux_ijm)
      }
    }
    
    gamma_ij_sim[which(c$paciente==i),] <- gamma_aux
  }
  #Output
  return(gamma_ij_sim)
}