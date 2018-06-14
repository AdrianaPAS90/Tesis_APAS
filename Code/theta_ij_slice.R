theta_ij_slice <- function(alpha_d_sim,alpha_theta_sim,beta_theta_sim,d){
  #
  # Latent variables simulation associated with durations
  #
  
  theta_ij_sim <- d
  colnames(theta_ij_sim) <- c("paciente","num.cambio","theta_ij_sim")
  
  I <- max(d$paciente)
  i<- 1
  for(i in 1:I){
    theta_aux <- theta_ij_sim[which(d$paciente==i),]
    d_aux <- d[which(d$paciente==i),]
    n_i <- nrow(theta_aux)
    n <- 1
    for(n in 1:n_i){
      if(n==1){
        theta_aux[n,"theta_ij_sim"] <- rgamma(1,alpha_d_sim+alpha_theta_sim,d_aux[n,"duration"]+beta_theta_sim)
      }else{
        theta_aux[n,"theta_ij_sim"] <- rgamma(1,2*alpha_d_sim+alpha_theta_sim,d_aux[n,"duration"]+d_aux[(n-1),"duration"]+beta_theta_sim)
        }
    }
    theta_ij_sim[which(d$paciente==i),] <- theta_aux
  }
  # Output
  return(theta_ij_sim)
}