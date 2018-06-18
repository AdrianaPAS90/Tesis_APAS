bayesmpp <- function(datos,M.sim,alpha_0=2,beta_0=0.3){
  #
  # datos - Matriz de dimension 
  #         contador	paciente	num.cambio	duracion	costo
  # M.sim - Numero de simulaciones MCMC
  # alpha_0 - Prior shape parameter for Gamma distribution
  # beta_0 - Prior scale parameter for Gamma distribution
  # 
  
  # Preliminar
  source("./Code/slice.sampler/uni.slice.R")  
  source('./Code/alpha_d_slice.R')
  source('./Code/alpha_theta_slice.R')
  source('./Code/beta_theta_slice.R')
  source('./Code/alpha_gamma_slice.R')
  source('./Code/beta_gamma_slice.R')
  source('./Code/theta_ij_slice.R')

  # Numero de pacientes
  N.paciente <- length(unique(datos$paciente))
  
  # Observaciones correspondientes a pacientes
  N.observaciones <- aggregate(datos[,c("paciente","num.cambio")],
                               by=list(datos$paciente),
                               FUN=max)
  dim(N.observaciones)
  colnames(N.observaciones) <- c("id","paciente","num.cambios")

  dim.datos <- dim(datos)
  
  # --- Repositorios
  
  #	Parametros
  alpha_d_rep <- array(NaN,M.sim)
  alpha_theta_rep <- array(NaN,M.sim)
  beta_theta_rep <- array(NaN,M.sim)
  alpha_gamma_rep <- array(NaN,M.sim)
  beta_gamma_rep <- array(NaN,M.sim)
  
  # Latentes
  thetagamma_rep <- array(NaN,dim=c(dim.datos[1],(dim.datos[2]-1),M.sim))
  #dim(thetagamma_rep)

  #	--- Valores iniciales
  alpha_d_sim <- 1
  alpha_theta_sim <- 1
  beta_theta_sim <- 1
  alpha_gamma_sim <- 1
  beta_gamma_sim <- 1
  
  theta_sim <- array(1,dim=c(dim.datos[1],1,1))
  gamma_sim <- array(1,dim=c(dim.datos[1],1,1))
  
  #head(datos)
  
  d <- datos[,c("paciente","num.cambio","duration")]
  c <- datos[,c("paciente","num.cambio","costs")]
  
  #head(d)

  #	--- Gibbs sampler
  m <- 1	
  for(m in 1:M.sim){
    # Parametros
    alpha_d_sim <- alpha_d_slice( alpha_theta_sim,alpha_d_sim,beta_theta_sim, theta_sim,
                                  alpha_0,beta_0,
                                  d,N.observaciones) 
    
    alpha_theta_sim <- alpha_theta_slice(alpha_d_sim, alpha_theta_sim,beta_theta_sim, theta_sim,
                                         alpha_0,beta_0,
                                         d,N.observaciones)
    
    beta_theta_sim <- beta_theta_slice(alpha_d_sim, alpha_theta_sim, beta_theta_sim, theta_sim,
                                       alpha_0, beta_0,
                                       d,N.observaciones)
    
    alpha_gamma_sim <- alpha_gamma_slice(alpha_gamma_sim, gamma_sim,
                                         alpha_0,beta_0)
      
    beta_gamma_sim <- beta_gamma_slice(beta_gamma_sim, gamma_sim,
                                       alpha_0,beta_0)
    
    # Latentes
    theta_sim <- theta_ij_slice(alpha_d_sim,alpha_theta_sim,beta_theta_sim,d)#theta_sim
    gamma_sim <- gamma_ij_slice (alpha_gamma_sim, beta_gamma_sim, d, c)
    
    #	Almacenamos en el repositorio los parametros.
    alpha_d_rep[m] <- alpha_d_sim
    alpha_theta_rep[m] <- alpha_theta_sim
    beta_theta_rep[m] <- beta_theta_sim
    alpha_gamma_rep[m] <- alpha_gamma_sim
    beta_gamma_rep[m] <- beta_gamma_sim
    
    thetagamma_rep[,3,m] <- theta_sim$theta_ij_sim
    thetagamma_rep[,4,m] <- gamma_sim$gamma_ij_sim
  }

  # --- Output
  output <- list(alpha_d_rep,
                alpha_theta_rep,
                beta_theta_rep,
                alpha_gamma_rep,
                beta_gamma_rep,
                thetagamma_rep)

  return(output)
}
