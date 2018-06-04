bayesmpp <- function(datos,M.sim,alpha_0=2,beta_0=0.3){
  #
  # datos - Matriz de dimension 
  #         contador	paciente	num.obs	presc.code	presc.cost
  # M.sim - Numero de simulaciones MCMC
  # alpha_0 - Prior shape parameter for Gamma distribution
  # beta_0 - Prior scale parameter for Gamma distribution
  # 
  
  # Preliminar
  source("./Code/slice.sampler/uni.slice.R")  
  
  N.paciente <- length(unique(datos$paciente))
  
  N.observaciones <- aggregate(datos[,c("paciente","num.obs")],
                               by=list(datos$paciente),
                               FUN=max)
  dim(N.observaciones)
  head(N.observaciones)

  dim.datos <- dim(datos)
  
  # Repositorios
  #	Parametros
  alpha_d_rep <- array(NaN,M.sim)
  alpha_theta_rep <- array(NaN,M.sim)
  beta_theta_rep <- array(NaN,M.sim)
  alpha_gamma_rep <- array(NaN,M.sim)
  beta_gamma_rep <- array(NaN,M.sim)
  
  # Latentes
  thetagamma_rep <- array(NaN,dim=c(dim.datos[1],dim.datos[2],M.sim))
  gamma_rep <- list()

  #	Valores iniciales
  alpha_d_sim <- 1
  alpha_theta_sim <- 1
  beta_theta_sim <- 1
  alpha_gamma_sim <- 1
  beta_gamma_sim <- 1
  
  theta_sim <- 1
  gamma_sim <- 1
  
  #	Gibbs sampler, per se
  m <- 1	
  for(m in 1:M.sim){
    
    #	Almacenamos en el repositorio los parametros.
    alpha_d_rep[m] <- alpha_d_sim
    alpha_theta_rep[m] <- alpha_theta_sim
    beta_theta_rep[m] <- beta_theta_sim
    alpha_gamma_rep[m] <- alpha_gamma_sim
    beta_gamma_rep[m] <- beta_gamma_sim
    
  }

    # Output
  ouput <- list(alpha_d_rep, alpha_theta_rep, 
                beta_theta_rep, alpha_gamma_rep,
                beta_gamma_rep)

  return(output)
  }
