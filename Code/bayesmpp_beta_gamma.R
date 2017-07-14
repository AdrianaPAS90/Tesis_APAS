bayesmpp_beta_gamma <- function(gamma_sim, alpha_0, beta_0){
  #
  #input:
  # gamma_sim <- escalar
  # alpha_0 <- parámetro fijo (escalar)
  # beta_0 <- parámetro fijo (escalar)
  #
  # output:
  # beta_gamma_sim <- escalar que va a entrar en un array (M,1)
  #
  beta_gamma_sim <- rgamma(1,  alpha_0, (1/gamma_sim) + beta_0)
  
  return(beta_gamma_sim)
}