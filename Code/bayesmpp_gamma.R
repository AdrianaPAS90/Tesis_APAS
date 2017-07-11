bayesmpp_gamma <- function(d, c, alpha_gamma_sim, beta_gamma_sim){
  #
  # input:
  #   d --
  #   c --
  #   alpha_gamma_sim --
  #   beta_gamma_sim --
  #
  # ouput:
  #   gamma_sim -- array dim=c(n,1)
  #

  n <- nrow(c)

  gamma_sim_new <- Nan * gamma_sim

  j <- 1
  for(j in 1:n){
    if(j == 1){
      d_var <- d[j,1]
      c_var <- c[j,1]
      g <- function(gamma_var, d_var, c_var, alpha_gamma_sim, beta_gamma_sim){
        return(-(d_var+alpha_gamma_sim+1)*log(gamma_var)-((beta_gamma_sim/gamma_var)+(c_var/gamma_var)^d))
      }
      x0 <- gamma_sim[j,] 
      gamma_sim_new <- uni.slice(x0, g, w=1, m=Inf, lower=0, upper=+Inf, gx0=NULL) #????
    }else{
      d_var_1 <- d[j, ]
      d_var_2 <- d[j-1, ]
      c_var_1 <- c[j, ]
      c_var_2 <- c[j-1, ]
    g <- function(gamma_var, d_var, c_var, alpha_gamma_sim, beta_gamma_sim){
      # ? return(...)
    }
    x0 <- gamma_sim[j,] 
    gamma_sim_new <- uni.slice(x0, g, w=1, m=Inf, lower=0, upper=+Inf, gx0=NULL) #????
    }
  
  }
  return(gamma_sim_new)
}
