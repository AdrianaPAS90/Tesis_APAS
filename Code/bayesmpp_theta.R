bayesmpp_theta <- function(d, alpha_d_sim, alpha_theta_sim, beta_theta_sim){
    #
    #   input:
    #   d   --  array dim=c(n,1) -- n=# obs.
    #   alpha_d_sim -- scalar
    #   alpha_theta_sim -- scalar
    #   beta_theta_sim -- scalar
    #
    #   output:
    #   theta_sim -- array dim=c(n,1)
    #
    
    #n <- nrow(d)
    #theta_sim <- array(NaN, n)

    #j <- 1
    #for(j in 1:n){
      if (j==1){
        theta_sim <- rgamma(1, alpha_d_sim + alpha_theta_sim, d[j,] + beta_theta_sim)
      } else {
        theta_sim <- rgamma(1, (2*alpha_d_sim) + alpha_theta_sim, d[j-1,] + d[j,]+ beta_theta_sim)
      }
    #} 
    return(theta_sim)
}
