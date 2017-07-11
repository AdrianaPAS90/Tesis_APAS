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
    
    n <- nrow(d)
    theta_sim <- NaN * d

    j <- 1
    for(j in 1:n){
      thea_sim[j, ] <- rgamma(1, alpha_d_sim + alpha_theta_sim, d[j,] + beta_theta_sim)  
    } 
    return(theta_sim)
}
