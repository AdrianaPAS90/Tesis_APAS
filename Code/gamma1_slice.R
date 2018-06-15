gamma1_slice<-function(alpha_gamma_sim,beta_gamma_sim,
                       d,c){
los.unos.c <- which(c$num.cambio==1)
los.unos <- which(d$num.cambio==1)

#a
#-(d[los.unos,"duration"]+alpha_gamma_sim)*log(gamma_sim[los.unos.c,1,1])

#b
#(beta_gamma_sim/gamma_sim[los.unos.c,1,1])+((c[los.unos.c,"costs"]/gamma_sim[los.unos.c,1,1])^(d[los.unos,"duration"]))

uni_gamma1 <- uni.slice(gamma_sim, function(gamma_i) -(beta_gamma_sim/gamma_i[los.unos.c,1,1])+((c[los.unos.c,"costs"]/gamma_i[los.unos.c,1,1])^(d[los.unos,"duration"]))
                       -(d[los.unos,"duration"]+alpha_gamma_sim)*log(gamma_i[los.unos.c,1,1]),
                       w=1, m=Inf, lower=0, upper=+Inf)

#g<-function(gamma_i){-(beta_gamma_sim/gamma_i[los.unos.c,1,1])+((c[los.unos.c,"costs"]/gamma_i[los.unos.c,1,1])^(d[los.unos,"duration"]))-(d[los.unos,"duration"]+alpha_gamma_sim)*log(gamma_i[los.unos.c,1,1])}

gamma_sim <- attr(uni_gamma1,"uni.slice.evals")

# Output
return(gamma_sim)
}