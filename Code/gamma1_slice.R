gamma1_slice<-function(punto_inicial,
                       alpha_gamma_sim,beta_gamma_sim,
                       d_aux_ij,c_aux_ij){
#a
#a<- (-(d[los.unos[1],"duration"]+alpha_gamma_sim)*log(gamma_sim[los.unos.c[1],1,1]))

#b
#b<-(beta_gamma_sim/gamma_sim[los.unos.c[1],1,1])+((c[los.unos.c[1],"costs"]/gamma_sim[los.unos.c[1],1,1])^(d[los.unos[1],"duration"]))

uni_gamma1 <- uni.slice(punto_inicial, function(gamma1){-(beta_gamma_sim/gamma1)+((c_aux_ij/gamma1)^(d_aux_ij))-(d_aux_ij+alpha_gamma_sim)*log(gamma1)},
                       w=1, m=10, lower=0, upper=10)

gamma_sim_1 <- attr(uni_gamma1,"uni.slice.evals")

# Output
return(gamma_sim_1)
}