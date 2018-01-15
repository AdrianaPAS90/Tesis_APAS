#Lectura del archivo base
setwd("C:/Users/SONY/Documents/GitHub/Tesis_APAS/Diabetes.Marked")
rm(list=ls())
tabla_inicial_NHS<-read.csv("diabetes_marked_datos_sim_2.csv", header = TRUE)

#Creacion de la base de datos
df_NHS<-data.frame(tabla_inicial_NHS)
df_NHS$paciente<-as.factor(df_NHS$paciente)
n<-nrow(df_NHS)

head(df_NHS)
pac.num <- as.matrix(sort(unique(df_NHS$paciente)))
N <- as.numeric(pac.num[dim(pac.num)[1], ])
#N

n <- 1
for(n in 1:N){
  # Escaneamos pacientes
  indices_aux <- which(df_NHS$paciente==pac.num[n])
  data_aux <- df_NHS[indices_aux, ] 
  #data_aux
  T_n <- dim(data_aux)[1]
  #T_n
  
  # Creacion
  data_agg_aux <-  data_aux[1,]
  t <- 2
  count <- 1
  duration_aux <- 1
  costs_aux <- 0
  data_agg_aux[1,"duration"] <- 1
  data_agg_aux[1,"costs"] <- data_agg_aux[1,"presc.cost"]
  #data_agg_aux
  for(t in 2:T_n){
    if(data_aux[t,"presc.code"]==data_aux[(t-1),"presc.code"]){
      data_agg_aux[count,"duration"] <- duration_aux + 1
      #data_agg_aux
      data_agg_aux[count,"costs"] <- data_agg_aux[count,"costs"] + data_aux[t,"presc.cost"]
      #data_agg_aux
    }else if(data_aux[t,"presc.code"]!=data_aux[(t-1),"presc.code"]){
      duration_aux <- 1
      costs_aux <- 0
      count <- count + 1
      data_agg_aux[count,c("contador","paciente","num.obs","presc.code","presc.cost")] <- data_aux[t,c("contador","paciente","num.obs","presc.code","presc.cost")]
      #data_agg_aux
      data_agg_aux[count,"duration"] <- duration_aux
      #data_agg_aux
      data_agg_aux[count,"costs"] <- costs_aux + data_aux[t,"presc.cost"]
      #data_agg_aux
    }
    t <- t+1
  }
  data_agg_aux
  if(n==1){
    data_agg <- data_agg_aux
  }else{
    data_agg <- rbind(data_agg,data_agg_aux)
  }
}
data_agg
dim(data_agg)
write.csv(data_agg,"datos_agregados.csv")
