#Lectura del archivo base
setwd("C:/Users/SONY/Documents/GitHub/Tesis_APAS/Diabetes.Marked")
tabla_inicial_NHS<-read.csv("diabetes_marked_datos_sim.csv", header = TRUE)

#Creacion de la base de datos
df_NHS<-data.frame(tabla_inicial_NHS)
df_NHS$paciente<-as.factor(df_NHS$paciente)
n<-nrow(df_NHS)

#Calculo de los costos agregados
aux<-array(NaN,n)
aux[1]<-df_NHS$presc.cost[1]
for(i in 2:n){
  if(df_NHS$paciente[i] == df_NHS$paciente[i-1]){
    aux[i]<-aux[i-1] + df_NHS$presc.cost[i]
  }
  else{
    aux[i]<-df_NHS$presc.cost[i]
  }
}

c<-cbind(df_NHS, aux)

#Calculo de duraciones
df_NHS$paciente<-as.factor(c$paciente)
dur<-tapply(c$presc.code, df_NHS$paciente, rle)

i<-1
duraciones<-data.frame("paciente"=rep(i,length(dur[[i]][1]$lengths)),"tratamiento"=dur[[i]][2],"duracion"=dur[[i]][1])

for(i in 2:n_pacientes){
  duraciones<-rbind(duraciones,data.frame("paciente"=rep(i,length(dur[[i]][1]$lengths)),"tratamiento"=dur[[i]][2],"duracion"=dur[[i]][1]))
}

#Calculo de costos agregados
#cost<-array(NaN,dim(dur))
#ca<-as.double(c$aux)
#cost[1]<-ca[1]
#j<-1

#cost<-tapply(c$aux, tapply(c$presc.code, df_NHS$paciente, rle), rle)
#Calculo de costos agregados
#cost<-tapply(df_NHS$presc.cost, df_NHS$paciente, cumsum)
