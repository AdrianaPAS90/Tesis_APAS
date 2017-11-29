#Lectura del archivo base
setwd("C:/Users/SONY/Documents/GitHub/Tesis_APAS/Diabetes.Marked")
tabla_inicial_NHS<-read.csv("diabetes_marked_datos_sim.csv", header = TRUE)

#Creacion de la base de datos
df_NHS<-data.frame(tabla_inicial_NHS)
paciente<-as.factor(df_NHS$paciente)
n_pacientes<-500

#Calculo de duraciones
dur<-tapply(df_NHS$presc.code, df_NHS$paciente, rle)

i<-1
duraciones<-data.frame("paciente"=rep(i,length(dur[[i]][1]$lengths)),"tratamiento"=dur[[i]][2],"duracion"=dur[[i]][1])

for(i in 2:n_pacientes){
  duraciones<-rbind(duraciones,data.frame("paciente"=rep(i,length(dur[[i]][1]$lengths)),"tratamiento"=dur[[i]][2],"duracion"=dur[[i]][1]))
}
