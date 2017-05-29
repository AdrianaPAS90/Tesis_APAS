#
#	Ilustra "bayesmpp.R"
#

rm(list = ls())

#	Ejemplo
path <- "/run/media/jmartineov/JC.ITAM/JCMO.Trabajo/@Estudiantes/Adriana Perez-Arciniega/Tesis_APAS/Code"

#	Leer datos
datos <- read.csv(paste(path,"/archivo.csv",sep=""),HEAD=T)

# 	Cargar el codigo
source(paste(path,"/bayesmpp.R",sep=""))
source(paste(path,"/slice.sampler/uni.slice.R",sep=""))
...
...


#	Ejecutar
M <- 10
bayesmpp_out <- bayesmpp(...,datos, M)



