#
#	EST-46114:		Inferencia Bayesiana en Alta Dimension (Maestria en Ciencia de Datos)
#	Autor: 			Juan~Carlos Martinez Ovando
#	Email:			juan.martinez.ovando@itam.mx
#					jc.martinez.ovando@gmail.com
#	
#	Gibbs sampler
#	

rm(list = ls())
rutawork <- 'C:/JCMO.Academia/@Cursos/2016-I_Inferencia Bayesiana en Alta Dimension/_data&code/Gibbs_sampler/'
#rutawork <- '/media/jcmo/ROCA\ ADATA/JCMO.Academia/@Cursos/2016-I_Inferencia\ Bayesiana\ en\ Alta Dimension/_data&code/Gibbs_sampler/'
 
#	----------------------------------------------------
#		Exemplo: Distribución Normal-Gamma
#	----------------------------------------------------
gibbs_normalgamma <- function(M=50000,thin=1000){
    #	Repositorio
	repositorio <- matrix(NA, ncol=2, nrow=M)
    #	Valores iniciales de la cadena
	x <- 1
    y <- 1
    #	Iteraciones
	m <- 1
	for(m in 1:M) {
		j <- 1
        for (j in 1:thin) {
            x <- rgamma(1,3,y*y+4)
            y <- rnorm(1,1/(x+1),1/sqrt(2*x+2))
        }
        repositorio[m,] <- c(x,y)
    }
	#	Resultado
    names(repositorio) <- c("x","y")
    return(repositorio)
 }

#	Exemplo
M <- 10000
thin <- 100
gibbs_normalgamma_out <- gibbs_normalgamma(M,thin)

# Histograma de "x"
hist(gibbs_normalgamma_out[,1],M/100)

# Histograma de "x"
hist(gibbs_normalgamma_out[,2],M/100)

# Trayectorias de "x"
plot(gibbs_normalgamma_out[,1],type='l')

# Trayectorias de "y"
plot(gibbs_normalgamma_out[,2],type='l')

# Medias ergódicas de "x"
plot(as.matrix(cumsum(gibbs_normalgamma_out[,1]))/as.matrix(c(1:M)))

# Medias ergódicas de "y"
plot(as.matrix(cumsum(gibbs_normalgamma_out[,2]))/as.matrix(c(1:M)))

#	Gráficas de diagnóstico
par(mfrow=c(3,2))
	#	Dispersión
	plot(gibbs_normalgamma_out,col=1:M)
	#	Trayectorias conjuntas
	plot(gibbs_normalgamma_out,type="l")
	#	Trayectorias individuales
	plot(ts(gibbs_normalgamma_out[,1]))
	plot(ts(gibbs_normalgamma_out[,2]))
	#	Histogramas
	hist(gibbs_normalgamma_out[,1],40)
	hist(gibbs_normalgamma_out[,2],40)
par(mfrow=c(1,1))  

#	Gráficas de diagnóstico (autocorrelación)
par(mfrow=c(3,2))
	#	Trayectorias individuales
	plot(ts(gibbs_normalgamma_out[,1]))
	plot(ts(gibbs_normalgamma_out[,2]))
	#	Trayectorias individuales
	acf(ts(gibbs_normalgamma_out[,1]))
	acf(ts(gibbs_normalgamma_out[,2]))
	# Medias ergódicas de "x"
	plot(as.matrix(cumsum(gibbs_normalgamma_out[,1]))/as.matrix(c(1:M)))
	# Medias ergódicas de "y"
	plot(as.matrix(cumsum(gibbs_normalgamma_out[,2]))/as.matrix(c(1:M)))
par(mfrow=c(1,1))  

#	----------------------------------------------------
#		Exemplo: Distribucion Normal Bivariada
#	----------------------------------------------------
gibbs_bigaussian <- function(M=10000, rho=0.98){
    #	Repositorio
	repositorio <- matrix(NA, ncol=2, nrow=M)
    #	Valores iniciales
	x <- 0
    y <- 0
    repositorio[1, ] <- c(x, y)
	#	Iteraciones
	m <- 2
	for(m in 2:M){
        x <- rnorm(1, rho * y, sqrt(1 - rho^2))
        y <- rnorm(1, rho * x, sqrt(1 - rho^2))
        repositorio[m, ] <- c(x, y)
      }
	#	Resultado
    names(repositorio) <- c("x1","x2")
    return(repositorio)
  }

#	Ejemplo
M <- 10000
rho <- 0
gibbs_bigaussian_out <- gibbs_bigaussian(M,rho)

#	Gráficas de diagnóstico
par(mfrow=c(3,2))
	#	Dispersión
	plot(gibbs_bigaussian_out,col=1:M)
	#	Trayectorias conjuntas
	plot(gibbs_bigaussian_out,type="l")
	#	Trayectorias individuales
	plot(ts(gibbs_bigaussian_out[,1]))
	plot(ts(gibbs_bigaussian_out[,2]))
	#	Histogramas
	hist(gibbs_bigaussian_out[,1],40)
	hist(gibbs_bigaussian_out[,2],40)
par(mfrow=c(1,1))  

#	Gráficas de diagnóstico (autocorrelación)
par(mfrow=c(3,2))
	#	Trayectorias individuales
	plot(ts(gibbs_bigaussian_out[,1]))
	plot(ts(gibbs_bigaussian_out[,2]))
	#	Trayectorias individuales
	acf(ts(gibbs_bigaussian_out[,1]))
	acf(ts(gibbs_bigaussian_out[,2]))
	#	Histogramas
	hist(gibbs_bigaussian_out[,1],40)
	hist(gibbs_bigaussian_out[,2],40)
par(mfrow=c(1,1))  

#
#		FIN de "EST46114_GibbsSampler_Script.R"