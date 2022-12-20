#Preguntas a realizar

#¿Lleva demografía? Si o no
#¿Es con vectores? Si o no
#¿Es homógeneo? Si o no
#¿Es heterógeneo? SI o no

#Tipos de Modelo
#SI
#SIS
#SIR
#SIRS
#SEIR
#SEIRS

epi_model <- function(){
  library(deSolve)
  incio <- readline(prompt = "Hola, por favor conteste las siguiente preguntas para generar su modelo. Pulse enter para iniciar ")
  i1 <- readline(prompt = "¿Cuál de la siguientes opciones se adapta a sus datos? SI, SIS, SIR, SIRS, SEIR, SEIRS ")
  i1 <- as.character(i1)
  i2 <- readline(prompt = "¿El modelo lleva demografía? Conteste T o TRUE si lleva demografía, si no, conteste con F o FALSE si no lleva demografía ")
  i2 <- as.logical(i2)
  i3 <- readline(prompt = "¿El modelo lleva vectores? Conteste nuevamente con T o F ")
  i3 <- as.logical(i3)
  i4 <- readline(prompt = "¿El modelo es homógeneo? Conteste nuevamente con T o F ")
  i4 <- as.logical(i4)
  i5 <- readline(prompt = "¿El modelo es heterogéneo? Conteste nuevamente con T o F ")
  i5 <- as.logical(i5)
  
}

epi_model()

#Lo que llevo si jala, si no contestas con T o F la función se detiene


#SI DE PRUEBA
SI <- function(t,state,parameters){
  with(as.list(c(state, parameters)), {
    dS <- -beta*S*I + gama*I
    dI <- beta*S*I - gama*I
    list(c(dS, dI))
  })
}

parametro1 <- c(beta = 0.2, gama = 0.4)
principio <- c(S = 9, I = 1)
tiempo <- seq(c(0,20, by = 0.001))
out <- ode(principio, tiempo, SI, parametro1)

grafica <- matplot(out[ ,1], out[ ,2:8], type = "l", xlab = "Tiempo", 
                   main = "Modelo SI", lwd = 2)
legend("Topright", c("Susceptible", "Infectado"))

#Acomodo de las preguntas para hacer el modelo

SIp <- function () {
  p1 <- readline(prompt = "¿El modelo deseado es un SI?" )
p1 <- as.character(p1)
if (p1 == "SIS"){
  SIS <- function(t,state,parameters){
    with(as.list(c(state, parameters)), {
      dS <- -beta2*dapri*dapri1 + gama1*dapri1
      dI <- beta1*dapri*dapri1 - gama2*dapri1
      list(c(dS, dI))
    })
  }
  beta1 <- readline(prompt = "Cuál es el valor de beta?" )
  beta1 <- as.numeric(beta1)
  beta2 <- readline(prompt = "Cuál es el valor de beta?" )
  beta2 <- as.numeric(beta1)
  gama1 <- readline(prompt = "Cuál es el valor de gama?" )
  gama1 <- as.numeric(gama1)
  gama2 <- readline(prompt = "Cuál es el valor de gama?" )
  gama2 <- as.numeric(gama1)
  dapri <- readline(prompt = "Cuál es el valor de S?" )
  dapri <- as.numeric(dapri)
  dapri1 <- readline(prompt = "Cuál es el valor de I?" )
  dapri1 <- as.numeric(dapri1)
  parametro2 <- c(beta1, beta2, gama1, gama2)
  daprincipio <- c(dapri, dapri1)
  taaim <- seq(c(0, 20, by = 0.001))
  outo <- ode(daprincipio, taaim, SI, parametro2)
  return(outo)
}
}

#Todo corre bien pero al momento de hacer el cálculo arroja un error en "-beta"
#Ahora arroja un error al multiplicar beta*dapri
#OLVIDALO, YA QUEDÓ, es muy poco, pero es un inicio c´:

#Ecuaciones para los modelos sin poblacion
#SIR 
SIR <- function(t,state,parameters){
  with(as.list(c(state, parameters)), {
    dS <- -beta*S*I 
    dI <- beta*S*I - gama*I
    dR <- gama*I
    list(c(dS, dI, dR))
  })
}

#SIRS 
SIR <- function(t,state,parameters){
  with(as.list(c(state, parameters)), {
    dS <- -beta*S*I + delta*R
    dI <- beta*S*I - gama*I
    dR <- gama*I - delta*R
    list(c(dS, dI, dR))
  })
}

#SEIR 
SEIR <- function(t,state,parameters){
  with(as.list(c(state, parameters)), {
    dS <- -beta*S*I 
    dE <- beta*S*I - delta*I
    dI <- delta*I - gama*R
    dR <- gama*R
    list(c(dS, dE, dI, dR))
  })
}

#SEIRS 
SEIR <- function(t,state,parameters){
  with(as.list(c(state, parameters)), {
    dS <- -beta*S*I + ro*R
    dE <- beta*S*I - delta*I
    dI <- delta*I - gama*R
    dR <- gama*R - ro*R
    list(c(dS, dE, dI, dR))
  })
}

#Ecuaciones para los modelos con poblacion
#SIR 
SIR <- function(t,state,parameters){
  with(as.list(c(state, parameters)), {
    dS <- mu*N -beta*S*I - mu*S
    dI <- beta*S*I - gama*I - mu*I
    dR <- gama*I - mu*R
    list(c(dS, dI, dR))
  })
}

#SIRS 
SIR <- function(t,state,parameters){
  with(as.list(c(state, parameters)), {
    dS <- mu*N -beta*S*I + delta*R - mu*S
    dI <- beta*S*I - gama*I - mu*I
    dR <- gama*I - delta*R - mu*R
    list(c(dS, dI, dR))
  })
}

#SEIR 
SEIR <- function(t,state,parameters){
  with(as.list(c(state, parameters)), {
    dS <- mu*N -beta*S*I -mu*S 
    dE <- beta*S*I - delta*I - mu*E
    dI <- delta*I - gama*R - mu*I
    dR <- gama*R -  mu*R
    list(c(dS, dE, dI, dR))
  })
}

#SEIRS 
SEIR <- function(t,state,parameters){
  with(as.list(c(state, parameters)), {
    dS <- mu*N -beta*S*I + ro*R - mu*S
    dE <- beta*S*I - delta*I -mu*E
    dI <- delta*I - gama*R - mu*I
    dR <- gama*R - ro*R - mu*R
    list(c(dS, dE, dI, dR))
  })
}