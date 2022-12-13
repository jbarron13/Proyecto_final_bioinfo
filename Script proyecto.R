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
