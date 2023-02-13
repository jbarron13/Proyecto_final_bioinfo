# Script Final #
modelando <- function(){
  library(deSolve)
  library(jpeg)
  p1 <- readline(prompt = "¿El modelo deseado es un SI, SIS, SIR, SIRS, SEIR, SEIRS? : " )
  p1 <- as.character(p1)
  p2 <- readline(prompt = "¿El modelo lleva demografía? En caso de llevar, contestar con T o  TRUE, si no es el caso, contestar con F o False :  ")
  p2 <- as.logical(p2)
  if(p1 == "SI" & p2 == T){  ## LISTO GRF
    i1 <-readJPEG("image_model_2/SIP.jpeg",native=TRUE) ;par(mai= c(.2,.2,.2,.2)) ;plot(0:5,0:5, type="n",ann=FALSE,axes=FALSE) ; rasterImage(i1,-0.2,-1.5 ,5.35 ,6.7)
    SI <- function(t,state,parameters){
      with(as.list(c(state, parameters)), {
        dS <- mu*N - beta*S*I - mu1*S
        dI <- beta*S*I - mu2*I
        list(c(dS, dI))
      })
    }
    mu <- readline(prompt = "¿Cuál es la tasa de crecimiento de los nacimientos? Por favor, ingrese el número únicamente:" )
    mu <- as.numeric(mu)
    N <- readline(prompt = "¿Cuál es el valor de N? Por favor, ingrese el número únicamente:")
    N <- as.numeric(N)
    S <- readline(prompt = "¿Cuátos susceptibles hay? Por favor, ingrese el número únicamente:" )
    S <- as.numeric(S)
    mu1 <- readline(prompt = "¿Cuál es la tasa de muerte de los susceptibles? Por favor, ingrese el número únicamente:"  )
    mu1 <- as.numeric(mu1)
    beta <- readline(prompt = "¿Cuál es el valor de beta? Por favor, ingrese el número únicamente:")
    beta <- as.numeric(beta)
    I <- readline(prompt = "¿Cuántos infectados hay? Por favor, ingrese el número únicamente:")
    I <- as.numeric(I)
    mu2 <- readline(prompt = "¿Cuál es la tasa de muerte de los infectados? Por favor, ingrese el número únicamente:" )
    mu2 <- as.numeric(mu2)
    ti <- readline(prompt = "¿Cuál es el tiempo total del modelo? Por favor, ingrese el número únicamente:" )
    ti <- as.numeric(ti)
    parametro2 <- c(beta= beta, mu=mu, mu1=mu1, mu2=mu2)
    daprincipio <- c(S=S, I=I)
    taaim <- seq(0, ti, by = 0.001)
    outo <- ode(daprincipio, taaim, SI, parametro2)
    matplot(outo[ , 1], outo[ , 2:3], type = "l", xlab = "Tiempo", ylab = "Población",
            main = "Modelo SI D.", lwd = 2) ; legend("topright", c("Susceptible", "Infectado"), col = 1:3,
                                                     lty=1:3,cex=0.5)
    
  }else if(p1 == "SI" & p2 == F){
    i2 <-readJPEG("image_model_2/SI.jpeg",native=TRUE) ;par(mai= c(.2,.2,.2,.2)) ;plot(0:5,0:5, type="n",ann=FALSE,axes=FALSE) ; rasterImage(i2,-0.2,-0.6 ,5.2 ,5.4)
    
    SI <- function(t,state,parameters){
      with(as.list(c(state, parameters)), {
        dS <- - beta*S*I
        dI <- beta*S*I
        list(c(dS, dI))
      })
    }
    S <- readline(prompt = "¿Cuántos susceptibles hay? Por favor, ingrese el número únicamente:")
    S <- as.numeric(S)
    beta <- readline(prompt = "¿Cuál es el valor de beta? Por favor, ingrese el número únicamente:")
    beta <- as.numeric(beta)
    I <- readline(prompt = "¿Cuántos infectados hay? Por favor, ingrese el número únicamente:")
    I <- as.numeric(I)
    ti <- readline(prompt = "¿Cuál es el tiempo total del modelo? Por favor, ingrese el número únicamente:")
    ti <- as.numeric(ti)
    parametro2 <- c(beta=beta)
    daprincipio <- c(S=S, I=I)
    taaim <- seq(0, ti, by = 0.001)
    outo <- ode(daprincipio, taaim, SI, parametro2)
    matplot(outo[ , 1], outo[ , 2:3], type = "l", xlab = "Tiempo", ylab = "Población",
            main = " Modelo SI", lwd = 2) ; legend("topright", c("Susceptible", "Infectado"), 
                                                   col = 1:3,lty=1:3,cex=0.5)
    
  }else if(p1 == "SIS" & p2 == T){
    i3 <-readJPEG("image_model_2/SISP.jpeg",native=TRUE) ;par(mai= c(.2,.2,.2,.2)) ;plot(0:5,0:5, type="n",ann=FALSE,axes=FALSE) ; rasterImage(i3,-0.2,-0.6 ,5.2 ,5.8)
    SIS <- function(t,state,parameters){
      with(as.list(c(state, parameters)), {
        dS <- mu*N - beta*S*I + gama*I - mu1*S
        dI <- beta*S*I - gama*I - mu2*I
        list(c(dS, dI))
      })
    }
    mu <- readline(prompt = "¿Cuál es la tasa de crecimiento de los nacimientos? Por favor, ingrese el número únicamente:" )
    mu <- as.numeric(mu)
    N <- readline(prompt = "¿Cuál es el valor de N? Por favor, ingrese el número únicamente:")
    N <- as.numeric(N)
    S <- readline(prompt = "¿Cuántos susceptibles hay? Por favor, ingrese el número únicamente:")
    S <- as.numeric(S)
    mu1 <- readline(prompt = "¿Cuál es la tasa de muerte de los susceptibles? Por favor, ingrese el número únicamente:")
    mu1 <- as.numeric(mu1)
    beta <- readline(prompt = "¿Cuál es el valor de beta? Por favor, ingrese el número únicamente:")
    beta <- as.numeric(beta)
    I <- readline(prompt = "¿Cuántos infectados hay? Por favor, ingrese el número únicamente:")
    I <- as.numeric(I)
    mu2 <- readline(prompt = "¿Cuál es la tasa de muerte de los infectados? Por favor, ingrese el número únicamente:")
    mu2 <- as.numeric(mu2)
    gama <- readline(prompt = "¿Cuál es el valor de gama? Por favor, ingrese el número únicamente:")
    gama <- as.numeric(gama)
    ti <- readline(prompt = "¿Cuál es el tiempo total del modelo? Por favor, ingrese el número únicamente:")
    ti <- as.numeric(ti)
    
    parametro2 <- c(beta=beta, gama=gama, mu=mu, N=N, mu1=mu1, mu2=mu2)
    daprincipio <- c(S=S, I=I)
    taaim <- seq(0, ti, by = 0.001)
    outo <- ode(daprincipio, taaim, SIS, parametro2)
    matplot(outo[ , 1], outo[ , 2:3], type = "l", xlab = "Tiempo", ylab = "Población",
            main = " Modelo SIS D.", lwd = 2) ; legend("topright", c("Susceptible", "Infectado"), 
                                                       col = 1:3,lty=1:3,cex=0.5)
  }else if(p1 == "SIS" & p2 == F){
    i4 <-readJPEG("image_model_2/SIS.jpeg",native=TRUE) ;par(mai= c(.2,.2,.2,.2)) ;plot(0:5,0:5, type="n",ann=FALSE,axes=FALSE) ; rasterImage(i4,-0.2,-0.4 ,5.2 ,5.4)
    SIS <- function(t,state,parameters){
      with(as.list(c(state, parameters)), {
        dS <- - beta*S*I + gama*I
        dI <- beta*S*I - gama*I
        list(c(dS, dI))
      })
    }
    S <- readline(prompt = "¿Cuántos susceptibles hay? Por favor, ingrese el número únicamente:")
    S <- as.numeric(S)
    beta <- readline(prompt = "¿Cuál es el valor de beta? Por favor, ingrese el número únicamente:")
    beta <- as.numeric(beta)
    I <- readline(prompt = "¿Cuántos infectados hay? Por favor, ingrese el número únicamente:")
    I <- as.numeric(I)
    gama <- readline(prompt = "¿Cuál es el valor de gama? Por favor, ingrese el número únicamente:" )
    gama <- as.numeric(gama)
    ti <- readline(prompt = "¿Cuál es el tiempo total del modelo? Por favor, ingrese el número únicamente:")
    ti <- as.numeric(ti)
    parametro2 <- c(beta=beta, gama=gama)
    daprincipio <- c(S=S, I=I)
    taaim <- seq(0, ti, by = 0.001)
    outo <- ode(daprincipio, taaim, SIS, parametro2)
    matplot(outo[ , 1], outo[ , 2:3], type = "l", xlab = "Tiempo", ylab = "Población",
            main = " Modelo SIS", lwd = 2) ; legend("topright", c("Susceptible", "Infectado"), 
                                                    col = 1:3,lty=1:3,cex=0.5)
    
  }else if(p1 == "SIR" & p2 == T){
    i5 <-readJPEG("image_model_2/SIRP.jpeg",native=TRUE) ;par(mai= c(.2,.2,.2,.2)) ;plot(0:5,0:5, type="n",ann=FALSE,axes=FALSE) ; rasterImage(i5,-0.2,-0.6 ,5.2 ,5.8)
    SIR <- function(t,state,parameters){
      with(as.list(c(state, parameters)), {
        dS <- mu*N - beta*S*I - mu1*S
        dI <- beta*S*I - gama*I - mu2*I
        dR <- gama*I - mu3*R
        list(c(dS, dI, dR))
      })
    }
    mu <- readline(prompt = "¿Cuál es la tasa de crecimiento de los nacimientos? Por favor, ingrese el número únicamente:")
    mu <- as.numeric(mu)
    N <- readline(prompt = "¿Cuál es el valor de N? Por favor, ingrese el número únicamente:")
    N <- as.numeric(N)
    S <- readline(prompt = "¿Cuántos susceptibles hay? Por favor, ingrese el número únicamente:")
    S <- as.numeric(S)
    mu1 <- readline(prompt = "¿Cuál es la tasa de muerte de los susceptibles? Por favor, ingrese el número únicamente:")
    mu1 <- as.numeric(mu1)
    beta <- readline(prompt = "¿Cuál es el valor de beta? Por favor, ingrese el número únicamente:")
    beta <- as.numeric(beta)
    I <- readline(prompt = "¿Cuántos infectados hay? Por favor, ingrese el número únicamente:")
    I <- as.numeric(I)
    mu2 <- readline(prompt = "¿Cuál es la tasa de muerte de los infectados? Por favor, ingrese el número únicamente:")
    mu2 <- as.numeric(mu2)
    gama <- readline(prompt = "¿Cuál es el valor de gama? Por favor, ingrese el número únicamente:")
    gama <- as.numeric(gama)
    R <- readline(prompt = "¿Cuántos recuperados hay? Por favor, ingrese el número únicamente:")
    R <- as.numeric(R)
    mu3 <- readline(prompt ="¿Cuál es la tasa de muerte de los recuperados? Por favor, ingrese el número únicamente:")
    mu3 <- as.numeric(mu3)
    ti <- readline(prompt = "¿Cuál es el tiempo total del modelo? Por favor, ingrese el número únicamente:")
    ti <- as.numeric(ti)
    parametro2 <- c(beta=beta, gama=gama, mu=mu, N=N, mu1=mu1, mu2=mu2, mu3=mu3)
    daprincipio <- c(S=S, I=I, R=R)
    taaim <- seq(0, ti, by = 0.001)
    outo <- ode(daprincipio, taaim, SIR, parametro2)
    matplot(outo[ , 1], outo[ , 2:4], type = "l", xlab = "Tiempo", ylab = "Población",
            main = " Modelo SIR", lwd = 2) ; legend("topright", c("Susceptible", "Infectado", "Recuperado"), 
                                                    col = 1:4,lty=1:4,cex=0.5)
  }else if(p1 == "SIR" & p2 == F){
    i6 <-readJPEG("image_model_2/SIR.jpeg",native=TRUE) ;par(mai= c(.2,.2,.2,.2)) ;plot(0:5,0:5, type="n",ann=FALSE,axes=FALSE) ; rasterImage(i6,-0.2,-1.9 ,5.2 ,5.5)
    SIR <- function(t,state,parameters){
      with(as.list(c(state, parameters)), {
        dS <- - beta*S*I 
        dI <- beta*S*I - gama*I
        dR <- gama*I
        list(c(dS, dI, dR))
      })
    }
    S <- readline(prompt = "¿Cuántos susceptibles hay? Por favor, ingrese el número únicamente:")
    S <- as.numeric(S)
    beta <- readline(prompt = "¿Cuál es el valor de beta? Por favor, ingrese el número únicamente:")
    beta <- as.numeric(beta)
    I <- readline(prompt = "¿Cuántos infectados hay? Por favor, ingrese el número únicamente:")
    I <- as.numeric(I)
    gama <- readline(prompt = "¿Cuál es el valor de gama? Por favor, ingrese el número únicamente:")
    gama <- as.numeric(gama)
    R <- readline(prompt = "¿Cuántos recuperados hay? Por favor, ingrese el número únicamente:")
    R <- as.numeric(R)
    ti <- readline(prompt = "¿Cuál es el tiempo total del modelo? Por favor, ingrese el número únicamente:")
    ti <- as.numeric(ti)
    parametro2 <- c(beta=beta, gama=gama)
    daprincipio <- c(S=S, I=I, R=R)
    taaim <- seq(0, ti, by = 0.001)
    outo <- ode(daprincipio, taaim, SIR, parametro2)
    matplot(outo[ , 1], outo[ , 2:4], type = "l", xlab = "Tiempo", ylab = "Población",
            main = " Modelo SIR", lwd = 2) ; legend("topright", c("Susceptible", "Infectado","Recuperado"), 
                                                    col = 1:4,lty=1:4,cex=0.5)
  }else if(p1 == "SIRS" & p2 == T){
    i7 <-readJPEG("image_model_2/SIRSP.jpeg",native=TRUE) ;par(mai= c(.2,.2,.2,.2)) ;plot(0:5,0:5, type="n",ann=FALSE,axes=FALSE) ; rasterImage(i7,-0.35,-0.6 ,5.3 ,5.8)
    SIRS <- function(t,state,parameters){
      with(as.list(c(state, parameters)), {
        dS <- mu*N - beta*S*I + delta*R - mu1*S
        dI <- beta*S*I - gama*I - mu2*I
        dR <- gama*I - delta*R - mu3*R
        list(c(dS, dI, dR))
      })
    }
    mu <- readline(prompt = "¿Cuál es la tasa de crecimiento de los nacimientos?" )
    mu <- as.numeric(mu)
    N <- readline(prompt = "¿Cuál es el valor de N? Por favor, ingrese el número únicamente:")
    N <- as.numeric(N)
    S <- readline(prompt = "¿Cuántos susceptibles hay? Por favor, ingrese el número únicamente:")
    S <- as.numeric(S)
    mu1 <- readline(prompt = "¿Cuál es la tasa de muerte de los susceptibles? Por favor, ingrese el número únicamente:")
    mu1 <- as.numeric(mu1)
    beta <- readline(prompt = "¿Cuál es el valor de beta? Por favor, ingrese el número únicamente:")
    beta <- as.numeric(beta)
    I <- readline(prompt = "¿Cuántos infectados hay? Por favor, ingrese el número únicamente:")
    I <- as.numeric(I)
    mu2 <- readline(prompt = "¿Cuál es la tasa de muerte de los infectados? Por favor, ingrese el número únicamente:")
    mu2 <- as.numeric(mu2)
    gama <- readline(prompt = "¿Cuál es el valor de gama? Por favor, ingrese el número únicamente:")
    gama <- as.numeric(gama)
    R <- readline(prompt = "¿Cuántos recuperados hay? Por favor, ingrese el número únicamente:")
    R <- as.numeric(R)
    mu3 <- readline(prompt = "¿Cuál es la tasa de muerte de los recuperados? Por favor, ingrese el número únicamente:")
    mu3 <- as.numeric(mu3)
    delta <- readline(prompt = "¿Cuál es el valor de delta? Por favor, ingrese el número únicamente:")
    delta <- as.numeric(delta)
    ti <- readline(prompt = "¿Cuál es el tiempo total del modelo? Por favor, ingrese el número únicamente:" )
    ti <- as.numeric(ti)
    parametro2 <- c(beta=beta, gama=gama, mu=mu, N=N, mu1=mu1, mu2=mu2, mu3=mu3)
    daprincipio <- c(S=S, I=I, R=R)
    taaim <- seq(0, ti, by = 0.001)
    outo <- ode(daprincipio, taaim, SIRS, parametro2)
    matplot(outo[ , 1], outo[ , 2:4], type = "l", xlab = "Tiempo", ylab = "Población",
            main = " Modelo SIRS D.", lwd = 2) ; legend("topright", c("Susceptible", "Infectado","Recuperado"), 
                                                        col = 1:4,lty=1:4,cex=0.5)
  }else if(p1 == "SIRS" & p2 == F){
    i8 <-readJPEG("image_model_2/SIRS.jpeg",native=TRUE) ;par(mai= c(.2,.2,.2,.2)) ;plot(0:5,0:5, type="n",ann=FALSE,axes=FALSE) ; rasterImage(i8,-0.9,-.8 ,5.9 ,5.5)
    SIRS <- function(t,state,parameters){
      with(as.list(c(state, parameters)), {
        dS <- - beta*S*I + delta*R
        dI <- beta*S*I - gama*I
        dR <- gama*I - delta*R
        list(c(dS, dI, dR))
      })
    }
    S <- readline(prompt = "¿Cuántos susceptibles hay? Por favor, ingrese el número únicamente:")
    S <- as.numeric(S)
    beta <- readline(prompt = "¿Cuál es el valor de beta? Por favor, ingrese el número únicamente:")
    beta <- as.numeric(beta)
    I <- readline(prompt = "¿Cuántos infectados hay? Por favor, ingrese el número únicamente:" )
    I <- as.numeric(I)
    gama <- readline(prompt = "¿Cuál es el valor de gama? Por favor, ingrese el número únicamente:")
    gama <- as.numeric(gama)
    R <- readline(prompt = "¿Cuántos recuperados hay? Por favor, ingrese el número únicamente:")
    R <- as.numeric(R)
    delta <- readline(prompt = "¿Cuál es el valor de delta? Por favor, ingrese el número únicamente:")
    delta <- as.numeric(delta)
    ti <- readline(prompt = "¿Cuál es el tiempo total del modelo? Por favor, ingrese el número únicamente:")
    ti <- as.numeric(ti)
    parametro2 <- c(beta=beta, gama=gama, delta=delta)
    daprincipio <- c(S=S, I=I, R=R)
    taaim <- seq(0, ti, by = 0.001)
    outo <- ode(daprincipio, taaim, SIRS, parametro2)
    matplot(outo[ , 1], outo[ , 2:4], type = "l", xlab = "Tiempo", ylab = "Población",
            main = " Modelo SIRS", lwd = 2) ; legend("topright", c("Susceptible", "Infectado","Recuperado"), 
                                                     col = 1:4,lty=1:4,cex=0.5)
    
  }else if(p1 == "SEIR" & p2 == T){
    i9 <-readJPEG("image_model_2/SEIRP.jpeg",native=TRUE) ;par(mai= c(.2,.2,.2,.2)) ;plot(0:5,0:5, type="n",ann=FALSE,axes=FALSE) ; rasterImage(i9,-0.2,-0.6 ,5.2 ,5.5)
    SEIR <- function(t,state,parameters){
      with(as.list(c(state, parameters)), {
        dS <- mu*N - beta*S*I - mu1*S
        dE <- beta*S*I - delta*I - mu2*E
        dI <- delta*I - gama*R - mu3*I
        dR <- gama*R -  mu4*R
        list(c(dS, dE, dI, dR))
      })
    }
    mu <- readline(prompt = "¿Cuál es la tasa de crecimiento de los nacimientos?" )
    mu <- as.numeric(mu)
    N <- readline(prompt = "¿Cuál es el valor de N? Por favor, ingrese el número únicamente:")
    N <- as.numeric(N)
    S <- readline(prompt = "¿Cuántos susceptibles hay? Por favor, ingrese el número únicamente:")
    S <- as.numeric(S)
    mu1 <- readline(prompt = "¿Cuál es la tasa de muerte de los susceptibles? Por favor, ingrese el número únicamente:")
    mu1 <- as.numeric(mu1)
    beta <- readline(prompt = "¿Cuál es el valor de beta? Por favor, ingrese el número únicamente:")
    beta <- as.numeric(beta)
    E <- readline(prompt = "¿Cuántos expuestos hay? Por favor, ingrese el número únicamente:")
    E <- as.numeric(E)
    mu2 <- readline(prompt = "¿Cuál es la tasa de muerte de los expuestos? Por favor, ingrese el número únicamente:")
    mu2 <- as.numeric(mu2)
    delta <- readline(prompt = "¿Cuál es el valor de delta? Por favor, ingrese el número únicamente:")
    delta <- as.numeric(delta)
    I <- readline(prompt = "¿Cuántos infectados hay? Por favor, ingrese el número únicamente:")
    I <- as.numeric(I)
    mu3 <- readline(prompt = "¿Cuál es la tasa de muerte de los infectados? Por favor, ingrese el número únicamente:")
    mu3 <- as.numeric(mu3)
    gama <- readline(prompt = "¿Cuál es el valor de gama? Por favor, ingrese el número únicamente:")
    gama <- as.numeric(gama)
    R <- readline(prompt = "¿Cuántos recuperados hay? Por favor, ingrese el número únicamente:")
    R <- as.numeric(R)
    mu4 <- readline(prompt = "¿Cuál es la tasa de muerte de los recuperados? Por favor, ingrese el número únicamente:")
    mu4 <- as.numeric(mu4)
    ti <- readline(prompt = "¿Cuál es el tiempo total del modelo? Por favor, ingrese el número únicamente:")
    ti <- as.numeric(ti)
    parametro2 <- c(beta=beta, delta=delta, gama=gama, mu=mu, N=N, mu1=mu1, mu2=mu2, mu3=mu3, mu4=mu4)
    daprincipio <- c(S=S, E=E, I=I, R=R)
    taaim <- seq(0, ti, by = 0.001)
    outo <- ode(daprincipio, taaim, SEIR, parametro2)
    matplot(outo[ , 1], outo[ , 2:5], type = "l", xlab = "Tiempo", ylab = "Población",
            main = " Modelo SEIR", lwd = 2) ; legend("topright", c("Susceptible", "Expuesto","Infectado","Recuperado"), 
                                                     col = 1:5,lty=1:5,cex=0.5)
  }else if(p1 == "SEIR" & p2 == F){
    i10 <-readJPEG("image_model_2/SEIR.jpeg",native=TRUE) ;par(mai= c(.2,.2,.2,.2)) ;plot(0:5,0:5, type="n",ann=FALSE,axes=FALSE) ; rasterImage(i10,-0.2,-1.9 ,5.2 ,5.5)
    SEIR <- function(t,state,parameters){
      with(as.list(c(state, parameters)), {
        dS <- - beta*S*I 
        dE <- beta*S*I - delta*E
        dI <- delta*E - gama*I
        dR <- gama*I
        list(c(dS, dE, dI, dR))
      })
    }
    S <- readline(prompt = "¿Cuántos susceptibles hay? Por favor, ingrese el número únicamente:")
    S <- as.numeric(S)
    beta <- readline(prompt = "¿Cuál es el valor de beta? Por favor, ingrese el número únicamente:")
    beta <- as.numeric(beta)
    E <- readline(prompt = "¿Cuántos expuestos hay? Por favor, ingrese el número únicamente:")
    E <- as.numeric(E)
    delta <- readline(prompt = "¿Cuál es el valor de delta? Por favor, ingrese el número únicamente:" )
    delta <- as.numeric(delta)
    I <- readline(prompt = "¿Cuántos infectados hay? Por favor, ingrese el número únicamente:")
    I <- as.numeric(I)
    gama <- readline(prompt = "¿Cuál es el valor de gama? Por favor, ingrese el número únicamente:")
    gama <- as.numeric(gama)
    R <- readline(prompt = "¿Cuántos recuperados hay? Por favor, ingrese el número únicamente:")
    R <- as.numeric(R)
    ti <- readline(prompt = "¿Cuál es el tiempo total del modelo? Por favor, ingrese el número únicamente:")
    ti <- as.numeric(ti)
    parametro2 <- c(beta=beta, delta=delta, gama=gama)
    daprincipio <- c(S=S, E=E, I=I, R=R)
    taaim <- seq(0, ti, by = 0.001)
    outo <- ode(daprincipio, taaim, SEIR, parametro2)
    matplot(outo[ , 1], outo[ , 2:5], type = "l", xlab = "Tiempo", ylab = "Población",
            main = " Modelo SEIR", lwd = 2) ; legend("topright", c("Susceptible", "Expuesto","Infectado","Recuperado"), 
                                                     col = 1:5,lty=1:5,cex=0.5)
  }else if(p1 == "SEIRS" & p2 == T){
    i11 <-readJPEG("image_model_2/SEIRSP.jpeg",native=TRUE) ;par(mai= c(.2,.2,.2,.2)) ;plot(0:5,0:5, type="n",ann=FALSE,axes=FALSE) ; rasterImage(i11,-0.2,-0.6 ,5.2 ,5.6)
    SEIRS <- function(t,state,parameters){
      with(as.list(c(state, parameters)), {
        dS <- mu*N - beta*S*I + ro*R - mu1*S
        dE <- beta*S*I - delta*I - mu2*E
        dI <- delta*I - gama*R - mu3*I
        dR <- gama*R - ro*R - mu4*R
        list(c(dS, dE, dI, dR))
      })
    }
    mu <- readline(prompt = "¿Cuál es la tasa de crecimiento de los nacimientos? Por favor, ingrese el número únicamente:")
    mu <- as.numeric(mu)
    N <- readline(prompt = "¿Cuál es el valor de N? Por favor, ingrese el número únicamente:")
    N <- as.numeric(N)
    S <- readline(prompt = "¿Cuántos susceptibles hay? Por favor, ingrese el número únicamente:")
    S <- as.numeric(S)
    mu1 <- readline(prompt = "¿Cuál es la tasa de muerte de los susceptibles? Por favor, ingrese el número únicamente:")
    mu1 <- as.numeric(mu1)
    beta <- readline(prompt = "¿Cuál es el valor de beta? Por favor, ingrese el número únicamente:")
    beta <- as.numeric(beta)
    E <- readline(prompt = "¿Cuántos expuestos hay? Por favor, ingrese el número únicamente:")
    E <- as.numeric(E)
    mu2 <- readline(prompt = "¿Cuál es la tasa de muerte de los expuestos? Por favor, ingrese el número únicamente:")
    mu2 <- as.numeric(mu2)
    delta <- readline(prompt = "¿Cuál es el valor de delta? Por favor, ingrese el número únicamente:")
    delta <- as.numeric(delta)
    I <- readline(prompt = "¿Cuántos infectados hay? Por favor, ingrese el número únicamente:")
    I <- as.numeric(I)
    mu3 <- readline(prompt = "¿Cuál es la tasa de muerte de los infectados? Por favor, ingrese el número únicamente:")
    mu3 <- as.numeric(mu3)
    gama <- readline(prompt = "¿Cuál es el valor de gama? Por favor, ingrese el número únicamente:")
    gama <- as.numeric(gama)
    R <- readline(prompt = "¿Cuántos recuperados hay? Por favor, ingrese el número únicamente:" )
    R <- as.numeric(R)
    mu4 <- readline(prompt = "¿Cuál es la tasa de muerte de los recuperados? Por favor, ingrese el número únicamente:")
    mu4 <- as.numeric(mu4)
    ro <- readline(prompt = "¿Cuál es el valor de ro? Por favor, ingrese el número únicamente:")
    ro <- as.numeric(ro)
    ti <- readline(prompt = "¿Cuál es el tiempo total del modelo? Por favor, ingrese el número únicamente:")
    ti <- as.numeric(ti)
    parametro2 <- c(beta=beta, delta=delta, gama=gama, mu=mu, N=N, mu1=mu1, mu2=mu2, mu3=mu3, mu4=mu4)
    daprincipio <- c(S=S, E=E, I=I, R=R)
    taaim <- seq(0, ti, by = 0.001)
    outo <- ode(daprincipio, taaim, SEIRS, parametro2)
    matplot(outo[ , 1], outo[ , 2:5], type = "l", xlab = "Tiempo", ylab = "Población",
            main = " Modelo SEIRS", lwd = 2) ; legend("topright", c("Susceptible", "Expuesto","Infectado","Recuperado"), 
                                                      col = 1:5,lty=1:5,cex=0.5)
  }else if(p1 == "SEIRS" & p2 == F){
    i12 <-readJPEG("image_model_2/SEIRS.jpeg",native=TRUE) ;par(mai= c(.2,.2,.2,.2)) ;plot(0:5,0:5, type="n",ann=FALSE,axes=FALSE) ; rasterImage(i12,-0.2,-0.6 ,5.2 ,5.3)
    SEIRS <- function(t,state,parameters){
      with(as.list(c(state, parameters)), {
        dS <- - beta*S*I + ro*R
        dE <- beta*S*I - delta*I
        dI <- delta*I - gama*R
        dR <- gama*R - ro*R
        list(c(dS, dE, dI, dR))
      })
    }
    S <- readline(prompt = "¿Cuántos susceptibles hay? Por favor, ingrese el número únicamente:" )
    S <- as.numeric(S)
    beta <- readline(prompt = "¿Cuál es el valor de beta? Por favor, ingrese el número únicamente:")
    beta <- as.numeric(beta)
    E <- readline(prompt = "¿Cuántos expuestos hay? Por favor, ingrese el número únicamente:")
    E <- as.numeric(E)
    delta <- readline(prompt = "¿Cuál es el valor de delta? Por favor, ingrese el número únicamente:")
    delta <- as.numeric(delta)
    I <- readline(prompt = "¿Cuántos infectados hay? Por favor, ingrese el número únicamente:")
    I <- as.numeric(I)
    gama <- readline(prompt = "¿Cuál es el valor de gama? Por favor, ingrese el número únicamente:")
    gama <- as.numeric(gama)
    ro <- readline(prompt = "¿Cuál es el valor de ro? Por favor, ingrese el número únicamente:")
    ro <- as.numeric(ro)
    R <- readline(prompt = "¿Cuántos recuperados hay? Por favor, ingrese el número únicamente:")
    R <- as.numeric(R)
    ti <- readline(prompt = "¿Cuál es el tiempo total del modelo? Por favor, ingrese el número únicamente:")
    ti <- as.numeric(ti)
    parametro2 <- c(beta=beta, delta=delta, gama=gama, mu=mu, N=N, mu1=mu1, mu2=mu2, mu3=mu3, mu4=mu4)
    daprincipio <- c(S=S, E=E, I=I, R=R)
    taaim <- seq(0, ti, by = 0.001)
    outo <- ode(daprincipio, taaim, SEIRS, parametro2)
    matplot(outo[ , 1], outo[ , 2:5], type = "l", xlab = "Tiempo", ylab = "Población",
            main = " Modelo SEIRS", lwd = 2) ; legend("topright", c("Susceptible", "Expuesto","Infectado","Recuperado"), 
                                                      col = 1:5,lty=1:5,cex=0.5)
  }
  
}
modelando()