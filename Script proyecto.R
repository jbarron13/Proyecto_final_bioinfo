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


library(jpeg)
i1 <-readJPEG("image_model/prueba_1.jpeg",native=TRUE) ; plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE) ; rasterImage(i1,0,0,1,1)

#MODELO FINAL
#FALTA QUE SAQUE LAS IMÁGENES DE LOS MODELOS Y GRÁFICAS
modelando <- function(){
  library(deSolve)
  library(jpeg)
  p1 <- readline(prompt = "¿El modelo deseado es un SI, SIS, SIR, SIRS, SEIR, SEIRS? : " )
  p1 <- as.character(p1)
  p2 <- readline(prompt = "¿El modelo lleva demografía? 
                 En caso de llevar, contestar con T o  TRUE, si no es el caso, contestar con F o False :  ")
  p2 <- as.logical(p2)
  if(p1 == "SI" & p2 == T){  ## LISTO GRF
    i1 <-readJPEG("image_model/prueba_1.jpeg",native=TRUE) ; plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE) ; rasterImage(i1,0,0,1,1)
    SI <- function(t,state,parameters){
      with(as.list(c(state, parameters)), {
        dS <- mu*N - beta*S*I - mu1*S
        dI <- beta*S*I - mu2*I
        list(c(dS, dI))
      })
    }
    mu <- readline(prompt = "¿Cuál es la tasa de crecimiento de los nacimientos?" )
    mu <- as.numeric(mu)
    N <- readline(prompt = "¿Cuál es el valor de N?" )
    N <- as.numeric(N)
    beta <- readline(prompt = "¿Cuál es el valor de beta?" )
    beta <- as.numeric(beta)
    S <- readline(prompt = "¿Cuál es el valor de S?" )
    S <- as.numeric(S)
    mu1 <- readline(prompt = "¿Cuál es la tasa de muerte de los susceptibles?" )
    mu1 <- as.numeric(mu1)
    I <- readline(prompt = "¿Cuál es el valor de I?" )
    I <- as.numeric(I)
    mu2 <- readline(prompt = "¿Cuál es la tasa de muerte de los infectados?" )
    mu2 <- as.numeric(mu2)
    ti <- readline(prompt = "¿Cuál es el tiempo total del modelo? : " )
    ti <- as.numeric(ti)
    parametro2 <- c(beta= beta, mu=mu, mu1=mu1, mu2=mu2)
    daprincipio <- c(S=S, I=I)
    taaim <- seq(0, ti, by = 0.001)
    outo <- ode(daprincipio, taaim, SI, parametro2)
    matplot(outo[ , 1], outo[ , 2:3], type = "l", xlab = "Tiempo", ylab = "Población",
            main = "Modelo SI D.", lwd = 2) ; legend("topright", c("Susceptible", "Infectado"), col = 1:3,
                                                  lty=1:3,cex=0.5)
    
  }else if(p1 == "SI" & p2 == F){
    i1 <-readJPEG("image_model/prueba_1.jpeg",native=TRUE) ; plot(0:1,0:1
                                                                  ,type="n",ann=FALSE, axes=FALSE
                                                                  ) ; rasterImage(i1,0,0,1,1)
    
    SI <- function(t,state,parameters){
      with(as.list(c(state, parameters)), {
        dS <- - beta*S*I
        dI <- beta*S*I
        list(c(dS, dI))
      })
    }
    beta <- readline(prompt = "¿Cuál es el valor de beta?" )
    beta <- as.numeric(beta)
    S <- readline(prompt = "¿Cuál es el valor de S?" )
    S <- as.numeric(S)
    I <- readline(prompt = "¿Cuál es el valor de I?" )
    I <- as.numeric(I)
    ti <- readline(prompt = "¿Cuál es el tiempo total del modelo? : " )
    ti <- as.numeric(ti)
    parametro2 <- c(beta=beta)
    daprincipio <- c(S=S, I=I)
    taaim <- seq(0, ti, by = 0.001)
    outo <- ode(daprincipio, taaim, SI, parametro2)
    matplot(outo[ , 1], outo[ , 2:3], type = "l", xlab = "Tiempo", ylab = "Población",
            main = " Modelo SI", lwd = 2) ; legend("topright", c("Susceptible", "Infectado"), 
                                                   col = 1:3,lty=1:3,cex=0.5)
    
  }else if(p1 == "SIS" & p2 == T){
    SIS <- function(t,state,parameters){
      with(as.list(c(state, parameters)), {
        dS <- mu*N - beta*S*I + gama*I - mu1*S
        dI <- beta*S*I - gama*I - mu2*I
        list(c(dS, dI))
      })
    }
    mu <- readline(prompt = "¿Cuál es la tasa de crecimiento de los nacimientos?" )
    mu <- as.numeric(mu)
    N <- readline(prompt = "¿Cuál es el valor de N?" )
    N <- as.numeric(N)
    beta <- readline(prompt = "¿Cuál es el valor de beta?" )
    beta <- as.numeric(beta)
    gama <- readline(prompt = "¿Cuál es el valor de gama?" )
    gama <- as.numeric(gama)
    S <- readline(prompt = "¿Cuál es el valor de S?" )
    S <- as.numeric(S)
    mu1 <- readline(prompt = "¿Cuál es la tasa de muerte de los susceptibles?" )
    mu1 <- as.numeric(mu1)
    I <- readline(prompt = "¿Cuál es el valor de I?" )
    I <- as.numeric(I)
    mu2 <- readline(prompt = "¿Cuál es la tasa de muerte de los infectados?" )
    mu2 <- as.numeric(mu2)
    ti <- readline(prompt = "¿Cuál es el tiempo total del modelo? : " )
    ti <- as.numeric(ti)
    
    parametro2 <- c(beta=beta, gama=gama, mu=mu, N=N, mu1=mu1, mu2=mu2)
    daprincipio <- c(S=S, I=I)
    taaim <- seq(0, ti, by = 0.001)
    outo <- ode(daprincipio, taaim, SIS, parametro2)
    matplot(outo[ , 1], outo[ , 2:3], type = "l", xlab = "Tiempo", ylab = "Población",
            main = " Modelo SIS D.", lwd = 2) ; legend("topright", c("Susceptible", "Infectado"), 
                                                   col = 1:3,lty=1:3,cex=0.5)
  }else if(p1 == "SIS" & p2 == F){
    SIS <- function(t,state,parameters){
      with(as.list(c(state, parameters)), {
        dS <- - beta*S*I + gama*I
        dI <- beta*S*I - gama*I
        list(c(dS, dI))
      })
    }
    beta <- readline(prompt = "¿Cuál es el valor de beta?" )
    beta <- as.numeric(beta)
    gama <- readline(prompt = "¿Cuál es el valor de gama?" )
    gama <- as.numeric(gama)
    S <- readline(prompt = "¿Cuál es el valor de S?" )
    S <- as.numeric(S)
    I <- readline(prompt = "¿Cuál es el valor de I?" )
    I <- as.numeric(I)
    ti <- readline(prompt = "¿Cuál es el tiempo total del modelo? : " )
    ti <- as.numeric(ti)
    parametro2 <- c(beta=beta, gama=gama)
    daprincipio <- c(S=S, I=I)
    taaim <- seq(0, ti, by = 0.001)
    outo <- ode(daprincipio, taaim, SIS, parametro2)
    matplot(outo[ , 1], outo[ , 2:3], type = "l", xlab = "Tiempo", ylab = "Población",
            main = " Modelo SIS", lwd = 2) ; legend("topright", c("Susceptible", "Infectado"), 
                                                   col = 1:3,lty=1:3,cex=0.5)
    
  }else if(p1 == "SIR" & p2 == T){
    SIR <- function(t,state,parameters){
      with(as.list(c(state, parameters)), {
        dS <- mu*N - beta*S*I - mu1*S
        dI <- beta*S*I - gama*I - mu2*I
        dR <- gama*I - mu3*R
        list(c(dS, dI, dR))
      })
    }
    mu <- readline(prompt = "¿Cuál es la tasa de crecimiento de los nacimientos?" )
    mu <- as.numeric(mu)
    N <- readline(prompt = "¿Cuál es el valor de N?" )
    N <- as.numeric(N)
    beta <- readline(prompt = "¿Cuál es el valor de beta?" )
    beta <- as.numeric(beta)
    gama <- readline(prompt = "¿Cuál es el valor de gama?" )
    gama <- as.numeric(gama)
    S <- readline(prompt = "¿Cuál es el valor de S?" )
    S <- as.numeric(S)
    mu1 <- readline(prompt = "¿Cuál es la tasa de muerte de los susceptibles?" )
    mu1 <- as.numeric(mu1)
    I <- readline(prompt = "¿Cuál es el valor de infectados?" )
    I <- as.numeric(I)
    mu2 <- readline(prompt = "¿Cuál es la tasa de muerte de los infectados?" )
    mu2 <- as.numeric(mu2)
    R <- readline(prompt = "¿Cuál es el valor de R?" )
    R <- as.numeric(R)
    mu3 <- readline(prompt = "¿Cuál es la tasa de muerte de los recuperados?" )
    mu3 <- as.numeric(mu3)
    ti <- readline(prompt = "¿Cuál es el tiempo total del modelo? : " )
    ti <- as.numeric(ti)
    parametro2 <- c(beta=beta, gama=gama, mu=mu, N=N, mu1=mu1, mu2=mu2, mu3=mu3)
    daprincipio <- c(S=S, I=I, R=R)
    taaim <- seq(0, ti, by = 0.001)
    outo <- ode(daprincipio, taaim, SIR, parametro2)
    matplot(outo[ , 1], outo[ , 2:4], type = "l", xlab = "Tiempo", ylab = "Población",
            main = " Modelo SIR", lwd = 2) ; legend("topright", c("Susceptible", "Infectado", "Recuperado"), 
                                                   col = 1:4,lty=1:4,cex=0.5)
  }else if(p1 == "SIR" & p2 == F){
    SIR <- function(t,state,parameters){
      with(as.list(c(state, parameters)), {
        dS <- - beta*S*I 
        dI <- beta*S*I - gama*I
        dR <- gama*I
        list(c(dS, dI, dR))
      })
    }
    beta <- readline(prompt = "¿Cuál es el valor de beta?" )
    beta <- as.numeric(beta)
    gama <- readline(prompt = "¿Cuál es el valor de gama?" )
    gama <- as.numeric(gama)
    S <- readline(prompt = "¿Cuál es el valor de S?" )
    S <- as.numeric(S)
    I <- readline(prompt = "¿Cuál es el valor de I?" )
    I <- as.numeric(I)
    R <- readline(prompt = "¿Cuál es el valor de R?" )
    R <- as.numeric(R)
    ti <- readline(prompt = "¿Cuál es el tiempo total del modelo? : " )
    ti <- as.numeric(ti)
    parametro2 <- c(beta=beta, gama=gama)
    daprincipio <- c(S=S, I=I, R=R)
    taaim <- seq(0, ti, by = 0.001)
    outo <- ode(daprincipio, taaim, SIR, parametro2)
    matplot(outo[ , 1], outo[ , 2:4], type = "l", xlab = "Tiempo", ylab = "Población",
            main = " Modelo SIR", lwd = 2) ; legend("topright", c("Susceptible", "Infectado","Recuperado"), 
                                                   col = 1:4,lty=1:4,cex=0.5)
  }else if(p1 == "SIRS" & p2 == T){
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
    N <- readline(prompt = "¿Cuál es el valor de N?" )
    N <- as.numeric(N)
    beta <- readline(prompt = "¿Cuál es el valor de beta?" )
    beta <- as.numeric(beta)
    delta <- readline(prompt = "¿Cuál es el valor de delta?" )
    delta <- as.numeric(delta)
    gama <- readline(prompt = "¿Cuál es el valor de gama?" )
    gama <- as.numeric(gama)
    S <- readline(prompt = "¿Cuál es el valor de S?" )
    S <- as.numeric(S)
    mu1 <- readline(prompt = "¿Cuál es la tasa de muerte de los susceptibles?" )
    mu1 <- as.numeric(mu1)
    I <- readline(prompt = "¿Cuál es el valor de infectados?" )
    I <- as.numeric(I)
    mu2 <- readline(prompt = "¿Cuál es la tasa de muerte de los infectados?" )
    mu2 <- as.numeric(mu2)
    R <- readline(prompt = "¿Cuál es el valor de R?" )
    R <- as.numeric(R)
    mu3 <- readline(prompt = "¿Cuál es la tasa de muerte de los recuperados?" )
    mu3 <- as.numeric(mu3)
    ti <- readline(prompt = "¿Cuál es el tiempo total del modelo? : " )
    ti <- as.numeric(ti)
    parametro2 <- c(beta=beta, gama=gama, mu=mu, N=N, mu1=mu1, mu2=mu2, mu3=mu3)
    daprincipio <- c(S=S, I=I, R=R)
    taaim <- seq(0, ti, by = 0.001)
    outo <- ode(daprincipio, taaim, SIRS, parametro2)
    matplot(outo[ , 1], outo[ , 2:4], type = "l", xlab = "Tiempo", ylab = "Población",
            main = " Modelo SIRS D.", lwd = 2) ; legend("topright", c("Susceptible", "Infectado","Recuperado"), 
                                                    col = 1:4,lty=1:4,cex=0.5)
  }else if(p1 == "SIRS" & p2 == F){
    SIRS <- function(t,state,parameters){
      with(as.list(c(state, parameters)), {
        dS <- - beta*S*I + delta*R
        dI <- beta*S*I - gama*I
        dR <- gama*I - delta*R
        list(c(dS, dI, dR))
      })
    }
    beta <- readline(prompt = "¿Cuál es el valor de beta?" )
    beta <- as.numeric(beta)
    delta <- readline(prompt = "¿Cuál es el valor de delta?" )
    delta <- as.numeric(delta)
    gama <- readline(prompt = "¿Cuál es el valor de gama?" )
    gama <- as.numeric(gama)
    S <- readline(prompt = "¿Cuál es el valor de S?" )
    S <- as.numeric(S)
    I <- readline(prompt = "¿Cuál es el valor de infectados?" )
    I <- as.numeric(I)
    R <- readline(prompt = "¿Cuál es el valor de R?" )
    R <- as.numeric(R)
    ti <- readline(prompt = "¿Cuál es el tiempo total del modelo? : " )
    ti <- as.numeric(ti)
    parametro2 <- c(beta=beta, gama=gama, delta=delta)
    daprincipio <- c(S=S, I=I, R=R)
    taaim <- seq(0, ti, by = 0.001)
    outo <- ode(daprincipio, taaim, SIRS, parametro2)
    matplot(outo[ , 1], outo[ , 2:4], type = "l", xlab = "Tiempo", ylab = "Población",
            main = " Modelo SIRS", lwd = 2) ; legend("topright", c("Susceptible", "Infectado","Recuperado"), 
                                                    col = 1:4,lty=1:4,cex=0.5)
    
  }else if(p1 == "SEIR" & p2 == T){
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
    N <- readline(prompt = "¿Cuál es el valor de N?" )
    N <- as.numeric(N)
    beta <- readline(prompt = "¿Cuál es el valor de beta?" )
    beta <- as.numeric(beta)
    delta <- readline(prompt = "¿Cuál es el valor de delta?" )
    delta <- as.numeric(delta)
    gama <- readline(prompt = "¿Cuál es el valor de gama?" )
    gama <- as.numeric(gama)
    S <- readline(prompt = "¿Cuál es el valor de S?" )
    S <- as.numeric(S)
    mu1 <- readline(prompt = "¿Cuál es la tasa de muerte de los susceptibles?" )
    mu1 <- as.numeric(mu1)
    E <- readline(prompt = "¿Cuál es el valor de expuestos?" )
    E <- as.numeric(E)
    mu2 <- readline(prompt = "¿Cuál es la tasa de muerte de los expuestos?" )
    mu2 <- as.numeric(mu2)
    I <- readline(prompt = "¿Cuál es el valor de infectados?" )
    I <- as.numeric(I)
    mu3 <- readline(prompt = "¿Cuál es la tasa de muerte de los infectados?" )
    mu3 <- as.numeric(mu3)
    R <- readline(prompt = "¿Cuál es el valor de R?" )
    R <- as.numeric(R)
    mu4 <- readline(prompt = "¿Cuál es la tasa de muerte de los recuperados?" )
    mu4 <- as.numeric(mu4)
    ti <- readline(prompt = "¿Cuál es el tiempo total del modelo? : " )
    ti <- as.numeric(ti)
    parametro2 <- c(beta=beta, delta=delta, gama=gama, mu=mu, N=N, mu1=mu1, mu2=mu2, mu3=mu3, mu4=mu4)
    daprincipio <- c(S=S, E=E, I=I, R=R)
    taaim <- seq(0, ti, by = 0.001)
    outo <- ode(daprincipio, taaim, SEIR, parametro2)
    matplot(outo[ , 1], outo[ , 2:5], type = "l", xlab = "Tiempo", ylab = "Población",
            main = " Modelo SEIR", lwd = 2) ; legend("topright", c("Susceptible", "Expuesto","Infectado","Recuperado"), 
                                                    col = 1:5,lty=1:5,cex=0.5)
  }else if(p1 == "SEIR" & p2 == F){
    SEIR <- function(t,state,parameters){
      with(as.list(c(state, parameters)), {
        dS <- - beta*S*I 
        dE <- beta*S*I - delta*E
        dI <- delta*E - gama*I
        dR <- gama*I
        list(c(dS, dE, dI, dR))
      })
    }
    beta <- readline(prompt = "¿Cuál es el valor de beta?" )
    beta <- as.numeric(beta)
    delta <- readline(prompt = "¿Cuál es el valor de delta?" )
    delta <- as.numeric(delta)
    gama <- readline(prompt = "¿Cuál es el valor de gama?" )
    gama <- as.numeric(gama)
    S <- readline(prompt = "¿Cuál es el valor de S?" )
    S <- as.numeric(S)
    E <- readline(prompt = "¿Cuál es el valor de expuestos?" )
    E <- as.numeric(E)
    I <- readline(prompt = "¿Cuál es el valor de infectados?" )
    I <- as.numeric(I)
    R <- readline(prompt = "¿Cuál es el valor de recuperados?" )
    R <- as.numeric(R)
    ti <- readline(prompt = "¿Cuál es el tiempo total del modelo? : " )
    ti <- as.numeric(ti)
    parametro2 <- c(beta=beta, delta=delta, gama=gama)
    daprincipio <- c(S=S, E=E, I=I, R=R)
    taaim <- seq(0, ti, by = 0.001)
    outo <- ode(daprincipio, taaim, SEIR, parametro2)
    matplot(outo[ , 1], outo[ , 2:5], type = "l", xlab = "Tiempo", ylab = "Población",
            main = " Modelo SEIR", lwd = 2) ; legend("topright", c("Susceptible", "Expuesto","Infectado","Recuperado"), 
                                                     col = 1:5,lty=1:5,cex=0.5)
  }else if(p1 == "SEIRS" & p2 == T){
    SEIRS <- function(t,state,parameters){
      with(as.list(c(state, parameters)), {
        dS <- mu*N - beta*S*I + ro*R - mu1*S
        dE <- beta*S*I - delta*I - mu2*E
        dI <- delta*I - gama*R - mu3*I
        dR <- gama*R - ro*R - mu4*R
        list(c(dS, dE, dI, dR))
      })
    }
    mu <- readline(prompt = "¿Cuál es la tasa de crecimiento de los nacimientos?" )
    mu <- as.numeric(mu)
    N <- readline(prompt = "¿Cuál es el valor de N?" )
    N <- as.numeric(N)
    beta <- readline(prompt = "¿Cuál es el valor de beta?" )
    beta <- as.numeric(beta)
    ro <- readline(prompt = "¿Cuál es el valor de ro?" )
    ro <- as.numeric(ro)
    delta <- readline(prompt = "¿Cuál es el valor de delta?" )
    delta <- as.numeric(delta)
    gama <- readline(prompt = "¿Cuál es el valor de gama?" )
    gama <- as.numeric(gama)
    S <- readline(prompt = "¿Cuál es el valor de S?" )
    S <- as.numeric(S)
    mu1 <- readline(prompt = "¿Cuál es la tasa de muerte de los susceptibles?" )
    mu1 <- as.numeric(mu1)
    E <- readline(prompt = "¿Cuál es el valor de expuestos?" )
    E <- as.numeric(E)
    mu2 <- readline(prompt = "¿Cuál es la tasa de muerte de los expuestos?" )
    mu2 <- as.numeric(mu2)
    I <- readline(prompt = "¿Cuál es el valor de infectados?" )
    I <- as.numeric(I)
    mu3 <- readline(prompt = "¿Cuál es la tasa de muerte de los infectados?" )
    mu3 <- as.numeric(mu3)
    R <- readline(prompt = "¿Cuál es el valor de R?" )
    R <- as.numeric(R)
    mu4 <- readline(prompt = "¿Cuál es la tasa de muerte de los recuperados?" )
    mu4 <- as.numeric(mu4)
    ti <- readline(prompt = "¿Cuál es el tiempo total del modelo? : " )
    ti <- as.numeric(ti)
    parametro2 <- c(beta=beta, delta=delta, gama=gama, mu=mu, N=N, mu1=mu1, mu2=mu2, mu3=mu3, mu4=mu4)
    daprincipio <- c(S=S, E=E, I=I, R=R)
    taaim <- seq(0, ti, by = 0.001)
    outo <- ode(daprincipio, taaim, SEIRS, parametro2)
    matplot(outo[ , 1], outo[ , 2:5], type = "l", xlab = "Tiempo", ylab = "Población",
            main = " Modelo SEIRS", lwd = 2) ; legend("topright", c("Susceptible", "Expuesto","Infectado","Recuperado"), 
                                                     col = 1:5,lty=1:5,cex=0.5)
  }else if(p1 == "SEIRS" & p2 == F){
    SEIRS <- function(t,state,parameters){
      with(as.list(c(state, parameters)), {
        dS <- - beta*S*I + ro*R
        dE <- beta*S*I - delta*I
        dI <- delta*I - gama*R
        dR <- gama*R - ro*R
        list(c(dS, dE, dI, dR))
      })
    }
    beta <- readline(prompt = "¿Cuál es el valor de beta?" )
    beta <- as.numeric(beta)
    ro <- readline(prompt = "¿Cuál es el valor de ro?" )
    ro <- as.numeric(ro)
    delta <- readline(prompt = "¿Cuál es el valor de delta?" )
    delta <- as.numeric(delta)
    gama <- readline(prompt = "¿Cuál es el valor de gama?" )
    gama <- as.numeric(gama)
    S <- readline(prompt = "¿Cuál es el valor de S?" )
    S <- as.numeric(S)
    E <- readline(prompt = "¿Cuál es el valor de expuestos?" )
    E <- as.numeric(E)
    I <- readline(prompt = "¿Cuál es el valor de infectados?" )
    I <- as.numeric(I)
    R <- readline(prompt = "¿Cuál es el valor de R?" )
    R <- as.numeric(R)
    ti <- readline(prompt = "¿Cuál es el tiempo total del modelo? : " )
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
