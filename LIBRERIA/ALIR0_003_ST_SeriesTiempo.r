# *********************************************************************
#
# LISTA DE ACRONIMOS UTILIZADOS
#
# HW: Se refiere a HoltWinters
# FF: Se refiere a Forectast
#
# *********************************************************************

ST_RaizUnitaria<- function(DS, PER=1){

  library('tseries')  #revisar sacar cosas de aca
  #_________________________________________________________________  
  # [1] PREPARACION DE DATOS 
  #_________________________________________________________________
  
  NOMBRE_DS=DS[1,1]
  
  DS=na.omit(as.numeric(DS))  
  
  DS_TS=ts(DS,frequency=PER, 1)
  
  nn=nrow(DS_TS)
  pp=ncol(DS_TS)
  
  DIMENSION=  paste("F: ",nn, "C:",pp, space=" ")
  
  #_________________________________________________________________ 
  # [2] PROCEDIMIENTO ANALITICO
  #_________________________________________________________________   
  
  a = tseries::adf.test(DS_TS)                                     
  b = tseries::pp.test(DS_TS) 
  c = tseries::kpss.test(DS_TS)
  
  aa = c("DickeyFuller","stat"=a$statistic,"p-value"= a$p.value, "H0"=a$alternative,"Lag"= a$parameter)
  bb = c("PhillipePerron","stat"=b$statistic,"p-value"= b$p.value, "H0"=b$alternative,"Lag"= b$parameter)
  #cc=c("KPSS","stat"=c$statistic,"p-value"= c$p.value, "H0"="","Lag"= c$parameter)
  
  OutPut_A=data.frame(rbind(aa,bb))

  #_________________________________________________________________ 
  # [3] PREPARACION DE RESULTADOS
  #_________________________________________________________________ 
  
  data = capture.output(summary(OutPut_A))
  data = data[data!= ""]
  
  NombreDF ="Raiz Unitaria :"
  OutPut_B = data.frame(NombreDF = data)
  OutPut_A = append(OutPut_B,OutPut_A)
  ListaOuput = list()
  qElementos = length(OutPut_A)
  
  for(i in 1:qElementos)
  {
    AA = toupper(names(OutPut_A[i]))
    AA = paste0("--> ", AA)
    nombre = (data.frame(c(paste0(NOMBRE_DS,": Raiz Unitaria | Out: ",i),AA)))
    
    ListaOuput = append(ListaOuput, nombre)
    ListaOuput = append(ListaOuput, OutPut_A[i])
  }
  
  #_________________________________________________________________ 
  # [4] RESULTADO FINAL
  #_________________________________________________________________   
  
  return(ListaOuput)  
  
}

#_________________________________________________________________________________
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++  
#_________________________________________________________________________________

ST_Cointegracion<- function(SetDatosX){
  
  library(tseries)  #revisar sacar cosas de aca
  #_________________________________________________________________  
  # [1] PREPARACION DE DATOS 
  #_________________________________________________________________
  
  p = ncol(SetDatosX)
  
  nombresX = paste0(SetDatosX[1,1:p])

  DatosX = SetDatosX[-1,]
  
  ObsX = nrow(SetDatosX)-1

  DatosX = matrix(as.numeric(DatosX), nrow=ObsX, ncol=p)
  Datos = ts(DatosX)
  
  #_________________________________________________________________ 
  # [2] PROCEDIMIENTO ANALITICO
  #_________________________________________________________________   

  OutPut_A = tseries::po.test(Datos)  
  
  #_________________________________________________________________ 
  # [3] PREPARACION DE RESULTADOS
  #_________________________________________________________________ 
  
  data = capture.output(summary(OutPut_A))
  data = data[data!= ""]
  
  NombreDF = "Raiz Unitaria :"
  OutPut_B = data.frame(NombreDF = data)
  OutPut_A = append(OutPut_B,OutPut_A)
  ListaOuput = list()
  qElementos = length(OutPut_A)
  
  for(i in 1:qElementos)
  {
    AA = toupper(names(OutPut_A[i]))
    AA = paste0("--> ", AA)
    nombre = (data.frame(c(paste0("Phillips-Ouliaris : Cointegracion | Out: ",i),AA)))
    
    ListaOuput = append(ListaOuput, nombre)
    ListaOuput = append(ListaOuput, OutPut_A[i])
  }
  
  #_________________________________________________________________ 
  # [4] RESULTADO FINAL
  #_________________________________________________________________   
  
  return(ListaOuput)  
  
}

#_________________________________________________________________________________
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++  
#_________________________________________________________________________________

#TEST DE COINTEGRACION
#a=po.test(DS)

ST_HoltWinters <- function(DS, PER=1, Clase=0, ff=0,PROY)
  
{
    library(TTR)
    library(tseries)  #revisar sacar cosas de aca
    library(forecast)

#_________________________________________________________________  
# [1] PREPARACION DE DATOS 
#_________________________________________________________________
  
    DS=na.omit(DS)
    DS_TS=ts(DS,frequency=PER, 1)
    
    nn=nrow(DS_TS)
    pp=ncol(DS_TS)                
    #DIMENSION=  paste("F: ",nn, "C:",pp, space=" ")

    if(Clase>2){Clase=0}      # RETORNA POR DEFAULT EL ESTADO CERO    
        
    if(Clase==0)      ## SIN: NIVEL, TENDENCIA, COMPONENTES ESTACIONALES 
      
        { 
          DS_ST_HW <- HoltWinters(DS_TS, beta=FALSE, gamma=FALSE)
          DS_ST_HW_FF<-forecast(DS_ST_HW, ff)
        } 
    
    else if(Clase==1) ## SIN: NIVEL, ESTACIONAL
      
        { 
          DS_ST_HW <- HoltWinters(DS_TS, beta=TRUE, gamma=FALSE)
          DS_ST_HW_FF<-forecast(DS_ST_HW, ff)
        } 
       
    else if (Clase==2) ## SIN: NIVEL : Holt-Winters Exponential Smoothing
      
        { 
          DS_ST_HW <- HoltWinters(DS_TS, beta=TRUE, gamma=TRUE)
          DS_ST_HW_FF<-forecast(DS_ST_HW, ff)
        } 
    
    if (PROY==1)
      {
        data.frame(DS_ST_HW_FF$mean)
        BERT.graphics.device(cell=T);
        plot(DS_ST_HW_FF)
        dev.off();
        T;
      }
    else
      {data.frame(DS_ST_HW$fitted)}
}

#_________________________________________________________________________________
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++  
#_________________________________________________________________________________

ST_ARIMA <- function(DS, PER, Clase=0, ff)
  
{
    library(TTR)
    library(tseries)  
    library(forecast)
    library(gplots)
  
    NOMBRE_DS=DS[1,1]
  
    DS=na.omit(as.numeric(DS))  

    DS_TS=ts(DS,frequency=PER, 1)
    
    nn=nrow(DS_TS)
    pp=ncol(DS_TS)  
    
    DIMENSION=  paste("F: ",nn, "C:",pp, space=" ")
    
    if(Clase>1){Clase=0}      # RETORNA POR DEFAULT EL ESTADO CERO  
    
    if(Clase==0)              # SIN: COMPONENTES ESTACIONALES
      
      { 
        OutPut_A <- auto.arima(DS_TS, seasonal=FALSE)
      } 
    
    else if(Clase==1) ## CON: COMPONENTES ESTACIONALES
      
      {
        OutPut_A <- auto.arima(DS_TS, seasonal=TRUE)
      } 

    if (ff>0){
              OutPut_C=data.frame("Proyeccion",forecast(OutPut_A,ff))
             }
    
    data = capture.output(summary(OutPut_A))
    data = data[data!= ""]

    #NOMBRE_DS
    
    OutPut_B = data.frame("ARIMA" = data)
    OutPut_A = append(OutPut_B,OutPut_A)
    
    if (ff>0){OutPut_A = append(OutPut_A,OutPut_C)}
    
    ListaOuput=list()
    qElementos=length(OutPut_A)
    
    for(i in 1:qElementos)
    {
      AA= toupper(names(OutPut_A[i]))
      AA= paste0("--> ", AA)
      nombre = (data.frame(c(paste0(NOMBRE_DS,": ARIMA | Out: ",i),AA)))

      ListaOuput=append(ListaOuput, nombre)
      ListaOuput=append(ListaOuput, OutPut_A[i])
    }
 
#-------------------------->>> 
# [4] RESULTADO FINAL
#-------------------------->>>     
       
    return(ListaOuput)
    
}

#_________________________________________________________________________________
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++  
#_________________________________________________________________________________

ST_GARCH <- function(DS, PER, p, q)
  
{
  library(TTR)
  library(tseries)  
  library(forecast)

#_________________________________________________________________  
# [1] PREPARACION DE DATOS 
#_________________________________________________________________
  
  NOMBRE_DS=DS[1,1]

  DS=na.omit(as.numeric(DS))  

  DS_TS=ts(DS,frequency=PER, 1)

  nn=nrow(DS_TS)
  pp=ncol(DS_TS)
    
  DIMENSION=  paste("F: ",nn, "C:",pp, space=" ")
  
#_________________________________________________________________ 
# [2] PROCEDIMIENTO ANALITICO
#_________________________________________________________________ 
  
  OutPut_A <- garch(DS_TS, order = c(p,q))
  
#_________________________________________________________________ 
# [3] PREPARACION DE RESULTADOS
#_________________________________________________________________ 
  
  data=capture.output(summary(OutPut_A))
  data = data[data!= ""]

  NombreDF ="GARCH(p,q):"
  OutPut_B = data.frame(NombreDF = data)
  OutPut_A = append(OutPut_B,OutPut_A)
  ListaOuput=list()
  qElementos=length(OutPut_A)
  
  for(i in 1:qElementos)
  {
    AA= toupper(names(OutPut_A[i]))
    AA= paste0("--> ", AA)
    nombre = (data.frame(c(paste0(NOMBRE_DS,": GARCH | Out: ",i),AA)))
    
    ListaOuput=append(ListaOuput, nombre)
    ListaOuput=append(ListaOuput, OutPut_A[i])
  }

#_________________________________________________________________ 
# [4] RESULTADO FINAL
#_________________________________________________________________   
    
  return(ListaOuput)
}

#_________________________________________________________________________________
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++  
#_________________________________________________________________________________

ST_DESCOMPOSICION<-function(DS, PER, CLASE)
  {
 
#_________________________________________________________________  
# [1] PREPARACION DE DATOS 
#_________________________________________________________________
   
  NOMBRE_DS=DS[1,1]

  DS=na.omit(as.numeric(DS))  
  
  DS_TS=ts(DS,frequency=PER, 1)
  
  nn=nrow(DS_TS)
  pp=ncol(DS_TS)
               
  DIMENSION=  paste("F: ",nn, "C: ",pp, space=" ")
  
  Tipo_Descomposicion="additive"
  if (CLASE==1){Tipo_Descomposicion="multiplicative"}
 
#_________________________________________________________________ 
# [2] PROCEDIMIENTO ANALITICO
#_________________________________________________________________ 
   
  OutPut_A = decompose(DS_TS, type=Tipo_Descomposicion)

#_________________________________________________________________ 
# [3] PREPARACION DE RESULTADOS
#_________________________________________________________________   
    
  ListaOuput=list()
  qElementos=length(OutPut_A)
  
  for(i in 1:qElementos)
  {
    AA= toupper(names(OutPut_A[i]))
    AA= paste0("--> ", AA)
    nombre = (data.frame(c(paste0(NOMBRE_DS,": Descomposicion | Out: ",i),AA)))
    
    ListaOuput=append(ListaOuput, nombre)
    ListaOuput=append(ListaOuput, OutPut_A[i])
  }
  
#_________________________________________________________________ 
# [4] RESULTADO FINAL
#_________________________________________________________________ 
  
  return(ListaOuput)
  
}

#_________________________________________________________________________________
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++  
#_________________________________________________________________________________

ST_AUTOCORRELOGRAMA<-function(DS, PER, ci)
{
  
  #_________________________________________________________________  
  # [1] PREPARACION DE DATOS 
  #_________________________________________________________________
  
  NOMBRE_DS=DS[1,1]
  
  DS=na.omit(as.numeric(DS))  
  
  DS_TS=ts(DS,frequency=PER, 1)
  
  nn=nrow(DS_TS)
  pp=ncol(DS_TS)
  
  DIMENSION=  paste("F: ",nn, "C: ",pp, space=" ")

  #_________________________________________________________________ 
  # [2] PROCEDIMIENTO ANALITICO
  #_________________________________________________________________ 
  
  OutPut_A = acf(DS_TS)
  OutPut_B = pacf(DS_TS)
  
  SL <- qnorm((1 + ci)/2)/sqrt(sum(!is.na(DS_TS)))

  OutPut_1 = data.frame("ACF" =OutPut_A$acf)
  OutPut_2 = data.frame("PACF"=OutPut_B$acf)
  OutPut_3 = data.frame("Intervalo ACF"= c(-SL, SL))
  
  #_________________________________________________________________ 
  # [3] PREPARACION DE RESULTADOS
  #_________________________________________________________________   
  
  OutPut_A = list(OutPut_1,OutPut_2,OutPut_3)
  
  ListaOuput=list()
  qElementos=length(OutPut_A)
  
  for(i in 1:qElementos)
  {
    AA= toupper(names(OutPut_A[i]))
    AA= paste0("--> ", AA)
    nombre = (data.frame(c(paste0(NOMBRE_DS,": Autocorrelograma | Out: ",i),AA)))
    
    ListaOuput=append(ListaOuput, nombre)
    ListaOuput=append(ListaOuput, OutPut_A[i])
  }
  
  #_________________________________________________________________ 
  # [4] RESULTADO FINAL
  #_________________________________________________________________ 
  
  return(ListaOuput)
  
}

#_________________________________________________________________________________
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++  
#_________________________________________________________________________________