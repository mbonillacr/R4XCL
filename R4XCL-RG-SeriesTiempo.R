#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# SERIES TEMPORALES
# ( COINTEGRACION,RAIZ UNITARIA, AUTOCORRELACIÓN, ADITIVO, MULTIPLICATIVO )
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

ST_SeriesTemporales<- function(SetDatosX,Periodicidad=1, TipoOutPut=0)
{
  
    library(tseries)
  
    Procedimientos <- R4XCL_INT_PROCEDIMIENTOS()
    
    pPeriodicidad <- c(1,2,3,4,12)
    DatosX        <- na.omit(as.numeric(SetDatosX))  
    DatosXts      <- ts(SetDatosX,frequency=pPeriodicidad[Periodicidad])
    pTipoDescomp  <- c("additive","multiplicative")
    pVariables    <- ncol(SetDatosX)

    if (TipoOutPut==0){
      
        OutPut = Procedimientos$SERIES_T
        
    }else if (TipoOutPut==1){
      
      # COINTEGRACION
      if(pVariables==1)
        {OutPut="Este test requiere al menos dos variables"}
      else
        {
          a <- tseries::po.test(DatosXts)
          OutPut  <- capture.output(a)
        }
      
    }else if (TipoOutPut==2){
      
        # RAIZ UNITARIA                      
        a <- tseries::adf.test(DatosXts)
        OutPut  <- capture.output(a)
        
    }else if (TipoOutPut==3){
      
        # PHILLIPS - PERRON
        a <- tseries::pp.test(DatosXts)
        OutPut  <- capture.output(a)
      
    }else if (TipoOutPut==4){
      
        # JARQUE BERA                      
        a <- tseries::jarque.bera.test(DatosXts) 
        OutPut  <- capture.output(a)                
        
    }else if (TipoOutPut==5){
      
        # AUTOCORRELACIÓN
        b <- acf(DatosXts, pl=FALSE)
        c <- pacf(DatosXts, pl=FALSE)
        n <- b$n.used
        h <- (-1/n) + c(-2, 2)/sqrt(n)
        
        OutPut  = cbind(
                        "AutoCorrelogramaTotal"=b$acf,
                        "AutoCorrelogramaParcial"=c$acf,
                        "LIM_inf"=h[1],
                        "LIM_sup"=h[2]
                       )
        
  }else if (TipoOutPut==6){
    
       # ADITIVO
       Modelo  <- decompose(DatosXts, type="additive") 
       OutPut  <- cbind(Modelo$trend, Modelo$seasonal,Modelo$random)
  
  }else if (TipoOutPut==7){
    
        # MULTIPLICATIVO
        Modelo  <- decompose(DatosXts, type="multiplicative") 
        OutPut  <- cbind(Modelo$trend, Modelo$seasonal,Modelo$random)
  }
    
  return(OutPut);  

}

Detalle = "Calcula diversos tests de series temporales, a la (s) Serie (s) facilitada (s) por el usuario"

attr(ST_SeriesTemporales, "description" ) = 
  list( 
      Detalle,
      SetDatosX    = "Series de tiempo por analizar",
      Periodicidad = "Periodicidad de los datos 1: Anual, 2: Semestral, 3: Trimestral 4:Mensual",
      TipoOutput   = "0:Lista de funciones, 1:ARMA, 2:ARIMA, 3: SARIMA,4: GARCH, 5:E-GARCH"
      )

ST_Autoregresivos<- function(SetDatosX,Periodicidad=1, TipoOutPut=0, OrdenP, OrdenD, OrdenQ)
{
  
  library(tseries)
  Procedimientos <- R4XCL_INT_PROCEDIMIENTOS()

  pPeriodicidad <- c(1,2,3,4,12)
  DatosX        <- na.omit(as.numeric(SetDatosX))  
  DatosXts      <- ts(SetDatosX,frequency=pPeriodicidad[Periodicidad])
  pTipoDescomp  <- c("additive","multiplicative")
  
  if (TipoOutPut==0){
    
    OutPut = Procedimientos$SERIES_AR
    
  }else if (TipoOutPut==1){
    
    # PREDICCIONARMA
    a       <- tseries::arma(DatosXts, order=c(OrdenP,OrdenQ))
    OutPut  <- capture.output(summary(a))
  
  }else if (TipoOutPut==1.1){
    
    # ARMA
    a       <- tseries::arma(DatosXts, order=c(OrdenP,OrdenQ))
    OutPut  <- data.frame(predict(a, n.ahead = 6,se.fit = TRUE))   
    
  }else if (TipoOutPut==2){
    
    # ARIMA                     
    a       <- stats::arima(DatosXts, order=c(OrdenP,OrdenD,OrdenQ))
    OutPut  <- capture.output(a)
  
  }else if (TipoOutPut==2.1){
    
    # PREDICCION ARIMA                     
    a       <- stats::arima(DatosXts, order=c(OrdenP,OrdenD,OrdenQ))
    OutPut  <- data.frame(predict(a, n.ahead = 6,se.fit = TRUE))
      
  }else if (TipoOutPut==3){
    
    # SARiMA                     
    OutPut  <- "EN PROCESO"
    
  }else if (TipoOutPut==3.1){
    
    # PREDICCION SARiMA                     
    OutPut  <- "EN PROCESO"    

  }else if (TipoOutPut==4){
    
    # GARCH                     
    a       <- tseries::garch(DatosXts, order=c(OrdenP,OrdenQ))
    OutPut  <- capture.output(summary(a))  

  }else if (TipoOutPut==4.1){
    
    # PREDICCION GARCH                     
    a       <- tseries::garch(DatosXts, order=c(OrdenP,OrdenQ))
    OutPut  <- data.frame(predict(a, n.ahead = 6,se.fit = TRUE))    
            
  }else if (TipoOutPut==5){
    # E GARCH                     
    OutPut  <- "EN PROCESO"
    
  }else if (TipoOutPut==5.1){
    
    # PREDICCION E GARCH                      
    OutPut  <- "EN PROCESO" 
    
  }else if (TipoOutPut>04){
    
    OutPut  <- "REVISAR PARAMETRO TIPO OUTPUT"
    
  }
  
  return(OutPut);  
  
}

Detalle = "Calcula diversos tests de series temporales, a la (s) Serie (s) facilitada (s) por el usuario"

attr(ST_Autoregresivos, "description" ) = 
  list( 
    Detalle,
    SetDatosX    = "Series de tiempo por analizar",
    Periodicidad = "Periodicidad de los datos 1: Anual, 2: Semestral, 3: Trimestral 4:Mensual",
    TipoOutput   = "0:Lista de funciones, 1:ARMA, 1.1: PREDICCIÓN ARMA, 2: ARiMA, 
                    2.1: PREDICCIÓN ARiMA, 3: GARCH, 3.1:PREDICCIÓN GARCH, 4:DESCOMPOSICIÓN ST ADITIVO, 
                    4:DESCOMPOSICIÓN ST MULTIPLICATIVO"
  )

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FILTROS:  1:Hodrick-Prescott, 
#           2:Baxter-King
#           3:Christiano-Fitzgerald
#           4:Butterworth
#           5:Trigonometric regression                 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

ST_Filtro <- function(
                     SetDatosX, 
                     Periodicidad=1,
                     TipoModelo=1,
                     Drift=0,
                     RaizUnitaria=0,
                     TipoOutput=0
                     )
{
  
  #-------------------------->>>
  # PREPARACION DE DATOS Y PARAMETROS
  #-------------------------->>>
  
  library(tseries) 
  library(mFilter)
 
  Procedimientos <- R4XCL_INT_PROCEDIMIENTOS()
  
  #-------------------------->>>   
  # [1] PREPARACION DE DATOS Y PARAMETROS  
  #-------------------------->>>  
  
  pLambda       <- c(100,800,1600,14400)
  pFiltro       <- c("HP","BK","CF","BW","TR")
  pPeriodicidad <- c(1,2,4,12)
  pRaizUnitaria <- c("FALSE", "TRUE")
  pDrift        <- c("FALSE", "TRUE")
  pTipoDescomp  <- c("additive","multiplicative")
  
  SetDatosX <- na.omit(as.numeric(SetDatosX))  
  
  SetDatosX_TS <- ts(SetDatosX,frequency=pPeriodicidad[Periodicidad])

  #-------------------------->>> 
  # [2] PROCEDIMIENTO ANALITICO
  #-------------------------->>> 
  
  if (TipoModelo == 1){ 
    
    Modelo <- mFilter(SetDatosX_TS, 
                      filter = pFiltro[TipoModelo],
                      freq   = pLambda[Periodicidad], 
                      root   = pRaizUnitaria[RaizUnitaria+1],
                      drift  = pDrift[Drift+1]) 
  
  }else if (TipoModelo == 3 |TipoModelo == 5 ){ 
    
    pRaizUnitaria  <- c(0,1) 
    pDrift  <- c(0,1)    
    Modelo  <- mFilter(SetDatosX_TS, 
                      filter = pFiltro[TipoModelo],
                      root   = pRaizUnitaria[RaizUnitaria+1],
                      drift  = pDrift[Drift+1])
    
  }else{ 
      
    Modelo <- mFilter(SetDatosX_TS, 
                      filter = pFiltro[TipoModelo],
                      root   = pRaizUnitaria[RaizUnitaria+1],
                      drift  = pDrift[Drift+1])
  }

   #-------------------------->>> 
   # [3] PREPARACION DE RESULTADOS
   #-------------------------->>> 
   
  if (TipoOutput == 0){
    
    OutPut = Procedimientos$SERIES_F

              
  }else if(TipoOutput > 0){      
    
    OutPut  <- cbind(Modelo$trend, Modelo$cycle,Modelo$lambda)
     
  } 
   #-------------------------->>> 
   # [4] RESULTADO FINAL
   #-------------------------->>> 

  return(OutPut)  
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

Detalle = "Aplica diversos filtros a una Serie de Tiempo"

attr(ST_Filtro, "description" ) = 
  list( 
       Detalle,
       SetDatosX       = "Serie de tiempo observada",
       Periodicidad    = "Periodicidad de los datos 1: Anual, 2: Semestral, 3: Trimestral 4:Mensual",
       TipoModelo      = "1: Hodrick-Prescott, 2:Baxter-King, 3: Christiano-Fitzgerald, 4: Butterworth, 5:Trigonometric regression",
       Drift           = "Hay presencia de Drift T|F (0:NO, 1:SI)",
       RaizUnitaria    = "Hay presencia de Raiz Unitaria T|F (0:NO, 1:SI)",
       TipoOutput      = "0:Modelo, 1:Prob Estimada, 2:Prediccion, 3:Test HL, 4:Efectos Marginales"
      )