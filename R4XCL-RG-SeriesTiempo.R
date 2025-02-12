#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# TEST DE COINTEGRACION
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

ST_TestCointegracion <- function(
                                  SetDatosX, 
                                  Periodicidad=1
                                )
{
  
  #-------------------------->>>
  # VALIDACIONES
  #-------------------------->>>
  if (missing(SetDatosX)) {
    stop("Error: SetDatosX es un parámetro obligatorio.")
  }
  if (!is.data.frame(SetDatosX)) {
    stop("Error: SetDatosX debe ser un data frame.")
  }
  if (!is.numeric(Periodicidad) || Periodicidad < 1 || Periodicidad > 4) {
    stop("Error: Periodicidad debe ser un número entre 1 y 4.")
  }
  
  #-------------------------->>>
  # PREPARACION DE DATOS Y PARAMETROS
  #-------------------------->>>
  
  library(tseries)  #revisar a mayor detalle
  # library(mFilter)
  # library(xts)  
  # library(TTR)   
  
  #-------------------------->>>   
  # [1] PREPARACION DE DATOS Y PARAMETROS  
  #-------------------------->>>  
  p        <- ncol(SetDatosX) 
  nombresX <- paste0(SetDatosX[1,1:p])
  DatosX   <- SetDatosX[-1,]
  n        <- nrow(DatosX)-1
  DatosX   <- matrix(as.numeric(DatosX), nrow=n, ncol=p)
  DatosXTS <- ts(DatosX)

  #-------------------------->>> 
  # [2] PROCEDIMIENTO ANALITICO
  #-------------------------->>> 
  
  a <- tseries::po.test(DatosXTS)  
  
  #-------------------------->>> 
  # [3] PREPARACION DE RESULTADOS
  #-------------------------->>> 
  
  OutPut  <- cbind("P-O"=a$statistic,"P-value"=a$p.value, "Lag"=a$parameter)

  #-------------------------->>> 
  # [4] RESULTADO FINAL
  #-------------------------->>> 
  
  return(OutPut)  
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

Detalle = "Calcula el test de cointegraci?n Phillips-Ouliaris a la Serie introducida"

attr(ST_TestCointegracion, "description" ) = 
  list( 
        Detalle,
        SetDatosX    = "Series de tiempo a las que se busca analizar cointegraci?n",
        Periodicidad = "Periodicidad de los datos 1: Anual, 2: Semestral, 3: Trimestral 4:Mensual"
       )

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# TEST DE RAIZ UNITARIA DF | PP
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

ST_RaizUnitaria <- function(SetDatosX,Periodicidad=1)
{
  
  #-------------------------->>>
  # VALIDACIONES
  #-------------------------->>>
  if (missing(SetDatosX)) {
    stop("Error: SetDatosX es un parámetro obligatorio.")
  }
  if (!is.vector(SetDatosX)) {
    stop("Error: SetDatosX debe ser un vector.")
  }
  if (!is.numeric(Periodicidad) || Periodicidad < 1 || Periodicidad > 4) {
    stop("Error: Periodicidad debe ser un número entre 1 y 4.")
  }
  
  #-------------------------->>>
  # PREPARACION DE DATOS Y PARAMETROS
  #-------------------------->>>
  
  library(tseries)  #revisar a mayor detalle
  library(mFilter)
  library(xts)  
  library(TTR)   
  #-------------------------->>>   
  # [1] PREPARACION DE DATOS Y PARAMETROS  
  #-------------------------->>>  
  
  pPeriodicidad <- c(1,2,4,12)
  SetDatosX     <- na.omit(as.numeric(SetDatosX))  
  SetDatosX_TS  <- ts(SetDatosX,frequency=pPeriodicidad[Periodicidad], 1)
  
  #-------------------------->>> 
  # [2] PROCEDIMIENTO ANALITICO
  #-------------------------->>> 
  
  a <- tseries::adf.test(SetDatosX_TS)                                     
  b <- tseries::pp.test(SetDatosX_TS) 
  c <- tseries::kpss.test(SetDatosX_TS)
  
  #-------------------------->>> 
  # [3] PREPARACION DE RESULTADOS
  #-------------------------->>> 
  
  OutPut  <- rbind(
                  c("DickeyFuller",
                    "stat"   = a$statistic,
                    "p-value"= a$p.value, 
                    "H0"     = a$alternative,
                    "Lag"    = a$parameter),
                  
                  c(
                    "PhillipePerron",
                    "stat"   = b$statistic,
                    "p-value"= b$p.value, 
                    "H0"     = b$alternative,
                    "Lag"    = b$parameter),
                  
                  c("KPSS",
                    "stat"   = c$statistic,
                    "p-value"= c$p.value, 
                    "H0"     = "stationary",
                    "Lag"    = c$parameter)
                )
  
  #-------------------------->>> 
  # [4] RESULTADO FINAL
  #-------------------------->>> 
  
  return(OutPut)  
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

Detalle <- "Calcula los test de ra?z unitaria de Dickey Fuller y Phillips Perron a la Serie introducida"

attr(ST_RaizUnitaria, "description" ) <- 
  list( 
        Detalle,
        SetDatosX       = "Serie de tiempo observada",
        Periodicidad    = "Periodicidad de los datos 1: Anual, 2: Semestral, 3: Trimestral 4:Mensual"
      )

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# AUTOCORRELOGRAMA
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

ST_Autocorrelacion <- function(
                              SetDatosX, 
                              Periodicidad=1
                              )
{
  
  #-------------------------->>>
  # VALIDACIONES
  #-------------------------->>>
  
  if (missing(SetDatosX)) {
    stop("Error: SetDatosX es un parámetro obligatorio.")
  }
  if (!is.vector(SetDatosX)) {
    stop("Error: SetDatosX debe ser un vector.")
  }
  if (!is.numeric(Periodicidad) || Periodicidad < 1 || Periodicidad > 4) {
    stop("Error: Periodicidad debe ser un número entre 1 y 4.")
  }
  
  #-------------------------->>>
  # PREPARACION DE DATOS Y PARAMETROS
  #-------------------------->>>
  
  library(tseries)
  library(mFilter)
  library(xts)  
  library(TTR)   
  
  #-------------------------->>>   
  # [1] PREPARACION DE DATOS Y PARAMETROS  
  #-------------------------->>>  
  
  pPeriodicidad <- c(1,2,4,12)
  SetDatosX     <- na.omit(as.numeric(SetDatosX))  
  SetDatosX_TS  <- ts(SetDatosX,frequency=pPeriodicidad[Periodicidad], 1)
  
  #-------------------------->>> 
  # [2] PROCEDIMIENTO ANALITICO
  #-------------------------->>> 
  
  b <- acf(SetDatosX_TS)
  c <- pacf(SetDatosX_TS)
  n <- b$n.used
  h <- -1/n + c(-2, 2)/sqrt(n)
  
  Modelo  = cbind(
                  "AutoCorrelogramaTotal"=b$acf,
                  "AutoCorrelogramaParcial"=c$acf,
                  "LIM_inf"=h[1],
                  "LIM_sup"=h[2]
                  )
  
  #-------------------------->>> 
  # [3] PREPARACION DE RESULTADOS
  #-------------------------->>> 

    OutPut <- Modelo
    
  #-------------------------->>> 
  # [4] RESULTADO FINAL
  #-------------------------->>> 
  
  return(OutPut)  
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

Detalle <- "Calcula la Autocorrelaci?n Serial de la Serie introducida"

attr(ST_Autocorrelacion, "description" ) <- 
  list( 
    Detalle,
    SetDatosX       = "Serie de tiempo observada",
    Periodicidad    = "Periodicidad de los datos 1: Anual, 2: Semestral, 3: Trimestral 4:Mensual"
  )


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# DESCOMPOSICION:
#           1:Aditiva
#           2:Multiplicativa
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

ST_Descomposicion <- function(
                              SetDatosX, 
                              Periodicidad=1,
                              TipoModelo=1,
                              TipoOutput=0
                            )
{
  
  #-------------------------->>>
  # VALIDACIONES
  #-------------------------->>>
  if (missing(SetDatosX)) {
    stop("Error: SetDatosX es un parámetro obligatorio.")
  }
  if (!is.vector(SetDatosX)) {
    stop("Error: SetDatosX debe ser un vector.")
  }
  if (!is.numeric(Periodicidad) || Periodicidad < 1 || Periodicidad > 4) {
    stop("Error: Periodicidad debe ser un número entre 1 y 4.")
  }
  if (!is.numeric(TipoModelo) || TipoModelo < 1 || TipoModelo > 2) {
    stop("Error: TipoModelo debe ser 1 o 2.")
  }
  if (!is.numeric(TipoOutput) || TipoOutput < 0 || TipoOutput > 1) {
    stop("Error: TipoOutput debe ser 0 o 1.")
  }
  
  #-------------------------->>>
  # PREPARACION DE DATOS Y PARAMETROS
  #-------------------------->>>
  
  library(tseries)
  library(mFilter)
  library(xts)  
  library(TTR)   
  
  #-------------------------->>>   
  # [1] PREPARACION DE DATOS Y PARAMETROS  
  #-------------------------->>>  
  
  pPeriodicidad <- c(1,2,4,12)
  pTipoDescomp  <- c("additive","multiplicative")
  SetDatosX     <- na.omit(as.numeric(SetDatosX))  
  SetDatosX_TS  <- ts(SetDatosX,frequency=pPeriodicidad[Periodicidad], 1)
  
  #-------------------------->>> 
  # [2] PROCEDIMIENTO ANALITICO
  #-------------------------->>> 

    Modelo  <- decompose(
                        SetDatosX_TS,
                        type=pTipoDescomp[TipoModelo]
                        ) 

  #-------------------------->>> 
  # [3] PREPARACION DE RESULTADOS
  #-------------------------->>> 
  
  if (TipoOutput == 0){
    
    OutPut  <- cbind(Modelo$trend, Modelo$seasonal,Modelo$random)
    
  }else if(TipoOutput == 1){      
    
    OutPut  <- cbind(Modelo$figure)
  
  }else if(TipoOutput >= 1){   
    
    OutPut <- "Revisar parámetros disponibles" 
    
  }    
      
  #-------------------------->>> 
  # [4] RESULTADO FINAL
  #-------------------------->>> 
  
  return(OutPut)  
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

Detalle <- "Aplica diversos filtros a una Serie de Tiempo"

attr(ST_Descomposicion, "description" ) = 
  list( 
      Detalle,
      SetDatosX       = "Serie de tiempo observada",
      Periodicidad    = "Periodicidad de los datos 1: Anual, 2: Semestral, 3: Trimestral 4:Mensual",
      TipoModelo      = "1: Aditivo, 2:Multiplicativo",
      TipoOutput      = "0:Modelo, 1:Efectos Estacionales"
      )

####### 

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
  # VALIDACIONES
  #-------------------------->>>
  
  if (missing(SetDatosX)) {
    stop("Error: SetDatosX es un parámetro obligatorio.")
  }
  if (!is.vector(SetDatosX)) {
    stop("Error: SetDatosX debe ser un vector.")
  }
  if (!is.numeric(Periodicidad) || Periodicidad < 1 || Periodicidad > 4) {
    stop("Error: Periodicidad debe ser un número entre 1 y 4.")
  }
  if (!is.numeric(TipoModelo) || TipoModelo < 1 || TipoModelo > 5) {
    stop("Error: TipoModelo debe ser un número entre 1 y 5.")
  }
  if (!is.numeric(Drift) || Drift < 0 || Drift > 1) {
    stop("Error: Drift debe ser 0 o 1.")
  }
  if (!is.numeric(RaizUnitaria) || RaizUnitaria < 0 || RaizUnitaria > 1) {
    stop("Error: RaizUnitaria debe ser 0 o 1.")
  }
  if (!is.numeric(TipoOutput) || TipoOutput < 0 || TipoOutput > 1) {
    stop("Error: TipoOutput debe ser 0 o 1.")
  }
  
  #-------------------------->>>
  # PREPARACION DE DATOS Y PARAMETROS
  #-------------------------->>>
  
  library(tseries) 
  library(mFilter)
  library(xts)  
  library(TTR)  
  
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
  
  SetDatosX_TS <- ts(SetDatosX,frequency=pPeriodicidad[Periodicidad], 1)

  #-------------------------->>> 
  # [2] PROCEDIMIENTO ANALITICO
  #-------------------------->>> 
  
  if (TipoModelo == 1){ 
    
    Modelo <- mFilter(SetDatosX_TS, 
    filter <- pFiltro[TipoModelo],
    freq   <- pLambda[Periodicidad], 
    root   <- pRaizUnitaria[RaizUnitaria+1],
    drift  <- pDrift[Drift+1]) 
  
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
    
    OutPut  <- cbind(Modelo$trend, Modelo$cycle,Modelo$lambda)
              
  }else if(TipoOutput > 0){      
    
    OutPut <- "Revisar par?metros disponibles" 
     
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