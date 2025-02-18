
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# DISTANCIA                                            +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

FX_Distancias <- function(SetDatosX,
                          pEscalaDatos=1,
                          pTipoDistancia=1,
                          pPotenciaMinkowski=2)
    
  {

    #-------------------------->>>   
    # VALIDACIONES
    #-------------------------->>>  
    # if (!is.data.frame(SetDatosX)) {
    #   stop("Error: SetDatosX debe ser un data frame.")
    # }
    # if (!is.numeric(pEscalaDatos) || pEscalaDatos < 0 || pEscalaDatos > 1) {
    #   stop("Error: pEscalaDatos debe ser 0 o 1.")
    # }
    # if (!is.numeric(pTipoDistancia) || pTipoDistancia < 1 || pTipoDistancia > 6) {
    #   stop("Error: pTipoDistancia debe ser un número entre 1 y 6.")
    # }
    # if (!is.numeric(pPotenciaMinkowski) || pPotenciaMinkowski <= 0) {
    #   stop("Error: pPotenciaMinkowski debe ser un número positivo.")
    # }
  
    #-------------------------->>>   
    # [1] PREPARACION DE DATOS Y PARAMETROS  
    #-------------------------->>>  

    DIR_ORIG <- "~/BERT2/functions/INTERNO/"
    ARCHIVO  <- paste0(DIR_ORIG,"R4XCL-INTERNO.R")
    FUENTE01 <- file.path(ARCHIVO)
    source(FUENTE01)
    
    DT <- R4XCL_INT_DATOS(SetDatosX)
    DT <- data.frame(DT)
    P  <- ncol(DT)
    N  <- nrow(DT)
    DT <- matrix(as.numeric(unlist(DT)), nrow=N, ncol=P)
    
    if (pEscalaDatos==1){
    DT.ESCALADO <- scale(DT, center = TRUE, scale = TRUE)
    } else { DT.ESCALADO <- DT}
    
    #-------------------------->>> 
    # [2] PROCEDIMIENTO ANALITICO
    #-------------------------->>> 
    
    TipoDistancia <- c("euclidean", "maximum", "manhattan", "canberra", "binary","minkowski")
    
    #-------------------------->>> 
    # [3] PREPARACION DE RESULTADOS
    #-------------------------->>> 
  
    DISTANCIAS <- as.matrix(
                           dist(
                                DT.ESCALADO, 
                                method = TipoDistancia[pTipoDistancia],
                                diag = TRUE, 
                                upper = TRUE
                                )
                           )
    
    OutPut <- DISTANCIAS
    
    #_________________________________________________________________   
    # [4] RESULTADO FINAL
    #_________________________________________________________________
    
    return(OutPut)
    
  }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

Detalle = "Calcula distancias entre individuos"

attr(FX_Distancias, "description") = 
  list(Detalle,
       SetDatosX = "Datos por Analizar",
       pEscalaDatos = "Normalizar Datos: 0-NO, 1-SI",
       pTipoDistancia= "1:Euclidea, 2:Maxima, 3: Manhattan, 4:Canberra, 5: Binaria, 6: Minkowski",
       pPotenciaMinkowski= "Potencia distancia Minkowski")


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# GENERACION ALEATORIO: UNIFORME                       +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

FX_AleatorioUniforme <- function(
                                 N,
                                 Min=0,
                                 Max=1,
                                 Semilla=NULL,
                                 Histograma=0
                                 )
  
  {
    #-------------------------->>>   
    # VALIDACIONES
    #-------------------------->>>  
    # if (!is.numeric(N) || N <= 0) {
    #   stop("Error: N debe ser un número positivo.")
    # }
    # if (!is.numeric(Min) ||!is.numeric(Max) || Min >= Max) {
    #   stop("Error: Min debe ser menor que Max.")
    # }
    # if (!is.null(Semilla) &&!is.numeric(Semilla)) {
    #   stop("Error: Semilla debe ser un número.")
    # }
    # if (!is.numeric(Histograma) || Histograma < 0 || Histograma > 1) {
    #   stop("Error: Histograma debe ser 0 o 1.")
    # }
    
  #--------------------------   
  # PREPARACION DE PARAMETROS  
  #--------------------------  
  
  set.seed(Semilla)
  
  #-------------------------- 
  # PROCEDIMIENTO ANALITICO
  #--------------------------
  
  Aleatorios <- runif(N, Min, Max)
  
  #--------------------------
  # RESULTADOS
  #-------------------------- 
  
  if (Histograma == 0) {
    OutPut <- Aleatorios
  } else {
    OutPut <- hist(Aleatorios)
  }
  
  #--------------------------  
  # RESULTADO FINAL
  #-------------------------- 
  
  return(OutPut)
  
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

Detalle = "Genera N datos aleatorios a partir de la Función de Distribución elegida"

attr(FX_AleatorioUniforme, "description") = 
  list(Detalle,
       N = "Cantidad de Datos a Generar",
       Min = "Valor Mínimo",
       Max = "Valor Máximo",
       Semilla = "Semilla de generación aleatoria (?til para re generar los mismos resultados)",
       Histograma= "0:NO, 1:SI, ")


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# GENERACION ALEATORIO: UNIFORME                       +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

FX_AleatorioNormal <- function(
                                N=100,
                                Mu=0,
                                Sigma=1,
                                Semilla=123456,
                                Histograma=0
                              )
  
{
  
  #--------------------------   
  # VALIDACIONES
  #--------------------------
  # 
  # if (!is.numeric(N) || N <= 0) {
  #   stop("Error: N debe ser un número positivo.")
  # }
  # if (!is.numeric(Mu) ||!is.numeric(Sigma) || Sigma <= 0) {
  #   stop("Error: Sigma debe ser un número positivo.")
  # }
  # if (!is.null(Semilla) &&!is.numeric(Semilla)) {
  #   stop("Error: Semilla debe ser un número.")
  # }
  # if (!is.numeric(Histograma) || Histograma < 0 || Histograma > 1) {
  #   stop("Error: Histograma debe ser 0 o 1.")
  # }
  
  #--------------------------   
  # PREPARACION DE PARAMETROS  
  #--------------------------  
  
  set.seed(Semilla)
  
  #-------------------------- 
  # PROCEDIMIENTO ANALITICO
  #--------------------------
  
  Aleatorios = rnorm(N,Mu,Sigma)
  
  #-------------------------- 
  # RESULTADOS
  #-------------------------- 
  
  if (Histograma==0)
    {OutPut =  Aleatorios}
  else
    {OutPut = hist(Aleatorios)} 
  
  #--------------------------  
  # RESULTADO FINAL
  #-------------------------- 
  
  return(OutPut)
  
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

Detalle = "Genera N datos aleatorios a partir de la Función de Distribución elegida"

attr(FX_AleatorioUniforme, "description") = 
  list(Detalle,
       N = "Cantidad de Datos a Generar",
       Min = "Valor Mínimo",
       Max = "Valor Máximo",
       Semilla = "Semilla de generación aleatoria (útil para re generar los mismos resultados)",
       Histograma= "0:NO, 1:SI, ")

FX_Muestreo = function(SetDatos, Semilla, Porc_Muestral, TipoOutput=0)
{
  
  #--------------------------   
  # VALIDACIONES
  #--------------------------
  # 
  # if (!is.data.frame(SetDatos)) {
  #   stop("Error: SetDatos debe ser un data frame.")
  # }
  # if (!is.numeric(Semilla)) {
  #   stop("Error: Semilla debe ser un número.")
  # }
  # if (!is.numeric(Porc_Muestral) || Porc_Muestral <= 0 || Porc_Muestral >= 1) {
  #   stop("Error: Porc_Muestral debe ser un número entre 0 y 1.")
  # }
  # if (!is.numeric(TipoOutput) || TipoOutput < 0 || TipoOutput > 1) {
  #   stop("Error: TipoOutput debe ser 0 o 1.")
  # }
  
  #--------------------------   
  # PREPARACION DE DATOS Y PARAMETROS  
  #--------------------------
  
  DT <- R4XCL_INT_DATOS(SetDatosX=SetDatos)
  DT <- data.frame(DT)
  P  <- ncol(DT)
  DT <- matrix(as.numeric(unlist(DT)), nrow=nrow(DT), ncol=P)
  N  <- nrow(DT)
  
  set.seed(Semilla)
  random<-sample(1:N)
  
  num.Datos.training<-as.integer(Porc_Muestral*N)
  
  #-------------------------->>> 
  # [2] PROCEDIMIENTO ANALITICO
  #-------------------------->>> 
  
  training.indices<-random[1:num.Datos.training]
  testing.indices <-random[(num.Datos.training+1):N]
  
  #-------------------------->>> 
  # [3] PREPARACION DE RESULTADOS
  #-------------------------->>> 
  
  if (TipoOutput == 0){
    
    A<-data.frame(DT[training.indices,])
    colnames(A)<-SetDatos[1,]
    A$id <- training.indices
    OutPut<- A
    
  }else if(TipoOutput == 1){
    
    A <- data.frame(DT[testing.indices,])
    colnames(A) <- SetDatos[1,]
    A$id <- testing.indices
    OutPut <- A
    
  }else if(TipoOutput > 2){     
    
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

Detalle = "Construye dos muestras del set de datos Original"
attr(FX_Muestreo, "description" ) = 
  list( 
    Detalle,
    SetDatosX = "Datos a procesar",
    Semilla = "Semilla de generación aleatoria",
    Porc_Muestral = "% de la Muestra que será empleada para Entrenar el modelo",
    TipoOutput= "0:Datos de Entrenamiento, 
                 1:Datos de Prueba"
  )