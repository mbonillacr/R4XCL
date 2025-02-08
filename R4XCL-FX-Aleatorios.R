
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# DISTANCIA                                            +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

FX_Distancias <- function(SetDatosX,
                          pEscalaDatos=1,
                          pTipoDistancia=1,
                          pPotenciaMinkowski=2)
    
  {

    #-------------------------->>>   
    # [1] PREPARACION DE DATOS Y PARAMETROS  
    #-------------------------->>>  

    DIR_ORIG = "~/BERT2/functions/INTERNO/"
    ARCHIVO  = paste0(DIR_ORIG,"R4XCL-INTERNO.R")
    FUENTE01 = file.path(ARCHIVO)
    source(FUENTE01)
    
    DT = R4XCL_INT_DATOS(SetDatosX)
    DT = data.frame(DT)
    P  = ncol(DT)
    N  = nrow(DT)
    DT = matrix(as.numeric(unlist(DT)), nrow=N, ncol=P)
    
    if (pEscalaDatos==1){
    DT.ESCALADO = scale(DT, center = TRUE, scale = TRUE)
    } else { DT.ESCALADO = DT}
    
    #-------------------------->>> 
    # [2] PROCEDIMIENTO ANALITICO
    #-------------------------->>> 
    
    TipoDistancia=c("euclidean", "maximum", "manhattan", "canberra", "binary","minkowski")
    
    #-------------------------->>> 
    # [3] PREPARACION DE RESULTADOS
    #-------------------------->>> 
  
    DISTANCIAS = as.matrix(
                           dist(
                                DT.ESCALADO, 
                                method = TipoDistancia[pTipoDistancia],
                                diag = TRUE, 
                                upper = TRUE
                                )
                           )
    
    OutPut = DISTANCIAS
    
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
  # [1] PREPARACION DE DATOS Y PARAMETROS  
  #-------------------------->>>  
  

  
  #-------------------------->>> 
  # [2] PROCEDIMIENTO ANALITICO
  #-------------------------->>> 
  
  
  #-------------------------->>> 
  # [3] PREPARACION DE RESULTADOS
  #-------------------------->>> 
  
  set.seed(Semilla)
  Aleatorios = runif(N,Min,Max)
  
  if (Histograma==0)
    {OutPut =  Aleatorios}
  else
    {OutPut = hist(Aleatorios)} 
  
  #_________________________________________________________________   
  # [4] RESULTADO FINAL
  #_________________________________________________________________
  
  return(OutPut)
  
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

Detalle = "Genera N datos aleatorios a partir de la Funcion de Distribución elegida"

attr(FX_AleatorioUniforme, "description") = 
  list(Detalle,
       N = "Cantidad de Datos a Generar",
       Min = "Valor Mínimo",
       Max = "Valor Máximo",
       Semilla = "Semilla de generación aleatoria (útil para re generar los mismos resultados)",
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
  
  #-------------------------->>>   
  # [1] PREPARACION DE DATOS Y PARAMETROS  
  #-------------------------->>>  
  
  
  
  #-------------------------->>> 
  # [2] PROCEDIMIENTO ANALITICO
  #-------------------------->>> 
  
  
  #-------------------------->>> 
  # [3] PREPARACION DE RESULTADOS
  #-------------------------->>> 
  
  set.seed(Semilla)
  Aleatorios = rnorm(N,Mu,Sigma)
  
  if (Histograma==0)
    {OutPut =  Aleatorios}
  else
    {OutPut = hist(Aleatorios)} 
  
  #_________________________________________________________________   
  # [4] RESULTADO FINAL
  #_________________________________________________________________
  
  return(OutPut)
  
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

Detalle = "Genera N datos aleatorios a partir de la Funcion de Distribución elegida"

attr(FX_AleatorioUniforme, "description") = 
  list(Detalle,
       N = "Cantidad de Datos a Generar",
       Min = "Valor Mínimo",
       Max = "Valor Máximo",
       Semilla = "Semilla de generación aleatoria (útil para re generar los mismos resultados)",
       Histograma= "0:NO, 1:SI, ")

FX_Muestreo = function(SetDatos, Semilla, Porc_Muestral, TipoOutput=0)
{
  
  library(stargazer)
  
  #-------------------------->>>   
  # [1] PREPARACION DE DATOS Y PARAMETROS  
  #-------------------------->>>
  
  DT = R4XCL_INT_DATOS(SetDatosX=SetDatos)
  
  DT = data.frame(DT)
  P  = ncol(DT)
  DT = matrix(as.numeric(unlist(DT)), nrow=nrow(DT), ncol=P)
  
  N=nrow(DT)
  
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
    
    A=data.frame(DT[training.indices,])
    colnames(A)=SetDatos[1,]
    A$id = training.indices
    OutPut= A
    
  }else if(TipoOutput == 1){
    
    A = data.frame(DT[testing.indices,])
    colnames(A)=SetDatos[1,]
    A$id = testing.indices
    OutPut= A
    
  }else if(TipoOutput > 2){     
    
    OutPut = "Revisar parámetros disponibles"
    
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