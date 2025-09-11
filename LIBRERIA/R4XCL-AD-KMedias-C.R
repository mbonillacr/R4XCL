AD_KMedias.C <- function(
    SetDatosX,
    Escala=0,
    Filtro=0,
    K=3,
    Koptimo=10,
    Semilla=123456,
    TipoModelo=1,
    TipoOutput=0
)
  
{
  
  #-------------------------->>>
  # VALIDACIONES
  #-------------------------->>>
  # if (missing(SetDatosX)) {
  #   stop("Error: SetDatosX es un parámetro obligatorio.")
  # }
  # if (!is.data.frame(SetDatosX)) {
  #   stop("Error: SetDatosX debe ser un data frame.")
  # }
  # if (!is.numeric(Escala) || Escala < 0 || Escala > 1) {
  #   stop("Error: Escala debe ser 0 o 1.")
  # }
  # if (!is.numeric(K) || K <= 0) {
  #   stop("Error: K debe ser un número positivo.")
  # }
  # if (!is.numeric(Koptimo) || Koptimo <= 0) {
  #   stop("Error: Koptimo debe ser un número positivo.")
  # }
  # if (!is.numeric(Semilla)) {
  #   stop("Error: Semilla debe ser un número.")
  # }
  # if (!is.numeric(TipoModelo) || TipoModelo < 1 || TipoModelo > 4) {
  #   stop("Error: TipoModelo debe ser un número entre 1 y 4.")
  # }
  # if (!is.numeric(TipoOutput) || TipoOutput < 0 || TipoOutput > 9) {
  #   stop("Error: TipoOutput debe ser un número entre 0 y 9.")
  # }
  
  #-------------------------->>>
  # PREPARACION DE DATOS Y PARAMETROS
  #-------------------------->>>
  
  library(cluster)
  
  #-------------------------->>>   
  # [1] PREPARACION DE DATOS Y PARAMETROS  
  #-------------------------->>>  
  
  
  set.seed(Semilla)
  
  DT <- R4XCL_INT_DATOS(
    SetDatosX=SetDatosX,
    Escala=Escala,
    Filtro=Filtro
  )
  
  P  <- ncol(DT)
  N  <- nrow(DT)
  
  Procedimientos <- R4XCL_INT_PROCEDIMIENTOS()
  
  #-------------------------->>> 
  # [2] PROCEDIMIENTO ANALITICO
  #-------------------------->>> 
  
  Metodos  <- c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen")
  res.kmns <- kmeans(
    DT,
    center= K,
    algorithm = Metodos[TipoModelo]
  )
  
  #-------------------------->>> 
  # [3] PREPARACION DE RESULTADOS
  #-------------------------->>> 
  if (TipoOutput <= 0){
    
    OutPut <- data.frame("PROCESOS"= Procedimientos$KMEDIAS)
    
  }else if(TipoOutput == 1){
    
    A      <- res.kmns$cluster
    OutPut <- data.frame("Cluster Asignado" = A)
    
  }else if(TipoOutput == 2){
    
    A           <- res.kmns$centers
    nombresX    <- paste0(SetDatosX[1,1:P])
    colnames(A) <- nombresX
    OutPut      <- data.frame("Centros"= A )
    
  }else if(TipoOutput == 3){   
    
    OutPut <- data.frame(
      "Cluster"=seq(1:K),
      "Variabilidad Dentro del Cluster" = res.kmns$withinss,
      "N en cada cluster" = res.kmns$size
    )
    
  }else if(TipoOutput == 4){
    
    OutPut <- rbind(
      "Variabilidad TOTAL"=res.kmns$totss,
      "Variabilidad TOTAL intra clases"=res.kmns$tot.withinss,
      "Variabilidad TOTAL entre clases"=res.kmns$betweenss
    )
    
  }else if(TipoOutput == 5){
    
    if(Escala==0)
    {
      mu    <- unlist(rep(list(0),P))
      sigma <- unlist(rep(list(1),P))
      
      OutPut = rbind(mu, sigma)
    } 
    else 
    { 
      mu    <- attr(DT,"scaled:center")
      sigma <- attr(DT,"scaled:scale")
      
      OutPut = rbind(mu, sigma)
    }
    
  }else if(TipoOutput == 6){
    
    res.kopt <- clusGap(DT, kmeans, Koptimo, B = 100, verbose = interactive())
    b <- data.frame(res.kopt$Tab)
    b$gap.min <- b[,3]-b[,4]
    b$gap.max <- b[,3]+b[,4]
    
    OutPut <- b
    
  }else if(TipoOutput == 7){    
    
    a=NULL
    for (i in 1:Koptimo){
      a[i] <- kmeans(DT,i)$tot.withinss
    }
    
    OutPut <- data.frame("iteracion"=1:Koptimo,"Var.Entre"=a)
    
  } else if (TipoOutput == 8){ 
    
    ListaM <- c(Modelo)
    ListaN <- c("BINARIO")
    
    OutPut <- R4XCL_INT_CREARDS(ListaM,ListaN)
    
  } else if (TipoOutput == 9){ 
    
    FX <- rbind(
                paste0("Centros: ", K), 
                paste0("Algoritmo: ", Metodos[TipoModelo])
                )
    
    OutPut  <- FX   
    
  } else if (TipoOutput > 9){   
    
    OutPut = "Revisar parámetros disponibles"    
    
  }  
  
  #-------------------------->>>
  # RESULTADO FINAL
  #-------------------------->>>
  
  return(OutPut)
  
}

DialogosXCL=R4XCL_INT_DIALOGOS()
attr(AD_KMedias.C, "description") = 
  list(
    Detalle     = DialogosXCL$Detalle.KM,
    SetDatosX   = DialogosXCL$SetDatosX.NS,
    Escala      = DialogosXCL$Escala,
    Filtro      = DialogosXCL$Filtro,
    K           = DialogosXCL$K,
    Koptimo     = DialogosXCL$Koptimo,
    Semilla     = DialogosXCL$Semilla,
    TipoModelo  = DialogosXCL$TipoModelo.KM,
    TipoOutput  = DialogosXCL$TipoOutput.KM
  )


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# ALGORITMO DE K MEDIAS FUERA DE MUESTRA               +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

AD_KmediasClasificar = function(SetDatosPredecir,
                                Escala=0,
                                Centroides=NULL,
                                FactorMu=0,
                                FactorSigma=1)
{
  #-------------------------->>>
  # VALIDACIONES
  #-------------------------->>>
  # if (missing(SetDatosPredecir)) {
  #   stop("Error: SetDatosPredecir es un parámetro obligatorio.")
  # }
  # if (!is.data.frame(SetDatosPredecir)) {
  #   stop("Error: SetDatosPredecir debe ser un data frame.")
  # }
  # if (is.null(Centroides)) {
  #   stop("Error: Centroides es un parámetro obligatorio.")
  # }
  # if (!is.data.frame(Centroides)) {
  #   stop("Error: Centroides debe ser un data frame.")
  # }
  # if (!is.numeric(Escala) || Escala < 0 || Escala > 1) {
  #   stop("Error: Escala debe ser 0 o 1.")
  # }
  # if (!is.numeric(FactorMu)) {
  #   stop("Error: FactorMu debe ser un número.")
  # }
  # if (!is.numeric(FactorSigma) || FactorSigma <= 0) {
  #   stop("Error: FactorSigma debe ser un número positivo.")
  # }
  
  #-------------------------->>>
  # PREPARACION DE DATOS Y PARAMETROS
  #-------------------------->>>

  DT.p        <- R4XCL_INT_DATOS(SetDatosX=SetDatosPredecir)

  P.p         <- ncol(DT.p)
  N.p         <- nrow(DT.p)
  Centroides  <- Centroides[-1,]
  K           <- nrow(Centroides)
  
  
  DT.p <- matrix(
                  as.numeric(unlist(DT.p)), 
                  nrow=N.p, 
                  ncol=P.p
                )
  
  if(Escala==1){
                mu=FactorMu
                sigma=FactorSigma
                DT.p=scale(DT.p, center = mu,scale = sigma)
               }
  
  #-------------------------->>> 
  # [2] PROCEDIMIENTO ANALITICO
  #-------------------------->>> 
  #
  nombresX <- paste0(SetDatosPredecir[1,1:P.p])
  
  colnames(Centroides) <- nombresX[1:P.p]
  colnames(DT.p) <- nombresX[1:P.p]
  dist_mat  <- as.matrix(dist(rbind(Centroides, DT.p)))
  
  dist_mat  <- dist_mat[-seq(K), seq(K)]
  A <- max.col(-dist_mat)
  
  #-------------------------->>> 
  # [3] PREPARACION DE RESULTADOS
  #-------------------------->>> 
  
  OutPut <- data.frame("R4XCL_PrediccionFueraDeMuestra"= A)
  
  #_________________________________________________________________   
  # [4] RESULTADO FINAL
  #_________________________________________________________________
  
  return(OutPut)
  
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++ 


attr(AD_KmediasClasificar, "description") = 
  list(
    Detalle            = "Algoritmo de K-Medias",
    SetDatosPredecir   = "Datos por Analizar",
    Centroides         = "Centros obtenidos del proceso de K-Medias",
    Escala             = "Escalar datos? 1:SI, 0:NO (0:Default)",
    FactorEscalamiento = "Criterio de Escalamiento Original"
  )
