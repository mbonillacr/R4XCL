#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# ALGORITMO DE K MEDIAS                                +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

AD_KMedias.D <- function(
                         SetDatosX,
                         Escala=0,
                         Filtro=NULL,
                         K=3,
                         Koptimo=10,
                         Semilla=123456,
                         TipoModelo=1
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
  if (!is.numeric(Escala) || Escala < 0 || Escala > 1) {
    stop("Error: Escala debe ser 0 o 1.")
  }
  if (!is.numeric(K) || K <= 0) {
    stop("Error: K debe ser un número positivo.")
  }
  if (!is.numeric(Koptimo) || Koptimo <= 0) {
    stop("Error: Koptimo debe ser un número positivo.")
  }
  if (!is.numeric(Semilla)) {
    stop("Error: Semilla debe ser un número.")
  }
  if (!is.numeric(TipoModelo) || TipoModelo < 1 || TipoModelo > 4) {
    stop("Error: TipoModelo debe ser un número entre 1 y 4.")
  }
  
  #-------------------------->>>
  # PREPARACION DE DATOS Y PARAMETROS
  #-------------------------->>>

  library(cluster)
  library(svDialogs)

  
  #-------------------------->>>   
  # [1] PREPARACION DE DATOS Y PARAMETROS  
  #-------------------------->>>  
  
  set.seed(Semilla)
  
  DT = R4XCL_INT_DATOS(
                        SetDatosX=SetDatosX,
                        Escala=Escala,
                        Filtro=Filtro
                      )

  P <- ncol(DT)
  N <- nrow(DT)
  
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
  
  A <- dlg_list(Procedimientos$KMEDIAS)
  TipoOutput<-A$res
  
  if (TipoOutput == Procedimientos$KMEDIAS[1]){

    A <- res.kmns$cluster
    OutPut <- data.frame("Cluster Asignado" = A )
    
  } else if (TipoOutput == Procedimientos$KMEDIAS[2]){
    
    A <- res.kmns$centers
    nombresX <- paste0(SetDatosX[1,1:P])
    colnames(A) <- nombresX
    OutPut <- data.frame("Centros"= A )
    
  } else if (TipoOutput == Procedimientos$KMEDIAS[3]){   
    
    OutPut <- data.frame(
                          "Cluster"=seq(1:K),
                          "Variabilidad Dentro del Cluster" = res.kmns$withinss,
                          "N en cada cluster" = res.kmns$size
                        )
    
  } else if (TipoOutput == Procedimientos$KMEDIAS[4]){
    
    OutPut <- rbind(
                  "Variabilidad TOTAL"=res.kmns$totss,
                  "Variabilidad TOTAL intra clases"=res.kmns$tot.withinss,
                  "Variabilidad TOTAL entre clases"=res.kmns$betweenss
                  )
    
  } else if (TipoOutput == Procedimientos$KMEDIAS[5]){
    
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
    
  } else if (TipoOutput == Procedimientos$KMEDIAS[6]){
    
    res.kopt <- clusGap(DT, kmeans, Koptimo, B = 100, verbose = interactive())
    b <- data.frame(res.kopt$Tab)
    b$gap.min <- b[,3]-b[,4]
    b$gap.max <- b[,3]+b[,4]
    
    OutPut <- b
    
  } else if (TipoOutput == Procedimientos$KMEDIAS[7]){    
    
    a<-NULL
    for (i in 1:Koptimo){
                          a[i]<-kmeans(DT,i)$tot.withinss
                        }
    
    OutPut = data.frame("iteracion"=1:Koptimo,"Var.Entre"=a)
  
  } else if (TipoOutput == Procedimientos$KMEDIAS[8]){ 
    
    ListaM <- c(Modelo)
    ListaN <- c("BINARIO")
    
    OutPut <- R4XCL_INT_CREARDS(
                                ListaM,                        
                                ListaN
                              )
    
  } else if (TipoOutput == Procedimientos$KMEDIAS[9]){ 
    
    FX= rbind(
              paste0("Centros: ", K), 
              paste0("Algoritmo: ", Metodos[TipoModelo])
              )
    
    OutPut  <- FX #R4XCL_INT_INFO_EJECUCION(FX, DT)    

  } 

  #-------------------------->>>
  # RESULTADO FINAL
  #-------------------------->>>
  
  return(OutPut)
  
}


DialogosXCL=R4XCL_INT_DIALOGOS()

attr(AD_KMedias.D, DialogosXCL$Descripcion) = 
  list(
    Detalle     = DialogosXCL$Detalle.KM,
    SetDatosX   = DialogosXCL$SetDatosX.NS,
    Escala      = DialogosXCL$Escala,
    Filtro      = DialogosXCL$Filtro,
    K           = DialogosXCL$K,
    Koptimo     = DialogosXCL$Koptimo,
    Semilla     = DialogosXCL$Semilla,
    TipoModelo  = DialogosXCL$TipoModelo.KM
  )

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++ 