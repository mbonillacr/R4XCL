#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# ANALISIS DE COMPONENTES PRINCIPALES                  +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

AD_ACP.C <- function(
                     SetDatosX, 
                     Escala=0,
                     Filtro=NULL,
                     TipoOutput=0, 
                     SetDatosPredecir=NULL
                     )
{
  
  library(PerformanceAnalytics)
  
  #-------------------------->>>   
  # VALIDACIONES
  #-------------------------->>>  
  #  
  # if (!is.data.frame(SetDatosX)) {
  #                                 stop("Error: SetDatosX debe ser un data frame.")
  #                                }

  #-------------------------->>>   
  # [1] PREPARACION DE DATOS Y PARAMETROS  
  #-------------------------->>>  
  
  Procedimientos = R4XCL_INT_PROCEDIMIENTOS()

  DT = R4XCL_INT_DATOS(
                        SetDatosX=SetDatosX,
                        Escala=Escala,
                        Filtro=Filtro
                      )

  
  P  = ncol(DT)
  N  = nrow(DT)
  
  #-------------------------->>> 
  # [2] PROCEDIMIENTO ANALITICO
  #-------------------------->>> 
  
  res.pca   = princomp(DT,cor = TRUE)
  
  #-------------------------->>> 
  # [3] PREPARACION DE RESULTADOS
  #-------------------------->>> 
  
  if (TipoOutput <= 0){
    
    OutPut = Procedimientos$ACP
    
  }else if(TipoOutput == 1){
    
    OutPut =  cor(DT)
    
  }else if(TipoOutput == 2){
    
    OutPut =  cov(DT)
    
  }else if(TipoOutput == 3){   
    
    chart.Correlation(DT, histogram=TRUE)
    OutPut =  "Grafico de Correlaciones Ejecutado"
    
  }else if(TipoOutput == 4){
    
    A       = res.pca$loadings
    SIGMA_2 = res.pca$sdev^2
    OutPut  = rbind(A,SIGMA_2)  
    
  }else if(TipoOutput == 5){ 
    
    #:::::::::::::::::::::::::::::::    
    # Coordenadas de Individuos
    #:::::::::::::::::::::::::::::::
    
    OutPut =  res.pca$scores
    
  }else if(TipoOutput == 6){ 
    
    #:::::::::::::::::::::::::::::::    
    # COS^2 de Individuos
    #:::::::::::::::::::::::::::::::
    
    # PCA CENTROS DE GRAVEDAD
    getd2 = function(x_i, center, scale){return(sum(((x_i-center)/scale)^2))}
    
    x_i   = res.pca$scores
    
    cos2  = function(x_i, d2){return(x_i^2/d2)}
    
    center= res.pca$center
    scale = res.pca$scale
    
    d2    = apply(DT,1,getd2, center, scale)
    
    OutPut= apply(x_i, 2, cos2, d2)
    
  }else if(TipoOutput == 7){   
    
    #:::::::::::::::::::::::::::::::
    # Contribucion de individuos
    #:::::::::::::::::::::::::::::::
    
    x_i   = res.pca$scores
    
    contrib <- function(x_i, comp.sdev, n.ind){100*(1/n.ind)*x_i^2/comp.sdev^2}
    
    OutPut = t(apply(x_i, 1, contrib, res.pca$sdev, nrow(x_i)))    
    
  }else if(TipoOutput == 8){  
    
    #:::::::::::::::::::::::::::::::    
    # Coordenadas de Variables
    #:::::::::::::::::::::::::::::::
    
    OutPut = cbind("Valores Propios"=res.pca$sdev^2,
                   "Aporte a la Varianza"=res.pca$sdev^2/sum(res.pca$sdev^2),
                   "Aporte Acumulado"=cumsum(res.pca$sdev^2/sum(res.pca$sdev^2)))
    
  }else if(TipoOutput == 9){    
    
    #:::::::::::::::::::::::::::::::    
    # COS^2 de variables
    #:::::::::::::::::::::::::::::::
    
    var_coord_func <- function(loadings, comp.sdev){loadings*comp.sdev}
    
    loadings  <- res.pca$loadings
    sdev      <- res.pca$sdev
    var.coord <- t(apply(loadings, MARGIN = 1, var_coord_func, sdev)) 
    OutPut    <- var.coord^2
    
  }else if(TipoOutput == 10){     
    
    #:::::::::::::::::::::::::::::::
    # Contribucion de variables
    #:::::::::::::::::::::::::::::::
    
    contrib   <- function(var.cos2, comp.cos2){var.cos2*100/comp.cos2}
    var_coord_func <- function(loadings, comp.sdev){loadings*comp.sdev}
    
    loadings  <- res.pca$loadings
    sdev      <- res.pca$sdev
    var.coord <- t(apply(loadings, MARGIN = 1, var_coord_func, sdev)) 
    var.cos2  <- var.coord^2 
    comp.cos2 <- apply(var.cos2, MARGIN = 2, FUN = sum)
    
    OutPut    <- t(apply(var.cos2, MARGIN = 1, contrib, comp.cos2))
    
  }else if(TipoOutput == 11){  
    
    if(missing(SetDatosPredecir)){
      
      OutPut <- res.pca$scores
      
    }else{
      
      DatosX <- SetDatosPredecir[-1,]
      DatosX <- matrix(as.numeric(DatosX), nrow=nrow(DatosX), ncol=p)
      DatosX <- data.frame(DatosX)
      colnames(DatosX)[1:p]=nombresX[1:p]
      
      A <- predict(res.pca, newdata = ind.sup)
      OutPut <- data.frame("R4XCL_PrediccionFueraDeMuestra"= A)
      
    } 
    
  }else if(TipoOutput == 12){    
    
    biplot(res.pca, scale = 0)
    OutPut <- "Biplot Ejecutado"
    
  }else if(TipoOutput >12){   
    
    OutPut <- "Revisar parámetros disponibles" 
    
  }  
  
  #-------------------------->>>   
  # [4] RESULTADO FINAL
  #-------------------------->>> 
  
  return(OutPut)
  
}

attr(AD_ACP.C, "description") = 
  list(
        Detalle          = "Análisis de Componentes Principales [ACP]",
        SetDatosX        = "Datos por Analizar",
        Escala           = "Escalar datos? 1:SI, 0:NO (0:Default)",
        Filtro           = "0:Incluir registro, 1:Excluir registro (0:Default)", 
        TipoOutput       = "1:Matriz de Correlación, 2:Coordenadas: Variables, 3:Coordenadas: Individuos, 4:COS^2: INDs, 5:Contribuci?n:INDs, 6:Valores Propios, 7:COS^2: VARs, 8:Contribuci?n:VARs, 9:Predicci?n, 10:Gr?fico VARs|INDs", 
        SetDatosPredecir = "Computar datos fuera de muestra"
      )