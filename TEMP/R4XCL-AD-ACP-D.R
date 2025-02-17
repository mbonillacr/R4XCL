#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# ANALISIS DE COMPONENTES PRINCIPALES                  +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

AD_ACP.D <- function(
    SetDatosX, 
    Escala=0,
    Filtro=NULL,
    SetDatosPredecir=NULL
)
{
  library(PerformanceAnalytics)
  library(svDialogs)
  
  #-------------------------->>>   
  # VALIDACIONES
  #-------------------------->>>  
  
  if (!is.data.frame(SetDatosX)) {
    stop("Error: SetDatosX debe ser un data frame.")
  }
  
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
  
  #-------------------------->>> 
  # [2] PROCEDIMIENTO ANALITICO
  #-------------------------->>> 
  
  res.pca   = princomp(DT,cor = TRUE)
  
  #-------------------------->>> 
  # [3] PREPARACION DE RESULTADOS
  #-------------------------->>> 
  
  A = dlg_list(Procedimientos$ACP)
  TipoOutput=A$res
  
  if (TipoOutput == Procedimientos$ACP[1]){
    
    OutPut =  cor(DT)
    
  }else if(TipoOutput == Procedimientos$ACP[2]){
    
    OutPut =  cov(DT)
    
  }else if(TipoOutput == Procedimientos$ACP[3]){   
    
    chart.Correlation(DT, histogram=TRUE)
    OutPut =  "Grafico de Correlaciones Ejecutado"
    
  }else if(TipoOutput == Procedimientos$ACP[4]){
    
    A       = res.pca$loadings
    SIGMA_2 = res.pca$sdev^2
    OutPut  = rbind(A,SIGMA_2) 
    
  }else if(TipoOutput == Procedimientos$ACP[5]){ 
    
    #:::::::::::::::::::::::::::::::    
    # Coordenadas de Individuos
    #:::::::::::::::::::::::::::::::
    
    OutPut =  res.pca$scores
    
  }else if(TipoOutput == Procedimientos$ACP[6]){ 
    
    #:::::::::::::::::::::::::::::::    
    # COS^2 de Individuos
    #:::::::::::::::::::::::::::::::
    
    # CENTROS DE GRAVEDAD
    getd2 = function(x_i, center, scale){return(sum(((x_i-center)/scale)^2))}
    
    x_i   = res.pca$scores
    
    cos2  = function(x_i, d2){return(x_i^2/d2)}
    
    center= res.pca$center
    scale = res.pca$scale
    
    d2    = apply(DT,1,getd2, center, scale)
    
    OutPut= apply(x_i, 2, cos2, d2)
    
  }else if(TipoOutput == Procedimientos$ACP[7]){   
    
    #:::::::::::::::::::::::::::::::
    # Contribuci?n de individuos
    #:::::::::::::::::::::::::::::::
    
    x_i   = res.pca$scores
    
    contrib <- function(x_i, comp.sdev, n.ind){100*(1/n.ind)*x_i^2/comp.sdev^2}
    
    OutPut = t(apply(x_i, 1, contrib, res.pca$sdev, nrow(x_i)))    
    
  }else if(TipoOutput == Procedimientos$ACP[8]){  
    
    #:::::::::::::::::::::::::::::::    
    # Coordenadas de Variables
    #:::::::::::::::::::::::::::::::
    
    OutPut = cbind("Valores Propios"=res.pca$sdev^2,
                   "Aporte a la Varianza"=res.pca$sdev^2/sum(res.pca$sdev^2),
                   "Aporte Acumulado"=cumsum(res.pca$sdev^2/sum(res.pca$sdev^2)))
    
  }else if(TipoOutput == Procedimientos$ACP[9]){    
    
    #:::::::::::::::::::::::::::::::    
    # COS^2 de variables
    #:::::::::::::::::::::::::::::::
    
    var_coord_func <- function(loadings, comp.sdev){loadings*comp.sdev}
    
    loadings  <- res.pca$loadings
    sdev      <- res.pca$sdev
    var.coord <- t(apply(loadings, MARGIN = 1, var_coord_func, sdev)) 
    OutPut    <- var.coord^2
    
  }else if(TipoOutput == Procedimientos$ACP[10]){     
    
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
    
  }else if(TipoOutput == Procedimientos$ACP[11]){  
    
    if(missing(SetDatosPredecir)){
      
      OutPut = res.pca$scores
      
    }else{
      
      DatosX = SetDatosPredecir[-1,]
      
      DatosX = matrix(as.numeric(DatosX), nrow=nrow(DatosX), ncol=p)
      DatosX = data.frame(DatosX)
      colnames(DatosX)[1:p]=nombresX[1:p]
      
      A = predict(res.pca, newdata = ind.sup)
      OutPut=data.frame("R4XCL_PrediccionFueraDeMuestra"= A)
      
    } 
    
  }else if(TipoOutput == Procedimientos$ACP[12]){    
    
    biplot(res.pca, scale = 0)
    OutPut="Biplot Ejecutado"
    
  }  
  
  #_________________________________________________________________   
  # [4] RESULTADO FINAL
  #_________________________________________________________________
  
  return(OutPut)
  
}

attr(AD_ACP.D, "description") = 
  list(
        Detalle          = "Análisis de Componentes Principales [ACP]",
        SetDatosX        = "Datos por Analizar",
        Escala           = "Escalar datos? 1:SI, 0:NO (0:Default)",
        Filtro           = "0:Incluir registro, 1:Excluir registro (0:Default)", 
        SetDatosPredecir = "Computar datos fuera de muestra"
      )