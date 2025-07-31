#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MODELO LINEAL
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

MR_Lineal <- function(
    SetDatosY, 
    SetDatosX,
    Categorica=0,
    Escala=0,
    Filtro=0,
    SetDatosPredecir=NULL,
    Constante=1,
    TipoOutput=1,
    Ponderadores = NULL
)
{
  #-------------------------->>>
  # PREPARACION DE DATOS Y PARAMETROS
  #-------------------------->>>
  
  library(stargazer)
  
  #-------------------------->>>   
  # [1] PREPARACION DE DATOS Y PARAMETROS  
  #-------------------------->>>  
  
  FX <- R4XCL_INT_FUNCION(SetDatosX,SetDatosY)

  Procedimientos <- R4XCL_INT_PROCEDIMIENTOS()
  
  if (Constante==0){FX=paste0(FX,"-1")}
  
  DT <- R4XCL_INT_DATOS(
                        SetDatosY=SetDatosY,                   
                        SetDatosX=SetDatosX,
                        Escala=Escala,
                        Filtro=Filtro,
                        Categorica=Categorica,
                        Ponderadores = Ponderadores
                      )

  P  <- ncol(DT)
  
  especificacion <- eval(parse(text=FX))
  
  #-------------------------->>> 
  # [2] PROCEDIMIENTO ANALITICO
  #-------------------------->>> 
  
  Modelo <- lm(
              formula = especificacion, 
              data = DT,
              weights = PESOS
              )
  
  Y_Pred <- Modelo$fitted.values
  
  #-------------------------->>> 
  # [3] PREPARACION DE RESULTADOS
  #-------------------------->>> 
  
  if (TipoOutput <= 0){
    
    OutPut = Procedimientos$LINEAL
    
  } else if (TipoOutput == 1){  
    
    OutPut <- data.frame("R4XCL_ModeloEstimado"= stargazer(Modelo, type="text", ci=TRUE, ci.level=0.95,single.row=TRUE))
    
  }else if(TipoOutput == 2){
    
    OutPut <- data.frame("R4XCL_PrediccionDentroDeMuestra"= Y_Pred)
    
  }else if(TipoOutput == 3){ 
    
    if(missing(SetDatosPredecir))
      
    {OutPut <- data.frame("R4XCL_PrediccionDentroDeMuestra"= Y_Pred)
    
    }else{
      
      p        <- ncol(SetDatosPredecir)
      nombresX <- paste0(SetDatosX[1,1:p])
      
      SetDatosPredecir <- SetDatosPredecir[-1,]
      SetDatosPredecir <- matrix(as.numeric(SetDatosPredecir), nrow=nrow(SetDatosPredecir)-1, ncol=p)
      SetDatosPredecir <- data.frame(SetDatosPredecir)
      
      colnames(SetDatosPredecir)[1:p]=nombresX[1:p]
      
      A <- predict(Modelo, newdata = SetDatosPredecir)
      OutPut <- data.frame("R4XCL_PrediccionFueraDeMuestra"= A)
      
    }
    
  }else if(TipoOutput == 4){
    
    OutPut <- sapply(marginal_effects(Modelo, DT),mean)
    OutPut <- data.frame("R4XCL_EfectosMarginales"= capture.output(OutPut))
    
  }else if(TipoOutput == 5){  
    
    require(usdm)
    OutPut <- usdm::vif(DT[,-1]) 
    OutPut <- data.frame("R4XCL_InflacionDeVarianza"= OutPut)
    
  }else if(TipoOutput == 6){  
    
    OutPut <- lmtest::bptest(Modelo) 
    OutPut <- data.frame("R4XCL_Heterocedasticidad"=capture.output(OutPut))    
    
  }else if(TipoOutput == 7){    
    
    cov <- sandwich::vcovHC(Modelo, type = "HC")
    
    robust.se <- sqrt(diag(cov))
    
    OutPut <- stargazer(Modelo, Modelo, 
                     se=list(NULL, robust.se),
                     column.labels=c("OLS","OLS E.S. Robusto"), 
                     type="text",
                     align=TRUE);  
    
  }else if(TipoOutput == 8){ 
    
    A <- influence.measures(Modelo)
    OutPut <- data.frame("R4XCL_ObservacionesDeInfluencia"=A$is.inf)
    
  }else if(TipoOutput == 9){ 
    
    OutPut <- R4XCL_INT_INFO_EJECUCION(FX, DT)  
    
  }else if(TipoOutput == 10){ 
    
    ListaM <- c(Modelo)
    ListaN <- c("LINEAL")
    OutPut <- R4XCL_INT_CREARDS(ListaM,ListaN)
    
  } else if (TipoOutput == 11){  
    
    OutPut <- data.frame("R4XCL_ModeloEstimado"= capture.output(summary(Modelo)))
    
  } else if (TipoOutput == 12){  
    
    OutPut <- data.frame("R4XCL_Residuos"= resid(Modelo))    
    
  }else if(TipoOutput > 12){   
    
    OutPut <- "Revisar par?metros disponibles" 
    
  }  
  
  #-------------------------->>> 
  # [4] RESULTADO FINAL
  #-------------------------->>> 
  
  return(OutPut);  
}

DialogosXCL <- R4XCL_INT_DIALOGOS()

attr(MR_Lineal, DialogosXCL$Descripcion) = 
  
  list(
    Detalle.Lineal  = DialogosXCL$Detalle,
    SetDatosY       = DialogosXCL$SetDatosY,
    SetDatosX       = DialogosXCL$SetDatosX,
    Categorica      = DialogosXCL$Categorica,
    Escala          = DialogosXCL$Escala,
    Filtro          = DialogosXCL$Filtro,
    SetDatosPredecir= DialogosXCL$SetDatosPredecir,
    Constante       = DialogosXCL$Constante,
    TipoOutput      = DialogosXCL$TipoOutput.Lineal,
    Ponderadores    = DialogosXCL$Ponderadores
  )

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MODELO LINEAL
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

MR_SVM <- function(
                   SetDatosY, 
                   SetDatosX,
                   Filtro = NULL,
                   TipoOutput=0
                  )
{
  #-------------------------->>>
  # PREPARACION DE DATOS Y PARAMETROS
  #-------------------------->>>
  
  # https://rpubs.com/Cristina_Gil/SVM
  
  library(e1071)

  Categorica        <- 0
  Escala            <- 0
  Ponderadores      <- 0
  SetDatosPredecir  <- NULL
  Constante         <- 1  
  
  #-------------------------->>>   
  # [1] PREPARACION DE DATOS Y PARAMETROS  
  #-------------------------->>>  
  
  FX <- R4XCL_INT_FUNCION(SetDatosX,SetDatosY)
  
  Procedimientos <- R4XCL_INT_PROCEDIMIENTOS()
  
  DT <- R4XCL_INT_DATOS(
                        SetDatosY=SetDatosY,                   
                        SetDatosX=SetDatosX,
                        Escala=Escala,
                        Filtro=Filtro,
                        Categorica=Categorica,
                        Ponderadores = Ponderadores
                      )
  
  P  <- ncol(DT)
  
  especificacion <- eval(parse(text=FX))
  
  #-------------------------->>> 
  # [2] PROCEDIMIENTO ANALITICO
  #-------------------------->>> 
  
  Modelo <- svm(
                formula = especificacion, 
                data = DT,
                kernel = "radial"
               )
  
  #-------------------------->>> 
  # [3] PREPARACION DE RESULTADOS
  #-------------------------->>> 
  
  if (TipoOutput == 0){
    
    print("entra")
    OutPut = Procedimientos$SVM
    
  }else if (TipoOutput == 1){  
    
    Y_Pred <- predict(Modelo, DT)
    OutPut=data.frame("Y estimado"=Y_Pred)#table(Y_Pred, SetDatosY)
    
  } 
  
  #-------------------------->>> 
  # [4] RESULTADO FINAL
  #-------------------------->>> 
  
  return(OutPut);  
}

DialogosXCL <- R4XCL_INT_DIALOGOS()

attr(MR_SVM, DialogosXCL$Descripcion) = 
  
  list(
    Detalle.Lineal  = DialogosXCL$Detalle,
    SetDatosY       = DialogosXCL$SetDatosY,
    SetDatosX       = DialogosXCL$SetDatosX,
    Filtro          = DialogosXCL$Filtro,
    TipoOutput      = "[0]: Lista de opciones, [1]: Kernel"
  )

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++