#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# MODELO BINARIO                                       +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

MR_Binario.C <- function(
    SetDatosY, 
    SetDatosX,
    Categorica=0,
    Filtro=0,
    Escala=0,
    SetDatosPredecir=NULL,
    TipoModelo=0,
    TipoOutput = 1
)
{
  
  #-------------------------->>>
  # VALIDACIONES
  #-------------------------->>>

  
  #-------------------------->>>
  # PREPARACION DE DATOS Y PARAMETROS
  #-------------------------->>> 
  
  
  FX <- R4XCL_INT_FUNCION(SetDatosX,SetDatosY)
  Procedimientos=R4XCL_INT_PROCEDIMIENTOS()
  
  DT <- R4XCL_INT_DATOS(
                        SetDatosX=SetDatosX,
                        SetDatosY=SetDatosY,
                        Escala=Escala,
                        Filtro=Filtro,
                        Categorica=Categorica
                      )
  P  <- ncol(DT)
  
  especificacion <- eval(parse(text=FX))
  
  #-------------------------->>> 
  # [2] PROCEDIMIENTO ANALITICO
  #-------------------------->>> 
  
  pModelo <- ifelse(TipoModelo==0,"logit","probit")
  
  Modelo <- glm(
                formula = especificacion, 
                data = DT, 
                family = binomial(link = pModelo)
               )
  
  Y_Pred <- Modelo$fitted.values
  
  #-------------------------->>> 
  # [3] PREPARACION DE RESULTADOS
  #-------------------------->>> 
  
  if (TipoOutput <= 0){
    
    OutPut <- Procedimientos$BINARIO 
    
  }else if(TipoOutput == 1){ 
    library(stargazer)
    OutPut <- data.frame("R4XCL_ModeloEstimado"= stargazer(Modelo, 
                                                          type="text", 
                                                          ci=TRUE, 
                                                          ci.level=0.95,
                                                          single.row=TRUE))
  }else if(TipoOutput == 2){
    
    OutPut <- data.frame("R4XL_PrediccionDentroDeMuestra" = Y_Pred)
    
  }else if(TipoOutput == 3){    
    
    if(missing(SetDatosPredecir)){
      
      OutPut <- data.frame("R4XL_PrediccionDentroDeMuestra" = Y_Pred)
      
    }else{
      
      p <- ncol(SetDatosPredecir)
      
      SetDatosPredecir <- R4XCL_INT_DATOS(
                                          SetDatosX <- SetDatosPredecir,
                                          Escala <- Escala,
                                          Filtro <- Filtro,
                                          Categorica<-Categorica
                                         )
      
      nombresX <- paste0(SetDatosX[1,1:p])
      
      colnames(SetDatosPredecir)[1:p] <- nombresX[1:p]
      print(class(SetDatosPredecir))
      
      A <- predict(
                    Modelo, 
                    newdata = SetDatosPredecir, 
                    type = "response"
                  )
      
      OutPut <- data.frame("R4XCL_PrediccionFueraDeMuestra"= A)
      
    }
    
  }else if(TipoOutput == 4){ 
    
    DatosY <- as.numeric(DT[,1])
    HL     <- ResourceSelection::hoslem.test(DatosY, Y_Pred, 10)
    HL     <- cbind(HL$expected,HL$observed,"HL"=HL$statistic,Prob=HL$p.value)
    OutPut <- data.frame("R4XL_TestDeHosmerLemeshow"=  data.frame(HL) )
    
  }else if(TipoOutput == 5){
    
    OutPut <- sapply(marginal_effects(Modelo, DT),mean)
    OutPut <- data.frame("R4XL_EfectosMarginales"= capture.output(OutPut))
    
  }else if(TipoOutput == 6){   
    
    A <- anova(Modelo, test = 'Chisq')
    OutPut <- data.frame("R4XL_ANOVA" = A)
    
  }else if(TipoOutput == 7){ 
    
    ListaM <- c(Modelo)
    ListaN <- c("BINARIO")
    OutPut <- R4XCL_INT_CREARDS(
                              ListaM,                        
                              ListaN
                              )
    
  }else if(TipoOutput == 8){ 
    
    A       <- R4XCL_INT_DESCRIPCION()
    OutPut  <- R4XCL_INT_INFO_EJECUCION(FX, DT)  
    
    
  }
  #-------------------------->>> 
  # [4] RESULTADO FINAL
  #-------------------------->>> 
  
  return(OutPut)  
  
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

DialogosXCL=R4XCL_INT_DIALOGOS()
attr(MR_Binario.C, DialogosXCL$Descripcion) = 
  list(
    Detalle         = DialogosXCL$Detalle.Binario,
    SetDatosY       = DialogosXCL$SetDatosY,
    SetDatosX       = DialogosXCL$SetDatosX,
    Categorica      = DialogosXCL$Categorica,
    Filtro          = DialogosXCL$Filtro,
    Escala          = DialogosXCL$Escala,
    SetDatosPredecir= DialogosXCL$SetDatosPredecir,
    TipoModelo      = DialogosXCL$TipoModelo.Binario,
    TipoOutput      = DialogosXCL$TipoOutput
  )