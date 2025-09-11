MR_Poisson.C <- function(
    SetDatosY, 
    SetDatosX,
    Categorica=0,
    #Escala=0,
    Filtro=0,
    TipoOutput=0, 
    SetDatosPredecir=NULL,
    Constante=1
)
{
  
  #-------------------------->>>
  # VALIDACIONES
  #-------------------------->>>
  # if (missing(SetDatosX)) {
  #   stop("Error: SetDatosX es un parámetro obligatorio.")
  # }
  # if (missing(SetDatosY)) {
  #   stop("Error: SetDatosY es un parámetro obligatorio.")
  # }
  # if (!is.data.frame(SetDatosX)) {
  #   stop("Error: SetDatosX debe ser un data frame.")
  # }
  # if (!is.data.frame(SetDatosY)) {
  #   stop("Error: SetDatosY debe ser un data frame.")
  # }
  # if (!is.numeric(Categorica) || Categorica < 0 || Categorica > 1) {
  #   stop("Error: Categorica debe ser 0 o 1.")
  # }
  # if (!is.numeric(TipoOutput) || TipoOutput < 0 || TipoOutput > 7) {
  #   stop("Error: TipoOutput debe ser un número entre 0 y 7.")
  # }
  # if (!is.null(SetDatosPredecir) &&!is.data.frame(SetDatosPredecir)) {
  #   stop("Error: SetDatosPredecir debe ser un data frame.")
  # }
  # if (!is.numeric(Constante) || Constante < 0 || Constante > 1) {
  #   stop("Error: Constante debe ser 0 o 1.")
  # }
  
  # https://stats.idre.ucla.edu/r/dae/poisson-regression/
  
  #-------------------------->>>
  # PREPARACION DE DATOS Y PARAMETROS
  #-------------------------->>>
  
  library(ResourceSelection)
  library(sandwich)
  library(margins)
  
  FX <- R4XCL_INT_FUNCION(SetDatosX,SetDatosY)
  especificacion <- eval(parse(text=FX))
  
  Procedimientos <- R4XCL_INT_PROCEDIMIENTOS()
  
  Datos <- R4XCL_INT_DATOS(SetDatosX=SetDatosX,
                          SetDatosY=SetDatosY, 
                          #Escala=Escala,
                          Filtro=Filtro,
                          Categorica=Categorica)
  
  P  <- ncol(Datos)
  
  #-------------------------->>> 
  # [2] PROCEDIMIENTO ANALITICO
  #-------------------------->>> 
  
  Modelo     <- glm(formula = especificacion, family = "poisson", data = Datos);
  Y_Pred     <- predict(Modelo, type="response")
  cov.Modelo <- sandwich::vcovHC(Modelo, type="HC0")
  std.err    <- sqrt(diag(cov.Modelo))
  
  #-------------------------->>> 
  # [3] PREPARACION DE RESULTADOS
  #-------------------------->>> 
  
  if (TipoOutput <= 0){
    
    OutPut <- Procedimientos$POISSON
    
  } else if (TipoOutput == 1){  
    
    OutPut <- data.frame("R4XCL_ModeloEstimado"= capture.output(stargazer(Modelo, type="text", ci=TRUE, ci.level=0.95,single.row=TRUE)))
    
  } else if (TipoOutput == 2){
    
    OutPut <- data.frame("R4XCL_PrediccionEnMuestra"=Y_Pred)
    
  } else if (TipoOutput == 3){ 
    
    if(missing(SetDatosPredecir)){
      
      OutPut <- data.frame("R4XCL_PrediccionDentroDeMuestra"= Y_Pred)
      
    }else{
      
      p        <- ncol(SetDatosPredecir)
      nombresX <- paste0(SetDatosX[1,1:p])
      SetDatosPredecir <- SetDatosPredecir[-1,]
      
      SetDatosPredecir <- matrix(as.numeric(SetDatosPredecir), nrow=nrow(SetDatosPredecir), ncol=p)
      SetDatosPredecir <- data.frame(SetDatosPredecir)
      
      colnames(SetDatosPredecir)[1:p]=nombresX[1:p]
      
      A <- predict(Modelo, newdata = SetDatosPredecir, type = "response")
      OutPut <- data.frame("R4XCL_PrediccionFueraDeMuestra"= A)
      
    } 
    
  } else if (TipoOutput == 4){
    
    OutPut <- sapply(marginal_effects(Modelo, Datos),mean)
    OutPut <- data.frame("R4XCL_EfectosMarginales"= capture.output(OutPut))
    
  } else if (TipoOutput == 4){  
    
    OutPut <- car::vif(Modelo) 
    OutPut <- data.frame("R4XCL_InflacionDeVarianza"=OutPut)
    
    OutPut <- cbind(
      "Estimate"= coef(Modelo), 
      "Robust SE" = std.err,
      "Pr(>|z|)" = 2 * pnorm(abs(coef(Modelo)/std.err),lower.tail=FALSE),
      "LL" = coef(Modelo) - 1.96 * std.err,
      "UL" = coef(Modelo) + 1.96 * std.err
    )
    
  } else if (TipoOutput == 5){  
    
    OutPut <- with(Modelo, 
                  cbind(res.deviance = deviance, 
                        df = df.residual,
                        p = pchisq(deviance, df.residual, lower.tail=FALSE))
    )
    
  } else if (TipoOutput == 6){ 
    
    ListaM <- c(Modelo)
    ListaN <- c("BINARIO")
    
    OutPut <- R4XCL_INT_CREARDS(ListaM,ListaN)
    
  } else if (TipoOutput == 7){ 
    
    OutPut  <- R4XCL_INT_INFO_EJECUCION(FX, DT)  
    
  } else if (TipoOutput > 7){   
    
    OutPut <- "Revisar parámetros disponibles"  
    
  }  
  
  #-------------------------->>>
  # RESULTADO FINAL
  #-------------------------->>>
  
  return(OutPut)
  
}
DialogosXCL=R4XCL_INT_DIALOGOS()
attr(MR_Poisson.C, DialogosXCL$Descripcion) = 
  list (
    Detalle           = DialogosXCL$Detalle.Poisson,
    SetDatosY         = DialogosXCL$SetDatosY,
    SetDatosX         = DialogosXCL$SetDatosX,
    Categorica        = DialogosXCL$Categorica,
    Filtro            = DialogosXCL$Filtro,
    TipoOutput        = DialogosXCL$TipoOutput.Poisson,
    SetDatosPredecir  = DialogosXCL$SetDatosPredecir,
    Constante         = DialogosXCL$Constante
  )

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++ 