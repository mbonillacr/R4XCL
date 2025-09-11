MR_Tobit.C <- function(
    SetDatosY, 
    SetDatosX,
    Categorica=0,
    #Escala=0,
    Filtro=0,
    DirTruncamiento=1,
    ValorTruncamiento=1,
    TipoOutput=0,
    SetDatosPredecir=NULL
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
  # if (!is.numeric(DirTruncamiento)) {
  #   stop("Error: DirTruncamiento debe ser un número.")
  # }
  # if (!is.numeric(ValorTruncamiento)) {
  #   stop("Error: ValorTruncamiento debe ser un número.")
  # }
  # if (!is.numeric(TipoOutput) || TipoOutput < 0 || TipoOutput > 6) {
  #   stop("Error: TipoOutput debe ser un número entre 0 y 6.")
  # }
  # if (!is.null(SetDatosPredecir) &&!is.data.frame(SetDatosPredecir)) {
  #   stop("Error: SetDatosPredecir debe ser un data frame.")
  # }
  
  # https://stats.idre.ucla.edu/r/dae/tobit-models/
  
  #-------------------------->>>
  # PREPARACION DE DATOS Y PARAMETROS
  #-------------------------->>>
  
  # ARCHIVO  = "~/BERT2/functions/INTERNO/R4XCL-Interno.r"
  # FUENTE01 = file.path(ARCHIVO)
  
  library(ResourceSelection)
  library(margins)
  library(VGAM)
  
  FX <- R4XCL_INT_FUNCION(SetDatosX,SetDatosY)
  
  especificacion <- eval(parse(text=FX))
  Procedimientos <- R4XCL_INT_PROCEDIMIENTOS()
  
  DT <- R4XCL_INT_DATOS(
                        SetDatosY=SetDatosY,                   
                        SetDatosX=SetDatosX,
                        Filtro=Filtro,
                        Categorica=Categorica
                       )
  
  P  <- ncol(DT)
  
  #-------------------------->>> 
  # [2] PROCEDIMIENTO ANALITICO
  #-------------------------->>> 
  
  if(DirTruncamiento > 0){
    
    Modelo <- VGAM ::vglm(
                          formula = especificacion, 
                          tobit(Upper = ValorTruncamiento), 
                          data = DT
                         )
    
  } else if (DirTruncamiento < 0){
    
    Modelo <- VGAM::vglm(
                        formula = especificacion,
                        tobit(Lower = ValorTruncamiento), 
                        data = DT
                       )
  }
  
  Y_Pred <- fitted(Modelo)[,1]
  
  #-------------------------->>> 
  # [3] PREPARACION DE RESULTADOS
  #-------------------------->>> 
  
  if (TipoOutput <= 0){
    
    OutPut <- Procedimientos$TOBIT
    
  } else if (TipoOutput == 1){  
    
    OutPut <- data.frame("R4XL_ModeloEstimado"= capture.output(summary(Modelo)))
    
  } else if (TipoOutput == 2){
    
    OutPut <- data.frame("R4XL_PrediccionEnMuestra"= Y_Pred)
    
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
    
    OutPut <- rbind(FX,ValorTruncamiento)  
    
  } else if (TipoOutput == 5){ 
    
    ListaM <- c(Modelo)
    ListaN <- c("BINARIO")
    OutPut <- R4XCL_INT_CREARDS(ListaM,ListaN)
    
  } else if (TipoOutput == 6){ 
    
    OutPut  = R4XCL_INT_INFO_EJECUCION(FX, DT)  
    
  } else if (TipoOutput > 6){   
    
    OutPut <- "Revisar par?metros disponibles"  
    
  }  
  
  #-------------------------->>> 
  # [4] RESULTADO FINAL
  #-------------------------->>> 
  
  return(OutPut)  
}

DialogosXCL <- R4XCL_INT_DIALOGOS()

attr(MR_Tobit.C, DialogosXCL$Descripcion) <- 
  list (
    Detalle           = DialogosXCL$Detalle.Tobit,
    SetDatosY         = DialogosXCL$SetDatosY,
    SetDatosX         = DialogosXCL$SetDatosX,
    Categorica        = DialogosXCL$Categorica,
    #Escala            = DialogosXCL$Escala,
    Filtro            = DialogosXCL$Filtro,
    DirTruncamiento   = DialogosXCL$DirTruncamiento,
    ValorTruncamiento = DialogosXCL$ValorTruncamiento,
    TipoOutput        = DialogosXCL$TipoOutput.Tobit,
    SetDatosPredecir  = DialogosXCL$SetDatosPredecir
  )

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++