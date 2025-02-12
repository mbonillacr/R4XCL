#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# MR_Tobit                                             +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
MR_Tobit.D <- function(
                    SetDatosY, 
                    SetDatosX,
                    Categorica=0,
                    #Escala=0,
                    Filtro=0,
                    DirTruncamiento=1,
                    ValorTruncamiento=1,
                    SetDatosPredecir=NULL
                    )
  
{
  
  #-------------------------->>>
  # VALIDACIONES
  #-------------------------->>>
  if (missing(SetDatosX)) {
    stop("Error: SetDatosX es un parámetro obligatorio.")
  }
  if (missing(SetDatosY)) {
    stop("Error: SetDatosY es un parámetro obligatorio.")
  }
  if (!is.data.frame(SetDatosX)) {
    stop("Error: SetDatosX debe ser un data frame.")
  }
  if (!is.data.frame(SetDatosY)) {
    stop("Error: SetDatosY debe ser un data frame.")
  }
  if (!is.numeric(Categorica) || Categorica < 0 || Categorica > 1) {
    stop("Error: Categorica debe ser 0 o 1.")
  }
  if (!is.numeric(DirTruncamiento)) {
    stop("Error: DirTruncamiento debe ser un número.")
  }
  if (!is.numeric(ValorTruncamiento)) {
    stop("Error: ValorTruncamiento debe ser un número.")
  }
  if (!is.null(SetDatosPredecir) &&!is.data.frame(SetDatosPredecir)) {
    stop("Error: SetDatosPredecir debe ser un data frame.")
  }
  
#https://stats.idre.ucla.edu/r/dae/tobit-models/
  
#-------------------------->>>   
# [1] PREPARACION DE DATOS Y PARAMETROS  
#-------------------------->>>  

  library(ResourceSelection)
  library(margins)
  library(VGAM)
  
  # ARCHIVO  = "~/BERT2/functions/INTERNO/R4XCL-Interno.r"
  # FUENTE01 = file.path(ARCHIVO)

  FX <- R4XCL_INT_FUNCION(SetDatosX,SetDatosY)
  especificacion <- eval(parse(text=FX))
  
  Procedimientos <- R4XCL_INT_PROCEDIMIENTOS()
  
  DT <- R4XCL_INT_DATOS(
                       SetDatosY=SetDatosY,                   
                       SetDatosX=SetDatosX,
                       #Escala=Escala,
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
    
  }else if (DirTruncamiento < 0){
    
    Modelo <- VGAM ::vglm(
                          formula = especificacion,
                          tobit(Lower = ValorTruncamiento), 
                          data = DT
                          )
  }
  
  Y_Pred <- fitted(Modelo)[,1]
  
#-------------------------->>> 
# [3] PREPARACION DE RESULTADOS
#-------------------------->>> 
  
  A = dlg_list(Procedimientos$ARBOLES)
  TipoOutput=A$res
  
  if (TipoOutput == Procedimientos$TOBIT[1]){
    
    OutPut <- data.frame("R4XL_ModeloEstimado"= capture.output(summary(Modelo)))
    
  } else if (TipoOutput == Procedimientos$TOBIT[2]){
    
    OutPut <- data.frame("R4XL_PrediccionEnMuestra"= Y_Pred)
    
  } else if (TipoOutput == Procedimientos$TOBIT[3]){  
    
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
    
  } else if (TipoOutput == Procedimientos$TOBIT[4]){      
    
    OutPut <- rbind(FX,ValorTruncamiento)  
    
  } else if (TipoOutput == Procedimientos$TOBIT[5]){ 
    
    ListaM <- c(Modelo)
    ListaN <- c("BINARIO")
    OutPut <- R4XCL_INT_CREARDS(
                               ListaM,                        
                               ListaN
                               )
    
  } else if (TipoOutput == Procedimientos$TOBIT[6]){ 
    
    OutPut <- R4XCL_INT_INFO_EJECUCION(FX, DT)  
    
  }
  
#-------------------------->>> 
# [4] RESULTADO FINAL
#-------------------------->>> 
  
  return(OutPut)  
}

DialogosXCL <- R4XCL_INT_DIALOGOS()

attr(MR_Tobit.D, DialogosXCL$Descripcion) <- 
  list (
        Detalle           = DialogosXCL$Detalle.Tobit,
        SetDatosY         = DialogosXCL$SetDatosY,
        SetDatosX         = DialogosXCL$SetDatosX,
        Categorica        = DialogosXCL$Categorica,
        Filtro            = DialogosXCL$Filtro,
        DirTruncamiento   = DialogosXCL$DirTruncamiento,
        ValorTruncamiento = DialogosXCL$ValorTruncamiento,
        SetDatosPredecir  = DialogosXCL$SetDatosPredecir
        )

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++