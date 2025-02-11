#------------------------------------>>> 
#  Crea conjunto de variables DUMMIES
#------------------------------------>>> 

UT_Computo_Vars <- function(SetDatosX)
{
  
  #-------------------------->>>   
  # VALIDACIONES
  #-------------------------->>>  
  if (!is.data.frame(SetDatosX)) {
    stop("Error: SetDatosX debe ser un data frame.")
  }
  
  
  library(dummies)
  library(svDialogs)
  
  #-------------------------->>>   
  # [1] PREPARACION DE DATOS Y PARAMETROS  
  #-------------------------->>>  
  
  p <- ncol(SetDatosX)
  n <- nrow(SetDatosX)-1
  
  nombresX       <- paste0(SetDatosX[1,1:p])
  DatosX         <- SetDatosX[-1,]
  Procedimientos <- R4XCL_INT_PROCEDIMIENTOS()
  
  #-------------------------->>> 
  # [2] PROCEDIMIENTO ANALITICO
  #-------------------------->>> 

  vctr.metodo <- Procedimientos$COMPUTOS
  A <- dlg_list(vctr.metodo)
  TipoOutput <- A$res
  
  if (TipoOutput <= Procedimientos$COMPUTOS[1]){
    
    OutPut <- dummies::dummy(DatosX)

  }else if(TipoOutput == Procedimientos$COMPUTOS[2]){  
    
    vctr.preguntas <- c("Centrar", "Estandarizar")
    Res.FX <- sapply(vctr.preguntas, R4XCL_INT_PREGUNTA_SN)
    
    OutPut <- scale(matrix(as.numeric(DatosX),n,p), 
                   center = Res.FX[1],
                   scale  = Res.FX[2])
    OutPut <- data.frame("R4XL_DatosNormalizados"= OutPut)


  }else if(TipoOutput == Procedimientos$COMPUTOS[3]){
    
    vctr.metodo <- c("euclidean", "maximum","canberra","binary")
    A <- dlg_list(vctr.metodo)
    TipoOutput <- A$res
    
    OutPut <- dist(DatosX, method = TipoOutput, diag = TRUE, upper = TRUE, p = 2)
    OutPut <- as.matrix(OutPut)
  
  } 
  #-------------------------->>> 
  # [4] RESULTADO FINAL
  #-------------------------->>> 
  
  return(OutPut)  
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
Detalle = "Realiza diversos cálculos de transformación de variables"
attr(UT_Computo_Vars, "description" ) = 
                                  list( 
                                       Detalle,
                                       SetDatosX = "Datos a procesar"
                                      )