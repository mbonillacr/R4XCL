MM_Algebra.C <- function(
                          SetDatosX,
                          TipoOutput=0
                        )
  
{

  #-------------------------->>>
  # VALIDACIONES
  #-------------------------->>>
  if (missing(SetDatosX)) {
    stop("Error: SetDatosX es un parámetro obligatorio.")
  }
  if (!is.matrix(SetDatosX) || nrow(SetDatosX)!= ncol(SetDatosX)) {
    stop("Error: SetDatosX debe ser una matriz cuadrada.")
  }
  if (!is.numeric(TipoOutput) || TipoOutput < 0 || TipoOutput > 8) {
    stop("Error: TipoOutput debe ser un número entre 0 y 8.")
  }
  
  #-------------------------->>>
  # PREPARACION DE DATOS Y PARAMETROS
  #-------------------------->>> 

  DT <- R4XCL_INT_DATOS(
                       SetDatosX=SetDatosX
                       )
  DT <- as.matrix(DT)
  P  <- ncol(DT)
  N  <- nrow(DT)

  Procedimientos=R4XCL_INT_PROCEDIMIENTOS()
  
  #-------------------------->>> 
  # [2] PROCEDIMIENTO ANALITICO
  #-------------------------->>> 


  #-------------------------->>> 
  # [3] PREPARACION DE RESULTADOS
  #-------------------------->>> 
  if (TipoOutput <= 0){
    
    OutPut <- Procedimientos$ALGEBRA
    
  }else if(TipoOutput == 1){
    
    #Factorización Choleski
    OutPut <- chol(DT)
    
  }else if(TipoOutput == 2){
    
    # Valores Propios
    X <- as.numeric(eigen(DT)$values)
    OutPut <- data.frame(X)
    
  }else if(TipoOutput == 3){   
    
    #Vectores Propios
    X <- as.numeric(eigen(DT)$vectors)
    OutPut <- matrix(X, nrow = nrow(DT), ncol=ncol(DT))
    
  }else if(TipoOutput == 4){
    
    #QR Decomposition
    OutPut <- qr(DT)$qr
    
  }else if(TipoOutput == 5){
    
    # Inverse
    OutPut <- solve(DT)
    
  }else if(TipoOutput == 6){
    
    #Singular Value Decomposition
    
    OutPut <- svd(DT)$u
    
  }else if(TipoOutput == 7){
    
    #Diagonal
    
    OutPut <- diag(DT)  
  
  }else if(TipoOutput == 8){
    
    #Transpuesta
    
    OutPut <- t(DT)    
      
  }else if(TipoOutput > 8){     
    
    OutPut <- "Revisar parámetros disponibles"    
    
  }  
  
  #-------------------------->>>
  # RESULTADO FINAL
  #-------------------------->>>
  
  return(OutPut)
  
}

DialogosXCL=R4XCL_INT_DIALOGOS()

attr(MM_Algebra.C, DialogosXCL$Descripcion) = 
  list(
        Detalle     = DialogosXCL$Detalle.Algebra,
        SetDatosX   = DialogosXCL$SetDatosX.MT,
        TipoOutput  = DialogosXCL$TipoOutput.MT
      )

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
#               +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++