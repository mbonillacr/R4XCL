XP_histograma <- function(SetDatosX,particiones)
{
  
  #_________________________________________________________________  
  # [1] PREPARACION DE DATOS 
  #_________________________________________________________________

  p = ncol(SetDatosX)
  
  message("PARTICIONES" = p) 
  
  nombresX=paste0(SetDatosX[1,1])
  DatosX=SetDatosX[-1,1]
  ObsX=nrow(DatosX)
  
  #DatosX = matrix(as.numeric(DatosX), nrow=ObsX, ncol=1)
  
  Datos  = as.numeric(DatosX)
  #colnames(Datos)[1]=nombresX[1]

  #_________________________________________________________________ 
  # [2] PROCEDIMIENTO ANALITICO
  #_________________________________________________________________ 
  
  OutPut_0  = hist(Datos, breaks = particiones)
  
  #_________________________________________________________________ 
  # [3] PREPARACION DE RESULTADOS
  #_________________________________________________________________       
  
  OutPut_A  = OutPut_0
  
  ListaOuput=list()
  qElementos=length(OutPut_A)
  
  for(i in 1:qElementos)
  {
    AA=toupper(names(OutPut_A[i]))
    AA= paste0("--> ", AA)
    nombre = (data.frame(c(paste0("Out: ",i),AA)))
    
    ListaOuput=append(ListaOuput, nombre)
    ListaOuput=append(ListaOuput, OutPut_A[i])
  }
  
  #_________________________________________________________________   
  # [4] RESULTADO FINAL
  #_________________________________________________________________  
  
  return(ListaOuput)
  
}

XP_percentiles <- function(SetDatosX,particiones)
{
  
  #_________________________________________________________________  
  # [1] PREPARACION DE DATOS 
  #_________________________________________________________________
  
  library(dplyr)
  p = ncol(SetDatosX)
  
  message("PARTICIONES" = p) 
  
  nombresX=paste0(SetDatosX[1,1])
  DatosX=SetDatosX[-1,1]
  ObsX=nrow(DatosX)
  
  #DatosX = matrix(as.numeric(DatosX), nrow=ObsX, ncol=1)
  
  Datos  = as.numeric(DatosX)

  #_________________________________________________________________ 
  # [2] PROCEDIMIENTO ANALITICO
  #_________________________________________________________________ 
  
  OutPut_0  = data.frame("Clasificacion" = dplyr::ntile(Datos, particiones))
  OutPut_1  = data.frame("Limites" = quantile(Datos, probs = seq(0, 1, length = 1+particiones)))

  #_________________________________________________________________ 
  # [3] PREPARACION DE RESULTADOS
  #_________________________________________________________________       
  
  OutPut_A  = append(OutPut_0,OutPut_1)
  
  ListaOuput=list()
  qElementos=length(OutPut_A)
  
  for(i in 1:qElementos)
  {
    AA=toupper(names(OutPut_A[i]))
    AA= paste0("--> ", AA)
    nombre = (data.frame(c(paste0("Out: ",i),AA)))
    
    ListaOuput=append(ListaOuput, nombre)
    ListaOuput=append(ListaOuput, OutPut_A[i])
  }
  
  #_________________________________________________________________   
  # [4] RESULTADO FINAL
  #_________________________________________________________________  
  
  return(ListaOuput)
  
}

XP_OutlierKernel <- function(SetDatosX,particiones)
{
  
  #_________________________________________________________________  
  # [1] PREPARACION DE DATOS 
  #_________________________________________________________________
  
  library(DMwR)
  
  p = ncol(SetDatosX)
  
  message("PARTICIONES" = p) 
  
  nombresX=paste0(SetDatosX[1,1])
  DatosX=SetDatosX[-1,1]
  ObsX=nrow(DatosX)
  
  #DatosX = matrix(as.numeric(DatosX), nrow=ObsX, ncol=1)
  
  Datos  = as.numeric(DatosX)
  
  #_________________________________________________________________ 
  # [2] PROCEDIMIENTO ANALITICO
  #_________________________________________________________________ 

  OutPut_0  = data.frame("Score Outlier" = DMwR::lofactor(Datos, particiones))

  #_________________________________________________________________ 
  # [3] PREPARACION DE RESULTADOS
  #_________________________________________________________________       
  
  OutPut_A  = OutPut_0
  
  ListaOuput=list()
  qElementos=length(OutPut_A)
  
  for(i in 1:qElementos)
  {
    AA= toupper(names(OutPut_A[i]))
    AA= paste0("--> ", AA)
    nombre = (data.frame(c(paste0("Out: ",i),AA)))
    
    ListaOuput=append(ListaOuput, nombre)
    ListaOuput=append(ListaOuput, OutPut_A[i])
  }
  
  #_________________________________________________________________   
  # [4] RESULTADO FINAL
  #_________________________________________________________________  
  
  return(ListaOuput)
  
}
