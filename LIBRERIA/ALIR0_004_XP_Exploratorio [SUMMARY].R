XP_Descriptivo <- function(SetDatosX)
{
  
  library('pastecs')
  #_________________________________________________________________  
  # [1] PREPARACION DE DATOS 
  #_________________________________________________________________

  
  p = ncol(SetDatosX)

  nombresX=paste0(SetDatosX[1,1:p])
  DatosX=SetDatosX[-1,1:p]
  ObsX=nrow(SetDatosX)-1
  
  DatosX = matrix(DatosX, nrow=ObsX, ncol=p)
  
  #Datos  = as.numeric(DatosX)
  colnames(DatosX)[1:p]=nombresX[1:p]
  Datos = DatosX

  #_________________________________________________________________ 
  # [2] PROCEDIMIENTO ANALITICO
  #_________________________________________________________________ 
  
  OutPut_0  = pastecs::stat.desc(Datos)

  OutPut_A  = list(OutPut_0)
  
  #_________________________________________________________________ 
  # [3] PREPARACION DE RESULTADOS
  #_________________________________________________________________       
  
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