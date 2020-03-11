ML_OLS <- function(SetDatosY,  SetDatosX)
  
{
  
  #library(ResourceSelection)
  #library(InformationValue)
  
  #http://statistics.ats.ucla.edu/stat/r/dae/logit.htm
  
  #-------------------------->>>   
  # [1] PREPARACION DE DATOS Y PARAMETROS  
  #-------------------------->>>  
  
  p = ncol(SetDatosX)
  
  nombresX=paste0(SetDatosX[1,1:p])
  nombresY=paste0(SetDatosY[1,1])
  
  DatosY=SetDatosY[-1,]  
  DatosX=SetDatosX[-1,]
  
  ObsX=nrow(SetDatosX)-1
  ObsY=ObsX
  
  DatosX = matrix(as.numeric(DatosX), nrow=ObsX, ncol=p)
  DatosY = matrix(as.numeric(DatosY), nrow=ObsY, ncol=1)
  
  Datos  = data.frame(DatosY,DatosX)
  
  pp=p+1
  colnames(Datos)[1]=nombresY[1]
  colnames(Datos)[2:pp]=nombresX[1:p]
  
  especificacion_A = paste(nombresY,"~",nombresX[1],collapse = "") ;
  especificacion_B = paste("+" ,nombresX[2:p],collapse = "") ;
  if (p==1){especificacion_B=""}
  
  especificacion = paste(c(especificacion_A,especificacion_B),collapse="");
  message("FORMULA: ",especificacion)
    
  #-------------------------->>> 
  # [2] PROCEDIMIENTO ANALITICO
  #-------------------------->>> 

  OutPut_A <- lm(as.formula(especificacion), data = Datos);
  NOMBRE_DATAFRAME = toString(paste0("Especificacion: ", especificacion))
  
  #-------------------------->>> 
  # [3] PREPARACION DE RESULTADOS
  #-------------------------->>> 

  OutPut_Z = data.frame(capture.output(summary(OutPut_A)))
  colnames(OutPut_Z)=NOMBRE_DATAFRAME
  
  OutPut_A = append(OutPut_Z, OutPut_A)
  #-------------------------->>>
  
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

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++ 