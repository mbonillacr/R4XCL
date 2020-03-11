ML_ArbolDecision <- function(SetDatosY,SetDatosX,Tipo='class')
  
{
  
  library(rpart)
  
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
  
  DatosX = matrix( as.numeric(DatosX), nrow=ObsX, ncol=p)
  DatosY = matrix( as.numeric(DatosY), nrow=ObsY, ncol=1)
  
  Datos  = data.frame(DatosY,DatosX)
  
  pp=p+1
  
  colnames(Datos)[1]    = nombresY[1]
  colnames(Datos)[2:pp] = nombresX[1:p]
  
  especificacion_A = paste(nombresY,"~",nombresX[1],collapse = "") ;
  especificacion_B = paste("+" ,nombresX[2:p],collapse = "") ;
  
  especificacion   = paste(c(especificacion_A,especificacion_B),collapse="");
  
  #-------------------------->>> 
  # [2] PROCEDIMIENTO ANALITICO
  #-------------------------->>>

  OutPut_A <- rpart::rpart(formula = especificacion, data=Datos, method = Tipo);
  
  #-------------------------->>> 
  # [3] PREPARACION DE RESULTADOS
  #-------------------------->>>  
  
  ListaOuput=list()
  qElementos=length(OutPut_A)
  
  for(i in 1:qElementos)
  {
    AA =toupper(names(OutPut_A[i]))
    AA = paste0("--> ", AA)
    nombre = (data.frame(c(paste0("Out: ",i),AA)))
    
    #message("NOMBRES: ", AA)
    ListaOuput = append(ListaOuput, nombre)
    ListaOuput = append(ListaOuput, OutPut_A[i])
  }
  
  #-------------------------->>> 
  # [4] RESULTADO FINAL
  #-------------------------->>>   
  rpart.plot(OutPut_A)  
  return(ListaOuput)

}

#_________________________________________________________________________________
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++  
#_________________________________________________________________________________