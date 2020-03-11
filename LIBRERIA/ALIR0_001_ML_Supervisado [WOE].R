ML_WOEbinSet<- function(SetDatosY,SetDatosX)
  
{

  library('Information')
  
#-------------------------->>>   
# [1] PREPARACION DE DATOS Y PARAMETROS  
#-------------------------->>> 
  
  p = ncol(SetDatosX)
  
  nombresX=paste0(SetDatosX[1,1:p])
  nombresY=paste0(SetDatosY[1,1])
  
  DatosY=SetDatosY[-1,]  
  DatosX=SetDatosX[-1,]
  
  ObsX=nrow(SetDatosY)-1
  ObsY=ObsX
  
  DatosX = matrix( as.numeric(DatosX), nrow=ObsX, ncol=p)
  DatosY = matrix( as.numeric(DatosY), nrow=ObsY, ncol=1)
  
  Datos  = data.frame(DatosY,DatosX)
  
  pp=p+1
  colnames(Datos)[1]=nombresY[1]
  colnames(Datos)[2:pp]=nombresX[1:p]
  
#-------------------------->>> 
# [2] PROCEDIMIENTO ANALITICO
#-------------------------->>> 
  
  Resultado_WOE <- Information::create_infotables(data=Datos, y=names(Datos)[1], parallel=FALSE)

#-------------------------->>> 
# [3] PREPARACION DE RESULTADOS
#-------------------------->>> 
  
  qTablas=length(Resultado_WOE$Tables)
  message("Cantidad de Tablas", qTablas)
    
  ListaOuput=list()  
  ListaOuput=append(ListaOuput, Resultado_WOE$Tables[1:qTablas])
  
#-------------------------->>> 
# [4] RESULTADO FINAL
#-------------------------->>> 
  
  return(ListaOuput)

}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++  

ML_WOEbinTree <- function(SetDatosY,SetDatosX)
{
  #https://multithreaded.stitchfix.com/blog/2015/08/13/weight-of-evidence/
  
  library('woeBinning')

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
  colnames(Datos)[1]=nombresY[1]
  colnames(Datos)[2:pp]=nombresX[1:p]
  
#-------------------------->>> 
# [2] PROCEDIMIENTO ANALITICO
#-------------------------->>> 
  
  Resultado_WOE = woeBinning::woe.tree.binning(Datos, names(Datos)[1] , names(Datos)[2:pp])

#-------------------------->>> 
# [3] PREPARACION DE RESULTADOS
#-------------------------->>> 
  
  OutPut_A = woeBinning::woe.binning.table(Resultado_WOE)

  ListaOuput=list()
  qElementos=length(OutPut_A)
  for(i in 1:qElementos)
    {
      nombre = data.frame( c("Variable:", names(OutPut_A[i])))
      ListaOuput=append(ListaOuput, nombre )
      ListaOuput=append(ListaOuput, OutPut_A[i])
    }
    
#-------------------------->>> 
# [4] RESULTADO FINAL
#-------------------------->>> 
    
  return(ListaOuput)
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++