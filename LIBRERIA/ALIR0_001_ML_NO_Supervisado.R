ACP<- function(SetDatosX, pScale) 
  
{
  #library(ggplot2)
#_________________________________________________________________  
# [1] PREPARACION DE DATOS 
#_________________________________________________________________
  
  p = ncol(SetDatosX)
  nombresX=paste0(SetDatosX[1,1:p])
  DatosX = SetDatosX[-1,]
  ObsX   = nrow(SetDatosX)-1
  ObsY   = ObsX
  
  DatosX = matrix( as.numeric(DatosX), nrow=ObsX, ncol=p)
  Datos  = data.frame(DatosX)
  
  if (pScale){Datos=scale(Datos)}
  
  colnames(Datos)[1:p]=nombresX[1:p]
  
#_________________________________________________________________ 
# [2] PROCEDIMIENTO ANALITICO
#_________________________________________________________________ 
  
  OutPut_0 <- prcomp(Datos) 
  
#_________________________________________________________________ 
# [3] PREPARACION DE RESULTADOS
#_________________________________________________________________ 
  
  OutPut_2 = data.frame("Resumen"= capture.output(summary(OutPut_0)))
 
  OutPut_A=append(OutPut_0, OutPut_2)
  
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
  
  par(mfrow=c(1,2))
  screeplot(OutPut_0, type="lines")  
  biplot (OutPut_0) 
  
  return(ListaOuput)
 
}

#_________________________________________________________________________________
#    +++++++++++++++++++++++++++++++++++++++++++++++++++++++    
#    +               FIN DE PROCEDIMIENTO                  +
#    +++++++++++++++++++++++++++++++++++++++++++++++++++++++  
#_________________________________________________________________________________

Kmedias<- function(SetDatosX, k, semilla, iteraciones) 
  
{
  
#_________________________________________________________________  
# [1] PREPARACION DE DATOS 
#_________________________________________________________________
  
  k=as.integer(k)
  set.seed(semilla)
  p = ncol(SetDatosX)
  kk=4*k+1

  nombresX=paste0(SetDatosX[1,1:p])
  DatosX=SetDatosX[-1,]
  ObsX=nrow(DatosX)
  DatosX = matrix(as.numeric(DatosX), nrow=ObsX, ncol=p)
  Datos  = data.frame(DatosX)

  colnames(Datos)[1:p]=nombresX[1:p]
  DatosEscalados = scale(Datos)

#_________________________________________________________________ 
# [2] PROCEDIMIENTO ANALITICO
#_________________________________________________________________ 

  OutPut_01 = kmeans(DatosEscalados, centers=k)#, iter.max=iteraciones) 

  Koptimo <- function(kk) {
                          cluster <- kmeans(DatosEscalados, kk)
                          return (cluster$tot.withinss) 
                          }
  
#_________________________________________________________________ 
# [3] PREPARACION DE RESULTADOS
#_________________________________________________________________ 
  
  A=c(seq(2,kk,1))
  OptimoK=sapply(A, Koptimo)
  
  OutPut_02=data.frame("tot.withinss K mayor a 2"=OptimoK)

  OutPut_A =append(OutPut_01,OutPut_02)
  
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

#_________________________________________________________________________________
#    +++++++++++++++++++++++++++++++++++++++++++++++++++++++    
#    +               FIN DE PROCEDIMIENTO                  +
#    +++++++++++++++++++++++++++++++++++++++++++++++++++++++  
#_________________________________________________________________________________