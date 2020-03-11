ML_Poisson <- function(SetDatosY,  SetDatosX)
  #https://stats.idre.ucla.edu/r/dae/poisson-regression/
{
  
  library(ResourceSelection)
  library(InformationValue)
  library(sandwich)
  
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
  
  especificacion= paste(c(especificacion_A,especificacion_B),collapse="");
  
  #-------------------------->>> 
  # [2] PROCEDIMIENTO ANALITICO
  #-------------------------->>> 
  
  OutPut_A <- glm(formula = especificacion, family = "poisson", data = Datos);
  
  cov.OutPut_A <- sandwich::vcovHC(OutPut_A, type="HC0")
  std.err <- sqrt(diag(cov.OutPut_A))
  
  OutPut_B = cbind(
                   "Estimate"= coef(OutPut_A), 
                   "Robust SE" = std.err,
                   "Pr(>|z|)" = 2 * pnorm(abs(coef(OutPut_A)/std.err),lower.tail=FALSE),
                   "LL" = coef(OutPut_A) - 1.96 * std.err,
                   "UL" = coef(OutPut_A) + 1.96 * std.err
                   )
  
  OutPut_C = with(
                  OutPut_A, 
                    cbind(
                          res.deviance = deviance, 
                          df = df.residual,
                          p = pchisq(deviance, df.residual, lower.tail=FALSE)
                         )
                  )
  
  #-------------------------->>> 
  # [3] PREPARACION DE RESULTADOS
  #-------------------------->>> 
  
  OutPut_Z = data.frame("Resultado"= capture.output(summary(OutPut_A)))
  OutPut_C = data.frame("Goodness of fit" = OutPut_C)
  
  OutPut_B = list(OutPut_B, OutPut_C, OutPut_Z)
  OutPut_A = append(OutPut_A, OutPut_B)
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
  
  return((ListaOuput))
  
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++ 