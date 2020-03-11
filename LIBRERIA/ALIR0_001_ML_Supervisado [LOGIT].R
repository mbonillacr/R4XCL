ML_Logit <- function(SetDatosY,  SetDatosX)
  
{
  
  library(ResourceSelection)
  library(InformationValue)
  
  #https://cran.r-project.org/web/packages/InformationValue/vignettes/InformationValue.html
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
  
  OutPut_A <- glm(formula = especificacion, data = Datos, family = "binomial");
  
  #-------------------------->>> 
  # [3] PREPARACION DE RESULTADOS
  #-------------------------->>> 
  
  OutPut_B = summary(OutPut_A)
  
  Y_Pred   = OutPut_A$fitted.values
  
  OutPut_C = InformationValue::optimalCutoff(Datos$Y,Y_Pred)

  OutPut_D = NULL #ResourceSelection::hoslem.test(Datos$Y, Y_Pred, 10)

  OutPut_E = with(OutPut_B, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
  OutPut_F = logLik(OutPut_A)
  OutPut_G = AIC(OutPut_A)
  OutPut_H = BIC(OutPut_A)
  OutPut_I = confint(OutPut_A)  
  OutPut_Z = data.frame("Resultado"= capture.output(summary(OutPut_A)))
  
  A_Tab  = data.frame(
    "Y"= OutPut_A$y,
    "Y.Estimado"= OutPut_A$fitted.values, 
    "Residuos"= OutPut_A$residuals
  )
  
  B_coef = data.frame(
    OutPut_B$coefficients,
    "CoefInt"=OutPut_I
  )
  
  B_Cov  = data.frame("Matriz Covarianza"=OutPut_B$cov.unscaled)

  
  #C_Opt  = data.frame(
    #"*****"="TEST",
    #"TestF.pValue"  =OutPut_E,
    #"HL.pValue"     =OutPut_D$p.value,                      
    # "LogL"          =OutPut_F,
    #"AIC"           =OutPut_G,
    #"BIC"           =OutPut_H,
    #"Corte.Optimo"  =OutPut_C
  #)
  
  #-------------------------->>> 
  # [4] RESULTADO FINAL
  #-------------------------->>> 
  
  message("FUNCIONA")
  
  #return(list(B_coef,C_Opt,B_Cov,A_Tab,OutPut_Z))
  return(list(B_coef,B_Cov,A_Tab,OutPut_Z))  
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++ 