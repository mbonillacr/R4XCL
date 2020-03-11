
ML_SVM <- function(SetDatosX, SetDatosY = NULL, scale = TRUE, kernel ="radial", 
                   degree=8,  cachesize = 40, tolerance = 0.001, CrearRDS)  
{
  
#___________________________________________________________________
# PROCESO: FUENTES EXTERNAS
#___________________________________________________________________

  
  library(e1071)

#___________________________________________________________________   
# PROCESO : PARAMETROS
#___________________________________________________________________
  
  HoraId            = format(Sys.time(), paste("%y%m%d%","-","%H%M%S"))
  NombreCarpeta     = "Support Vector Machine"  
  ExtensionOutput_00= ".rds"
  NombreArchivo     = "ML_SVM_MODELO"

  main_dir_orig   = "~/BERT2/RESULTADOS"
  NombreCarpeta   = "RandomForest"
  
  output_dir    = paste0(main_dir_orig, NombreCarpeta)
  
  {
    output_dir <- file.path(main_dir_orig)
    if (!dir.exists(output_dir)){dir.create(output_dir)}
  }
  
#_________________________________________________________________  
# PREVIO
#_________________________________________________________________      

 
  setwd(output_dir)
  WD_orig = getwd()
  
  NombreArchivoFinal_00 = paste(NombreArchivo,HoraId,ExtensionOutput_00)
  DireccionOutput = paste0(output_dir,"/",NombreArchivoFinal_00)
#_________________________________________________________________  
# [1] PREPARACION DE DATOS 
#_________________________________________________________________
  
  p = ncol(SetDatosX)
  
  nombresX=paste0(SetDatosX[1,1:p])
  nombresY=paste0(SetDatosY[1,1])
  
  DatosY=SetDatosY[-1,]  
  DatosX=SetDatosX[-1,]
  
  ObsX=nrow(DatosX)
  ObsY=ObsX
  
  DatosX = matrix(as.numeric(DatosX), nrow=ObsX, ncol=p)
  DatosY = matrix(as.numeric(DatosY), nrow=ObsY, ncol=1)
  
  Datos  = data.frame(DatosY,DatosX)
  
  pp=p+1
  colnames(Datos)[1]    = nombresY[1]
  colnames(Datos)[2:pp] = nombresX[1:p]
  
  especificacion_A = paste(nombresY,"~",nombresX[1],collapse = "") ;
  especificacion_B = paste("+" ,nombresX[2:p],collapse = "") ;
  
  especificacion  = paste(c(especificacion_A,especificacion_B),collapse="");
  especificacion  = as.formula(especificacion)
#_________________________________________________________________
# [2] PROCEDIMIENTO ANALITICO
#_________________________________________________________________


  Output_0 = e1071::svm(
                         formula=especificacion,
                         data=Datos,
                         scale=scale,
                         kernel=kernel,
                         degree=degree, 
                         cachesize=cachesize,
                         tolerance=tolerance
                       )
  
#_________________________________________________________________
# [3] PREPARACION DE RESULTADOS
#_________________________________________________________________
    
  #Output_1        = data.frame("Especificación" = c(capture.output(eval(especificacion)), DireccionOutput))  

  Output_2        = data.frame("Resumen"= capture.output(print(Output_0)))

  DireccionOutput = "ACHIVO NO CREADO"

  if (CrearRDS){
                DireccionOutput=paste0(output_dir,"/",NombreArchivoFinal_00)
                saveRDS(Output_0,NombreArchivoFinal_00)
               }
  setwd(WD_orig)  

  #-------------------------->>> 
  
  OutPut_A = append(Output_2 ,Output_0)
  #OutPut_A = append(OutPut_A,Output_2)  
  
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

#_________________________________________________________________
# PROCESO: ATRIBUTOS
#_________________________________________________________________

attr(ML_SVM, "description") <-list( 
  "Implementation in R by Chih-Chung Chang and Chih-Jen Lin",
  x="a data matrix, a vector, or a sparse matrix",
  y="a response vector with one label for each row/component of x. Can be either a factor (for classification tasks) or a numeric vector (for regression)",
  scale= "Logical vector indicating the variables to be scaled. If scale is of length 1, the value is recycled as many times as needed. (default: data are scaled internally both -x & y- to zero mean and unit variance)",
  type= "SVM can be used as a classification, regression or novelty detection. Default type is C-classification or Eps-regression. Options: [1] C-classification [2] Nu-classification [3] One-classification (novelty detection) [4] Eps-regression [5] Nu-regression",
  kernel= "Kernel used in training and predicting. You might consider changing some of the following parameters, depending on the kernel type [1]linear() [2] polynomial(degree, coef0, gamma) [3] radial basis(gamma), [4] sigmoid(coef0, gamma)",
  degree= "Needed for kernel of type polynomial (default: 3)", 
  gamma= "Needed for all kernels except linear (default: 1/(data dimension))", 
  coef0= "Needed for kernels of type polynomial and sigmoid (default: 0)",
  nu="parameter needed for nu-classification, nu-regression, and one-classification",
  class.weights="A named vector of weights for the different classes, used for asymmetric class sizes. Not all factor levels have to be supplied (default weight: 1). All components have to be named",
  cachesize = "Cache memory in MB (default: 40)",
  tolerance = "tolerance of termination criterion (default: 0.001)",
  epsilon =  "epsilon in the insensitive-loss function (default: 0.1)",
  shrinking = "option whether to use the shrinking-heuristics (default: TRUE)",
  cross ="if a integer value k>0 is specified, a k-fold cross validation on the training data is performed to assess the quality of the model: The accuracy rate for classification and the Mean Squared Error for regression",
  fitted = "logical indicating whether the fitted values should be computed and included in the model or not (default: TRUE)",
  probability="logical indicating whether the model should allow for probability predictions")

#________________________________________________________________________
# PROCESO: FIN
#________________________________________________________________________

#========================================================================
#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#========================================================================

ML_SVM_Pred <- function(MODELO_RF,SetDatosX)
{
  
#_________________________________________________________________  
# [1] PREPARACION DE DATOS 
#_________________________________________________________________
  
  p = ncol(SetDatosX)
  
  nombresX = paste0(SetDatosX[1,1:p])
  DatosX = SetDatosX[-1,]
  ObsX = nrow(DatosX)
  
  DatosX = matrix(as.numeric(DatosX), nrow=ObsX, ncol=p)
  
  Datos  = data.frame(DatosX)
  colnames(Datos)[1:p] = nombresX[1:p]
  
  MODELO_LEIDO  = readRDS(MODELO_RF) 
  MSG           = capture.output(MODELO_LEIDO$call)
  
#_________________________________________________________________ 
# [2] PROCEDIMIENTO ANALITICO
#_________________________________________________________________ 
  
  OutPut_0  = predict(MODELO_LEIDO, Datos)

#_________________________________________________________________ 
# [3] PREPARACION DE RESULTADOS
#_________________________________________________________________     
    
  OutPut_A  = data.frame("PREDICCION" = OutPut_0)
  
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

#========================================================================
#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#========================================================================