ML_RandomForest<- function(SetDatosX, SetDatosY=NULL, CrearRDS=TRUE, 
                           xtest=NULL, ytest=NULL, ntree=500,
                           mtry=if (!is.null(y) && !is.factor(y))
                           max(floor(ncol(x)/3), 1) else floor(sqrt(ncol(x))),
                           replace=TRUE, classwt=NULL, cutoff, strata,
                           sampsize = if (replace) nrow(x) else ceiling(.632*nrow(x)),
                           nodesize = if (!is.null(y) && !is.factor(y)) 5 else 1,
                           maxnodes = NULL,
                           importance=FALSE, localImp=FALSE, nPerm=1,
                           proximity, oob.prox=proximity,
                           norm.votes=TRUE, do.trace=FALSE,
                           keep.forest=!is.null(y) && is.null(xtest), corr.bias=FALSE,
                           keep.inbag=FALSE)

{
  

  library('randomForest')
  
#_________________________________________________________________   
# PARAMETROS
#_________________________________________________________________
  
  HoraId            = format(Sys.time(), paste("%y%m%d%","-","%H%M%S"))
  NombreCarpeta     = "RandomForest"  
  ExtensionInput    = ".XXXXXX"
  ExtensionOutput_00= ".rds"
  NombreArchivo     = "ML_RandomForest_MODELO"

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

#_________________________________________________________________  
# [1] PREPARACION DE DATOS 
#_________________________________________________________________

  p = ncol(SetDatosX)

  nombresX = paste0(SetDatosX[1,1:p])
  nombresY = paste0(SetDatosY[1,1])
  
  DatosY = SetDatosY[-1,]  
  DatosX = SetDatosX[-1,]
  
  ObsX = nrow(DatosX)
  ObsY = ObsX

  DatosX = matrix(as.numeric(DatosX), nrow=ObsX, ncol=p)
  DatosY = matrix(as.numeric(DatosY), nrow=ObsY, ncol=1)
    
  Datos  = data.frame(DatosY,DatosX)
  
  pp = p+1
  colnames(Datos)[1]   =nombresY[1]
  colnames(Datos)[2:pp]=nombresX[1:p]
  
  especificacion_A = paste(nombresY,"~",nombresX[1],collapse = "") ;
  especificacion_B = paste("+" ,nombresX[2:p],collapse = "") ;
  
  especificacion  = paste(c(especificacion_A,especificacion_B),collapse="");
  especificacion  = as.formula(especificacion)
  
  DireccionOutput = paste0(output_dir,"/",NombreArchivoFinal_00)
#_________________________________________________________________ 
# [2] PROCEDIMIENTO ANALITICO
#_________________________________________________________________ 

  message("RESULTADO especificacion: ", especificacion)  
  message("RESULTADO Datos: ", names(Datos)) 
  
  Output_0=randomForest::randomForest(formula=especificacion, data=Datos, importance=TRUE)
  
#_________________________________________________________________
# [3] PREPARACION DE RESULTADOS
#_________________________________________________________________
  
  Output_1=data.frame("Especificacion" = c(capture.output(eval(especificacion)), DireccionOutput))  
  Output_2=data.frame("Resumen"= capture.output(print(Output_0)))

  #-------------------------->>> 

  setwd(output_dir)
    if (CrearRDS){saveRDS(Output_0,NombreArchivoFinal_00)}
  setwd(WD_orig)  

  #-------------------------->>> 
  OutPut_A=append(Output_1,Output_0)
  OutPut_A=append(OutPut_A,Output_2) 
  
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
# [5] ATRIBUTOS
#_________________________________________________________________

attr( ML_RandomForest, "description" ) <-list( 
  "Breiman and Cutler's Random Forests",
  x="A data frame or a matrix of predictors, or a formula describing the model to be fitted (for the print method, an randomForest object)", 
  y="A response vector. If a factor, classification is assumed, otherwise regression is assumed. If omitted, randomForest will run in unsupervised mode.",
  xtest= "a data frame or matrix (like x) containing predictors for the test set",
  ytest= "response for the test set",
  mtry= "Number of variables randomly sampled as candidates at each split. Note that the default values are different for classification (sqrt(p) where p is number of variables in x) and regression (p/3)",
  replace="Should sampling of cases be done with or without replacement?",
  classwt="Priors of the classes. Need not add up to one. Ignored for regression.",
  cutoff="(Classification only) A vector of length equal to number of classes. The [winning] class for an observation is the one with the maximum ratio of proportion of votes to cutoff. Default is 1/k where k is the number of classes (i.e., majority vote wins)",
  strata="A (factor) variable that is used for stratified sampling",
  sampsize="Size(s) of sample to draw. For classification, if sampsize is a vector of the length the number of strata, then sampling is stratified by strata, and the elements of sampsize indicate the numbers to be drawn from the strata",
  nodesize="Minimum size of terminal nodes. Setting this number larger causes smaller trees to be grown (and thus take less time). Note that the default values are different for classification (1) and regression (5)",
  maxnodes="Maximum number of terminal nodes trees in the forest can have. If not given, trees are grown to the maximum possible (subject to limits by nodesize). If set larger than maximum possible, a warning is issued.",
  importance="Should importance of predictors be assessed?",
  localImp="Should casewise importance measure be computed? (Setting this to TRUE will override importance)",
  nPerm="Number of times the OOB data are permuted per tree for assessing variable importance. Number larger than 1 gives slightly more stable estimate, but not very effective. Currently only implemented for regression",
  proximity="Should proximity measure among the rows be calculated?",
  oob.prox="Should proximity be calculated only on [out-of-bag] data?",
  norm.votes="If TRUE (default), the final result of votes are expressed as fractions. If FALSE, raw vote counts are returned (useful for combining results from different runs). Ignored for regression",
  do.trace="If set to TRUE, give a more verbose output as randomForest is run. If set to some integer, then running output is printed for every do.trace trees",
  keep.forest="If set to FALSE, the forest will not be retained in the output object. If xtest is given, defaults to FALSE",
  corr.bias="perform bias correction for regression? Note: Experimental. Use at your own risk",
  keep.inbag ="Should an n by ntree matrix be returned that keeps track of which samples are [in-bag] in which trees (but not how many times, if sampling with replacement)"
)

#=============================================================================================================================
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#=============================================================================================================================

ML_RandomForest_Pred <- function(MODELO_RF,SetDatosX)
    {
  
#_________________________________________________________________  
# [1] PREPARACION DE DATOS 
#_________________________________________________________________
  
      p = ncol(SetDatosX)

      nombresX=paste0(SetDatosX[1,1:p])
      DatosX=SetDatosX[-1,]
      ObsX=nrow(DatosX)
            
      DatosX = matrix(as.numeric(DatosX), nrow=ObsX, ncol=p)
      
      Datos  = data.frame(DatosX)
      colnames(Datos)[1:p]=nombresX[1:p]

      MODELO_LEIDO  = readRDS(MODELO_RF) 
      MSG           = capture.output(MODELO_LEIDO$call)

      message("Datos: ", Datos)
      
#_________________________________________________________________ 
# [2] PROCEDIMIENTO ANALITICO
#_________________________________________________________________ 
      
      OutPut_0  = predict(MODELO_LEIDO, Datos, OOB=TRUE, type = "response")

#_________________________________________________________________ 
# [3] PREPARACION DE RESULTADOS
#_________________________________________________________________       
      
      OutPut_A  = data.frame("PREDICCION" = OutPut_0)
      
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


#=============================================================================================================================
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#=============================================================================================================================