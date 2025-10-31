MR_SVM <- function(
    SetDatosY, 
    SetDatosX,
    Filtro = NULL,
    pkernel = NULL,
    ptype = NULL,
    TipoOutput=0
)
{
  #-------------------------->>>
  # PREPARACION DE DATOS Y PARAMETROS
  #-------------------------->>>
  
  # https://rpubs.com/Cristina_Gil/SVM
  
  library(e1071)
  
  Categorica        <- 0
  Escala            <- 0
  Ponderadores      <- 0
  SetDatosPredecir  <- NULL
  Constante         <- 1  
  
  #-------------------------->>>   
  # [1] PREPARACION DE DATOS Y PARAMETROS  
  #-------------------------->>>  
  
  FX <- R4XCL_INT_FUNCION(SetDatosX,SetDatosY)
  
  Procedimientos <- R4XCL_INT_PROCEDIMIENTOS()
  
  DT <- R4XCL_INT_DATOS(
    SetDatosY=SetDatosY,                   
    SetDatosX=SetDatosX,
    Escala=Escala,
    Filtro=Filtro,
    Categorica=Categorica,
    Ponderadores = Ponderadores
  )
  
  P  <- ncol(DT)
  
  especificacion <- eval(parse(text=FX))
  
  #-------------------------->>> 
  # [2] PROCEDIMIENTO ANALITICO
  #-------------------------->>> 
  
  ckernel = c("linear", "polynomial", "radial basis", "sigmoid")
  ctype   = c("C-classification","nu-classification", "one-classification", "eps-regression", "nu-regression")
  
  
  Modelo <- svm(
               formula = especificacion, 
               data    = DT,
               kernel  = ckernel[pkernel], 
               type    = ctype[ptype]
               )
  
  #-------------------------->>> 
  # [3] PREPARACION DE RESULTADOS
  #-------------------------->>> 
  
  if (TipoOutput == 0){
    
    print("entra")
    OutPut = Procedimientos$SVM
    
  }else if (TipoOutput == 1){  
    
    Y_Pred <- predict(Modelo, DT)
    OutPut=data.frame("Y estimado"=Y_Pred)
    
  } 
  
  #-------------------------->>> 
  # [4] RESULTADO FINAL
  #-------------------------->>> 
  
  return(OutPut);  
}

DialogosXCL <- R4XCL_INT_DIALOGOS()

attr(MR_SVM, DialogosXCL$Descripcion) = 
  
  list(
    Detalle.Lineal  = DialogosXCL$Detalle,
    SetDatosY       = DialogosXCL$SetDatosY,
    SetDatosX       = DialogosXCL$SetDatosX,
    Filtro          = DialogosXCL$Filtro,
    pkernel         = "[0]: linear, [1]: polynomial, [2]: radial basis, [3]: sigmoid",
    ptype           = "[0]: C-classification,[1]: nu-classification, [2]:one-classification, [3]:eps-regression, [4]:nu-regression",
    TipoOutput      = "[0]: Lista de opciones, [1]: Kernel"
  )



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++