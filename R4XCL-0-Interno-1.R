#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# FUNCIONES INTERNAS
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

#------------------------------------>>>
# LISTAR DATASETS
#------------------------------------>>>

R4XCL_INT_LISTA_DATASETS = function(psPkg) 
{

  #-------------------------->>>
  # VALIDACIONES
  #-------------------------->>>
  if (!is.character(psPkg)) {
    stop("Error: psPkg debe ser un nombre de paquete válido.")
  }
  if (!requireNamespace(psPkg, quietly = TRUE)) {
    stop("Error: El paquete ", psPkg, " no está instalado.")
  }
  
  #-------------------------->>>
  # PREPARACION DE DATOS Y PARAMETROS
  #-------------------------->>>
  
  # Cargar el paquete sin mensajes de inicio
  suppressPackageStartupMessages(library(psPkg, character.only = TRUE))
  
  # Obtener la lista de objetos en el paquete
  v <- ls(paste0("package:", psPkg))
  
  # Crear un data frame vacío para almacenar los nombres de los datasets
  dfDS <- data.frame(dataset = character(0))
  
  # Obtener la cantidad de objetos en el paquete
  nV <- length(v)

  #-------------------------->>>
  # PROCEDIMIENTO ANALITICO
  #-------------------------->>>
  
  # Iterar sobre la lista de objetos en el paquete
  
  for(i in 1:nV ) 
    {
      x = eval(parse(text= paste0("class(",psPkg,"::",v[i],")") ) )
      
      if (x == 'data.frame')
        {
          dfDS = rbind(dfDS,data.frame(dataset = v[i]))
        }
    }
  
  #-------------------------->>>
  # RESULTADO FINAL
  #-------------------------->>>
  
  # Devolver la primera columna del data frame dfDS, que contiene los nombres de los datasets
  return(dfDS[, 1])

  }

#------------------------------------>>>
# CREAR CARPETA
#------------------------------------>>>

R4XCL_INT_CREA_CARPETA <- function()
{
  
  RutaGuardar     <- choose.dir(default = "", caption = "Seleccione el Destino")
  NombreCarpeta   <- "\\RESULTADOS R4XCL"
  Ruta_Local_0    <- paste0(RutaGuardar, NombreCarpeta)
  output_dir      <- file.path(Ruta_Local_0)
  
  #-------------------------->>>
  # CREAR CARPETA
  #-------------------------->>>
  
  # Si la carpeta no existe, crearla
  
  if (!dir.exists(output_dir)){dir.create(output_dir)}
  
  #-------------------------->>>
  # RESULTADO FINAL
  #-------------------------->>>
  
  # Devolver la ruta completa de la carpeta
  
  return(output_dir)
  
}

#------------------------------------>>>
# OBTENER FECHA
#------------------------------------>>>

R4XCL_INT_FECHA = function()
{
  
  #-------------------------->>>
  # PREPARACION DE DATOS Y PARAMETROS
  #-------------------------->>>
  
  # Obtener la fecha actual
  
  fecha=Sys.Date()
  a=format(as.Date(fecha, format="%d/%m/%Y"),"%Y")
  b=format(as.Date(fecha, format="%d/%m/%Y"),"%m")
  c=format(as.Date(fecha, format="%d/%m/%Y"),"%d")
  
  d=paste0(a,b,c)
  return(d)
}

#------------------------------------>>>
# PREGUNTAR SI/NO
#------------------------------------>>>

R4XCL_INT_PREGUNTA_SN = function(vctr.preguntas)
{
  #-------------------------->>>
  # VALIDACIONES
  #-------------------------->>>
  if (missing(vctr.preguntas)) {
    stop("Error: vctr.preguntas es un parámetro obligatorio.")
  }
  
  prm.mostrar=c("SI", "NO")
  
  # Crear un diálogo para preguntar al usuario si desea realizar la acción especificada en vctr.preguntas
  FX = dlg_list(
                prm.mostrar, 
                multiple = FALSE,
                preselect = prm.mostrar[1],
                title = paste0("Desea ",vctr.preguntas, " sus datos?")
               )
  
  #-------------------------->>>
  # RESULTADO FINAL
  #-------------------------->>>
  
  # Devolver TRUE si el usuario selecciona "SI", FALSE en caso contrario
  
  FX$res=="SI"
  
}

#------------------------------------>>>
# PREPARAR DATOS
#------------------------------------>>>

R4XCL_INT_DATOS = function(
                          SetDatosY  = NULL,                           
                          SetDatosX,
                          Escala     = NULL,
                          Filtro     = NULL,
                          Categorica = 0,
                          Ponderadores = NULL
                          )
{
  #-------------------------->>>
  # VALIDACIONES
  #-------------------------->>>
  if (missing(SetDatosX)) {
    stop("Error: SetDatosX es un parámetro obligatorio.")
  }
  if (!is.data.frame(SetDatosX)) {
    stop("Error: SetDatosX debe ser un data frame.")
  }
  if (!missing(SetDatosY) &&!is.data.frame(SetDatosY)) {
    stop("Error: SetDatosY debe ser un data frame.")
  }
  
  #-------------------------->>>
  # PREPARACION DE DATOS
  #-------------------------->>>
  
  if (Categorica==1){
        
    # Si Categorica es igual a 1, llamar a la función R4XCL_INT_DATOS_TEXTO
        Datos = R4XCL_INT_DATOS_TEXTO(
                                      SetDatosX  = SetDatosX,
                                      SetDatosY  = SetDatosY,
                                      Categorica = Categorica,
                                      Filtro     = Filtro,
                                      Ponderadores = Ponderadores
                                      )
  }else{
    
    # Si Categorica es diferente de 1, llamar a la función R4XCL_INT_DATOS_NUMERO
        Datos = R4XCL_INT_DATOS_NUMERO(
                                       SetDatosX = SetDatosX,
                                       SetDatosY = SetDatosY,
                                       Escala    = Escala,
                                       Filtro    = Filtro,
                                       Ponderadores = Ponderadores
                                       )
  }
  
  #-------------------------->>>
  # RESULTADO FINAL
  #-------------------------->>>
  
  # Devolver el data frame Datos
  
  return(Datos)
  
}

#------------------------------------>>>
# PREPARAR DATOS NUMÉRICOS
#------------------------------------>>>

R4XCL_INT_DATOS_NUMERO = function(
                                 SetDatosX,
                                 SetDatosY = NULL, 
                                 Escala    = NULL,
                                 Filtro    = NULL,
                                 Ponderadores = NULL
                                 )
{
  
  C <- NULL
  DatosX_N <- NULL
  
  pX <- ncol(SetDatosX)
  pY <- ncol(SetDatosY)
  nX <- nrow(SetDatosX) - 1
  nY <- nX
  pDimY <- dim(SetDatosY)

# Si el usuario requiere filtrar datos, proceder a conformar vector filtro  

  SetFiltrado = R4XCL_INT_FILTRAR(SetDatosX, 
                                  SetDatosY,
                                  Filtro, 
                                  Ponderadores,
                                  pDimY,
                                  pX,
                                  pY,
                                  nX)
  SetFiltrado = SetFiltrado[-1,]

# Contiene variable Dependiente (Y)? SI/NO   

  if (is.null(pDimY)) {

      nombresX  = paste0(SetDatosX[1,1:pX])
    
      if (pX==1){

          DatosX_N = as.numeric(unlist(SetFiltrado))

      }else{

            DatosX   = SetFiltrado[,1:pX]
            for (i in 1:pX){
                            C=as.numeric(unlist(DatosX[,i]))
                            DatosX_N=cbind(DatosX_N,C)
                            }
          }

      Datos    = data.frame(DatosX_N)
      colnames(Datos)[1:pX] = nombresX[1:pX]

} else {
  
      nombresX  = paste0(SetDatosX[1,1:pX])
      nombresY  = paste0(SetDatosY[1,1:pY])
       
      DatosY         = unlist(SetFiltrado[,1])
      Ponderadores   = unlist(SetFiltrado[,ncol(SetFiltrado)])
      
      if (pX==1){
                 
                DatosX_N   = as.numeric(unlist(SetFiltrado[,2]))

          }else{

                DatosX   = SetFiltrado[,2:(pX+1)]

                for (i in 1:pX){
                                C=as.numeric(unlist(DatosX[,i]))
                                DatosX_N=cbind(DatosX_N,C)
                                }
               }

      Datos = cbind(DatosY,DatosX_N,Ponderadores) 
      colnames(Datos)[1]        = nombresY
      colnames(Datos)[2:(pX+1)] = nombresX[1:pX]
      colnames(Datos)[(pX+2)]   = "PESOS"
      }
  
  #-------------------------->>>
  # ESCALAR DATOS
  #-------------------------->>>
  # Valida si el usuario solicitó escalar los datos  

  if (is.null(Escala)){Escala=0}  
      
  # Escalando datos    
  
  if (Escala==1)
  {
    Datos=scale(Datos, center = TRUE, scale  = TRUE)
  }

  #-------------------------->>>
  # RESULTADO FINAL
  #-------------------------->>>  
  # DataSet final  
  
  Datos = data.frame(Datos)
  return(Datos)

}

#------------------------------------>>>
# PREPARAR DATOS DE TEXTO
#------------------------------------>>>

R4XCL_INT_DATOS_TEXTO = function (
                                 SetDatosX,
                                 SetDatosY = NULL,
                                 Escala    = NULL,
                                 Filtro    = NULL,
                                 Categorica= 0,
                                 Ponderadores = NULL
                                 )
{
  #-------------------------->>>
  # VALIDACIONES
  #-------------------------->>>
  
  if (missing(SetDatosX)) {
    stop("Error: SetDatosX es un parámetro obligatorio.")
  }
  if (!is.data.frame(SetDatosX)) {
    stop("Error: SetDatosX debe ser un data frame.")
  }
  if (!missing(SetDatosY) &&!is.data.frame(SetDatosY)) {
    stop("Error: SetDatosY debe ser un data frame.")
  }
  
  #-------------------------->>>
  # PREPARACION DE DATOS Y PARAMETROS
  #-------------------------->>>
  
    require(svDialogs)
    P     = ncol(SetDatosX)
    pNObs = nrow(SetDatosX)
    
  #-------------------------->>>
  # FILTRAR DATOS
  #-------------------------->>>
    
  # Si el usuario requiere filtrar datos, proceder a conformar vector filtro  
    
    if (!is.null(Filtro))
       {Filtro = as.matrix(unlist(Filtro[-1,]))} 
    else 
       {Filtro=rep(0,pNObs)}

  # Si el usuario requiere corregir por heterocedasticidad      
    if (!is.null(Ponderadores))
      {Ponderadores = as.matrix(unlist(Ponderadores[-1,]))} 
    else 
      {Ponderadores=rep(1,pNObs)}    

  #-------------------------->>>
  # PROCESAR DATOS
  #-------------------------->>>
    
    NombreVariablesX  = unlist(SetDatosX[1,1:P])
    DTX.F1 = SetDatosX[-1,]
    
    if (!is.null(SetDatosY)){
                            NombreVariablesY  = unlist(SetDatosY[1,])
                            DTY.F1 = SetDatosY[-1,]
                            DTY.F  = unlist(DTY.F1)
                            }
    
    DTX.F  = NULL
    for (i in 1:P){
                  C     = unlist(DTX.F1[,i])
                  DTX.F = cbind(DTX.F,C) 
                  }
    
    colnames(DTX.F)[1:P] = NombreVariablesX[1:P]
    
    if(Categorica==1){
                     A=dlgList(c(NombreVariablesX,"Ninguna"), 
                               multiple=TRUE, 
                               title="Seleccione las variables NUMERICAS")
                     x.num = A$res
                     x.cat = setdiff(NombreVariablesX,x.num)
                     
                     DTX.F = data.frame(DTX.F)
                     if(A$res!="Ninguna"){                     
                                          DTX.F[, x.num] = sapply(DTX.F[ ,x.num], as.numeric)
                                         }

                     DTX.F[, x.cat] = sapply(DTX.F[ ,x.cat], as.factor)
                     } 
    
    if (!is.null(SetDatosY)){
            Datos              = data.frame(DTY.F,DTX.F)
            colnames(Datos)[1] = NombreVariablesY
    } else{           Datos    = DTX.F}
    
    #-------------------------->>>
    # FILTRAR DATOS
    #-------------------------->>>
    
    Datos = Datos[Filtro==0,]
    
    #-------------------------->>>
    # RESULTADO FINAL
    #-------------------------->>>
    
    return(Datos)
}

#------------------------------------>>>
# DEFINIR FUNCIÓN
#------------------------------------>>>

R4XCL_INT_FUNCION <- function (SetDatosX,SetDatosY = NULL)
{
  
  #-------------------------->>>
  # VALIDACIONES
  #-------------------------->>>
  if (missing(SetDatosX)) {
    stop("Error: SetDatosX es un parámetro obligatorio.")
  }
  if (!is.data.frame(SetDatosX)) {
    stop("Error: SetDatosX debe ser un data frame.")
  }
  if (!missing(SetDatosY) &&!is.data.frame(SetDatosY)) {
    stop("Error: SetDatosY debe ser un data frame.")
  }
  
  #-------------------------->>>
  # PREPARACION DE DATOS Y PARAMETROS
  #-------------------------->>>
  
  pX <- ncol(SetDatosX)
  pY <- ncol(SetDatosY)
  pp <- pY + pX
  
  nombresX <- paste0(SetDatosX[1,1:pX])
  nombresY <- paste0(SetDatosY[1,1])
  
  DatosY  <-SetDatosY[-1,]  
  DatosX  <-SetDatosX[-1,]
  
  nX    <- nrow(SetDatosX)-1
  nY    <- nX
  
  #-------------------------->>>
  # CONSTRUIR FORMULA
  #-------------------------->>>  
  
  DatosY <- matrix(DatosY, nrow=nY, ncol=pY)
  DatosX <- matrix(DatosX, nrow=nX, ncol=pX)
  
  colnames(DatosY)[1:pY] <- nombresY[1:pY]
  colnames(DatosX)[1:pX] <- nombresX[1:pX]

  especificacion_A <- ""
  especificacion_B <- ""
  
  if (pp<1){
    
    especificacion_F = ""
    
  }else if (pp==2){
    
    especificacion_F <- paste(nombresY[1],"~",nombresX[1],collapse = "")
    
  }else if (pp>2){
    
    especificacion_A <- paste(nombresY,"~",nombresX[1],collapse = "")
    especificacion_B <- paste(" +" ,nombresX[2:pX] ,collapse  = "")
    especificacion_F <- paste(
                              c(
                                especificacion_A,
                                especificacion_B
                                ),
                                collapse=""
                             )
  }  
  
  #-------------------------->>>
  # RESULTADO FINAL
  #-------------------------->>>
  
  return(especificacion_F)
}

#------------------------------------>>>
# FILTRAR DATOS
#------------------------------------>>>

R4XCL_INT_FILTRAR <- function (
                              SetDatosX,
                              SetDatosY = NULL,
                              Filtro = NULL,
                              Ponderadores = NULL,
                              pDimY,pX,pY,nX
                              )
{
  #-------------------------->>>
  # VALIDACIONES
  #-------------------------->>>
  if (missing(SetDatosX)) {
    stop("Error: SetDatosX es un parámetro obligatorio.")
  }
  if (!is.data.frame(SetDatosX)) {
    stop("Error: SetDatosX debe ser un data frame.")
  }
  if (!missing(SetDatosY) &&!is.data.frame(SetDatosY)) {
    stop("Error: SetDatosY debe ser un data frame.")
  }
  if (!is.null(Filtro) &&!is.vector(Filtro)) {
    stop("Error: Filtro debe ser un vector.")
  }
  if (!is.null(Ponderadores) &&!is.vector(Ponderadores)) {
    stop("Error: Ponderadores debe ser un vector.")
  }
  
  #-------------------------->>>
  # PREPARACION DE DATOS Y PARAMETROS
  #-------------------------->>>
  
  if (is.null(Ponderadores)){Ponderadores<-rep(1,nX)}
  if (is.null(Filtro)){Filtro<-rep(0,nX)}
 
  #-------------------------->>>
  # FILTRAR DATOS
  #-------------------------->>> 

      if (!is.null(pDimY))
      {
        
        pXY     <-    1 + pX
        YX      <-    cbind(SetDatosY,SetDatosX,Ponderadores)
        YX      <-    YX[Filtro==0,]
        
      }else{
        
        YX      <- cbind(SetDatosX,Ponderadores)
        YX      <- YX[Filtro==0,]
        
      }
  
  #-------------------------->>>
  # RESULTADO FINAL
  #-------------------------->>>
  
  SetFiltrado = data.frame(YX)
  return(SetFiltrado)
  
}

R4XCL_INT_INFO_EJECUCION <- function(FX,DT)
{
  
  #-------------------------->>>
  # VALIDACIONES
  #-------------------------->>>
  
  if (missing(FX)) {
    stop("Error: FX es un parámetro obligatorio.")
  }
  if (missing(DT)) {
    stop("Error: DT es un parámetro obligatorio.")
  }
  if (!is.data.frame(DT)) {
    stop("Error: DT debe ser un data frame.")
  }
  
  #-------------------------->>>
  # PREPARACION DE DATOS Y PARAMETROS
  #-------------------------->>>
  
  InfoEjecucion <- rbind(
                       paste0("Especificación: ",FX),
                       paste0("N = ",nrow(DT)),
                       paste0("Ejecutado por: ",Sys.getenv("USERNAME")),
                       paste0("Fecha Ejecución: ",Sys.time())
                       )
  
  #-------------------------->>>
  # RESULTADO FINAL
  #-------------------------->>>
  
  return(InfoEjecucion)
}

R4XCL_INT_CREARDS <- function(ListaM,ListaN)
{
  
  #-------------------------->>>
  # VALIDACIONES
  #-------------------------->>>
  if (missing(ListaM)) {
    stop("Error: ListaM es un parámetro obligatorio.")
  }
  if (missing(ListaN)) {
    stop("Error: ListaN es un parámetro obligatorio.")
  }
  if (!is.list(ListaM)) {
    stop("Error: ListaM debe ser una lista.")
  }
  if (!is.character(ListaN)) {
    stop("Error: ListaN debe ser un vector de caracteres.")
  }
  if (length(ListaM)!= length(ListaN)) {
    stop("Error: ListaM y ListaN deben tener la misma longitud.")
  }
  
  #-------------------------->>>
  # PREPARACION DE DATOS Y PARAMETROS
  #-------------------------->>>
  
  MODELOS <- ListaM
  NOMBRES <- ListaN 
  
  i<-1
  qModelos<- length(ListaM)
  
  fecha <- R4XCL_INT_FECHA()
  
  output_dir <- R4XCL_INT_CREA_CARPETA()
  
  #-------------------------->>>
  # GUARDAR MODELOS
  #-------------------------->>>
  
  while (i<=qModelos){
    saveRDS(
      MODELOS[i],  
      paste0(output_dir,"\\",NOMBRES[i],"_",fecha,".rds")
    )
    i<-i+1
    
  }
  
  #-------------------------->>>
  # RESULTADO FINAL
  #-------------------------->>>
  
  return(paste0("Archivos creados en: ", output_dir )) 
  
}

R4XCL_INT_CREAXCL <- function(pModelo)
{
  
  #-------------------------->>>
  # VALIDACIONES
  #-------------------------->>>
  
  if (missing(pModelo)) {
    stop("Error: pModelo es un parámetro obligatorio.")
  }
  if (!is.list(pModelo)) {
    stop("Error: pModelo debe ser una lista.")
  }
  if (length(pModelo) == 0) {
    stop("Error: pModelo no puede ser una lista vacía.")
  }
  
  #-------------------------->>>
  # PREPARACION DE DATOS Y PARAMETROS
  #-------------------------->>>
  
  library(writexl)
  
  qModelos <- length(pModelo)

  pRuta  <- R4XCL_INT_CREA_CARPETA()
  pFecha <- R4XCL_INT_FECHA()
  pFinal <- paste0(pRuta,"\\R4XCL_",pFecha,".xlsx")
  
  #-------------------------->>>
  # GUARDAR MODELOS
  #-------------------------->>>
  
  write_xlsx(pModelo, pFinal)
  
  #-------------------------->>>
  # RESULTADO FINAL
  #-------------------------->>>
  
  return(paste0("Archivo creado en: ", pFinal))

}