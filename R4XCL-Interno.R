R4XCL_INT_LISTA_DATASETS = function(psPkg) 
{

  eval(parse(text= paste0("suppressPackageStartupMessages(library(",psPkg,"))") ) )
  
  v = ls(paste0("package:",psPkg))
  dfDS = data.frame(dataset = character(0))
  
  nV=length(v)

  for(i in 1:nV ) 
    {
      x = eval(parse(text= paste0("class(",psPkg,"::",v[i],")") ) )
      
      if (x == 'data.frame')
        {
          dfDS = rbind(dfDS,data.frame(dataset = v[i]))
        }
    }
  
  dfDS [,1]

  }

R4XCL_INT_CREA_CARPETA = function()
{
  
  RutaGuardar     = choose.dir(default = "", caption = "Seleccione el Destino")
  NombreCarpeta   = "\\RESULTADOS R4XCL"
  Ruta_Local_0    = paste0(RutaGuardar, NombreCarpeta)
  output_dir      = file.path(Ruta_Local_0)
  
  if (!dir.exists(output_dir)){dir.create(output_dir)}
  
  return(output_dir)
  
  }

R4XCL_INT_FECHA = function()
{
  fecha=Sys.Date()
  a=format(as.Date(fecha, format="%d/%m/%Y"),"%Y")
  b=format(as.Date(fecha, format="%d/%m/%Y"),"%m")
  c=format(as.Date(fecha, format="%d/%m/%Y"),"%d")
  
  d=paste0(a,b,c)
  return(d)
}

R4XCL_INT_PREGUNTA_SN = function(vctr.preguntas)
{
  prm.mostrar=c("SI", "NO")
  
  FX = dlg_list(
                prm.mostrar, 
                multiple = FALSE,
                preselect = prm.mostrar[1],
                title = paste0("Desea ",vctr.preguntas, " sus datos?")
               )
  
  FX$res=="SI"
  
}

R4XCL_INT_DATOS = function(
                          SetDatosY  = NULL,                           
                          SetDatosX,
                          Escala     = NULL,
                          Filtro     = NULL,
                          Categorica = 0,
                          Ponderadores = NULL
                          )
{
  
  if (Categorica==1){
        
        Datos = R4XCL_INT_DATOS_TEXTO(
                                      SetDatosX  = SetDatosX,
                                      SetDatosY  = SetDatosY,
                                      Categorica = Categorica,
                                      Filtro     = Filtro,
                                      Ponderadores = Ponderadores
                                      )
  }else{
        Datos = R4XCL_INT_DATOS_NUMERO(
                                       SetDatosX = SetDatosX,
                                       SetDatosY = SetDatosY,
                                       Escala    = Escala,
                                       Filtro    = Filtro,
                                       Ponderadores = Ponderadores
                                       )
        }
}

R4XCL_INT_DATOS_NUMERO = function(
                                 SetDatosX,
                                 SetDatosY = NULL, 
                                 Escala    = NULL,
                                 Filtro    = NULL,
                                 Ponderadores = NULL
                                 )
{
  
  C=NULL
  DatosX_N=NULL
  
  pX = ncol(SetDatosX)
  pY = ncol(SetDatosY)
  nX = nrow(SetDatosX)-1
  nY = nX
  pDimY=dim(SetDatosY)

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

# Valida si el usuario solicitó estalar los datos  

  if (is.null(Escala)){Escala=0}  
      
# Escalando datos    
  
  if (Escala==1)
  {
    Datos=scale(Datos, center = TRUE, scale  = TRUE)
  }
  
# DataSet final  
  
  Datos = data.frame(Datos)
  return(Datos)

}

R4XCL_INT_DATOS_TEXTO = function (
                                 SetDatosX,
                                 SetDatosY = NULL,
                                 Escala    = NULL,
                                 Filtro    = NULL,
                                 Categorica= 0,
                                 Ponderadores = NULL
                                 )
{
    require(svDialogs)
    P     = ncol(SetDatosX)
    pNObs = nrow(SetDatosX)
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
    
    Datos = Datos[Filtro==0,]
    
    return(Datos)
}

R4XCL_INT_FUNCION = function (SetDatosX,SetDatosY = NULL)
{
  
  pX = ncol(SetDatosX)
  pY = ncol(SetDatosY)
  pp = pY + pX
  
  nombresX = paste0(SetDatosX[1,1:pX])
  nombresY = paste0(SetDatosY[1,1])
  
  DatosY  = SetDatosY[-1,]  
  DatosX  = SetDatosX[-1,]
  
  nX    = nrow(SetDatosX)-1
  nY    = nX
  
  DatosY = matrix(DatosY, nrow=nY, ncol=pY)
  DatosX = matrix(DatosX, nrow=nX, ncol=pX)
  
  colnames(DatosY)[1:pY] = nombresY[1:pY]
  colnames(DatosX)[1:pX] = nombresX[1:pX]

  especificacion_A = ""
  especificacion_B = ""
  
  if (pp<1){
    
    especificacion_F = ""
    
  }else if (pp==2){
    
    especificacion_F = paste(nombresY[1],"~",nombresX[1],collapse = "")
    
  }else if (pp>2){
    
    especificacion_A = paste(nombresY,"~",nombresX[1],collapse = "")
    especificacion_B = paste(" +" ,nombresX[2:pX] ,collapse  = "")
    especificacion_F = paste(
                              c(
                                especificacion_A,
                                especificacion_B
                                ),
                                collapse=""
                             )
  }  
  
  return(especificacion_F)
}

R4XCL_INT_FILTRAR = function (
                              SetDatosX,
                              SetDatosY = NULL,
                              Filtro = NULL,
                              Ponderadores = NULL,
                              pDimY,pX,pY,nX
                              )
{

  if (is.null(Ponderadores)){Ponderadores=rep(1,nX)}
  if (is.null(Filtro)){Filtro=rep(0,nX)}
  

      if (!is.null(pDimY))
      {
        
        pXY        = 1 + pX
        YX         = cbind(SetDatosY,SetDatosX,Ponderadores)
        YX         = YX[Filtro==0,]
        
      }else{
        
        YX         = cbind(SetDatosX,Ponderadores)
        YX         = YX[Filtro==0,]
        
      }
  
      SetFiltrado = data.frame(YX)
}

R4XCL_INT_INFO_EJECUCION = function(FX,DT)
{
  InfoEjecucion = rbind(
                       paste0("Especificación: ",FX),
                       paste0("N = ",nrow(DT)),
                       paste0("Ejecutado por: ",Sys.getenv("USERNAME")),
                       paste0("Fecha Ejecución: ",Sys.time())
                       )
  
  return(InfoEjecucion)
}

R4XCL_INT_CREARDS = function(ListaM,ListaN)
{
  MODELOS = ListaM
  NOMBRES = ListaN 
  
  i=1
  qModelos= length(ListaM)
  
  fecha=R4XCL_INT_FECHA()
  
  output_dir=R4XCL_INT_CREA_CARPETA()
  
  while (i<=qModelos){
    saveRDS(
      MODELOS[i],  
      paste0(output_dir,"\\",NOMBRES[i],"_",fecha,".rds")
    )
    i=i+1
    
  }
  
  return(paste0("Archivos creados en: ", output_dir )) 
  
}

R4XCL_INT_CREAXCL = function(pModelo)
{
  
  library(writexl)
  
  qModelos=length(pModelo)

  pRuta=R4XCL_INT_CREA_CARPETA()
  pFecha=R4XCL_INT_FECHA()
  pFinal=paste0(pRuta,"\\R4XCL_",pFecha,".xlsx")
  
  write_xlsx(pModelo, pFinal)

}

R4XCL_INT_PROCEDIMIENTOS = function()
{
     Reg_PanelData = 
      c(
        "[ 1] Estimacion Datos de Panel", 
        "[ 2] Efectos Fijos para INDIVIDUOS",
        "[ 3] Test de Efectos Fijos para INDIVIDUOS",
        "[ 4] Test de Haussman para efectos aleatorios",
        "[ 5] Efectos Fijos para TIEMPO",
        "[ 6] Test de Breush-Pagan para efectos aleatorios en [t]",
        "[ 7] Test de Breush-Pagan para efectos aleatorios en [i]",
        "[ 8] Test de Breush-Pagan para efectos aleatorios en [t,i]",
        "[ 9] Test de Breush-Pagan para efectos aleatorios",
        "[10] Test de Breush-Pagan para correlación contemporanea",
        "[11] Test de Pesaran para correlación contemporanea",
        "[12] Test de correlacion serial",
        "[13] Test de raiz unitaria", 
        "[14] Especificación empleada",
        "[15] Guardar Modelo Estimado en RDS"
       )
    
    Reg_Lineal = 
      c(
        "[1] Estimar Modelo de Regresión Lineal",
        "[2] Estimar valores para la [Y] (dentro de muestra)",
        "[3] Estimar valores para la [Y] (fuera de muestra)",
        "[4] Obtener efectos marginales",
        "[5] Calcular coeficiente de Inflación de Varianza VIF",
        "[6] Test de heterocedasticidad Breusch-Pagan", 
        "[7] Estimación robusta de coeficientes",
        "[8] Identificar observaciones de influencia en la muestra",
        "[9] Especificación empleada",
        "[10] Guardar Modelo Estimado en R",
        "[11] Modelo estimado con formato original de R",
        "[12] Obtener residuos estimados"
       )
    
    Reg_Binaria =  
      c(
        "[1] Estimar Modelo de Regresión Binaria",
        "[2] Estimar P[Y=1|XB] (dentro de muestra)",
        "[3] Estimar P[Y=1|XB] (fuera de muestra)",
        "[4] Test de Hosmer & Lemeshow",
        "[5] Obtener efectos marginales",
        "[6] ANOVA",
        "[7] Especificación empleada",
        "[8] Guardar Modelo Estimado en R"
       )
    
    Reg_Tobit =  
      c(
        "[1] Estimar Modelo de Regresión Tobit",
        "[2] Estimar [Y] (dentro de muestra)",
        "[3] Estimar [Y] (fuera de muestra)",
        "[4] Especificación empleada",
        "[5] Guardar Modelo Estimado en R"
       )
    
    Reg_Poisson = 
      c(
        "[1] Estimar Modelo de Regresión Poisson",
        "[2] Estimar [Y] (dentro de muestra)",
        "[3] Estimar [Y] (fuera de muestra)",
        "[4] Especificación empleada",
        "[5] Guardar Modelo Estimado en R"
       )
    
    Reg_Arboles = 
      c(
        "[1] Estimar Modelo de Arboles de Decisión",
        "[2] Estimar [Y] (dentro de muestra)",
        "[3] Estimar [Y] (fuera de muestra)",
        "[4] Especificación empleada",
        "[5] Guardar Modelo Estimado en R"
       )
    
    AD_KMedias =  
      c(
        "[1] Cluster Asignado",
        "[2] Centros",
        "[3] Variabilidad intra clases",
        "[4] Variabilidad Total",
        "[5] Parametros de Proceso",
        "[6] Test GAP",
        "[7] K-Optimo"
       )
    
    AD_ACP = 
      c(
        "[1] Matriz de Correlaciones",
        "[2] Matriz de Covarianza",
        "[3] Gráfico de Correlaciones",
        "[4] Componentes Principales",
        "[5] Coordenadas de Individuos",
        "[6] COS^2 de Individuos",
        "[7] Contribución de individuos",
        "[8] Aporte a la Varianza del CP(i)",
        "[9] COS^2 de variables",
        "[10] Contribución de variables",
        "[11] Predicción fuera de muestra",
        "[12] Gráfico Biplot"
       )
    
    Mat_Algebra =  
      c(
        "[1] Factorización de Cholesky",
        "[2] Valores propios",
        "[3] Vectores propios",
        "[4] Decomposición QR",
        "[5] Matriz Inversa",
        "[6] Singular Value Decomposition",
        "[7] Diagonal de la matriz",
        "[8] Matriz Transpuesta"
       )
    
    Computo_Vars=
      list(
           "[1] Crea variables Dummies",
           "[2] Centrar y/o estandarizar conjunto de Datos",
           "[3] Computar Distancias (Euclideana, Máximo, Canberra, Binaria)"
           )
    
    Graficacion=
      list(
        "[1] BoxPlot",
        "[2] EN PROCESO",
        "[3] EN PROCESO",
        "[4] EN PROCESO",
        "[5] EN PROCESO",
        "[6] EN PROCESO",
        "[7] EN PROCESO",
        "[8] EN PROCESO",
        "[9] EN PROCESO"
      )
    
Procedimientos = 
  list(
      "PANEL"   = Reg_PanelData, 
      "LINEAL"  = Reg_Lineal,
      "BINARIO" = Reg_Binaria,
      "TOBIT"   = Reg_Tobit,
      "POISSON" = Reg_Poisson,
      "ARBOLES" = Reg_Arboles,
      "KMEDIAS" = AD_KMedias,
      "ACP"     = AD_ACP,
      "ALGEBRA" = Mat_Algebra,
      "COMPUTOS"= Computo_Vars,
      "GRAFICA" = Graficacion
      )

return (Procedimientos)
}

R4XCL_INT_DIALOGOS <<- function()
{
    DialogosXCL = list( 
                      Descripcion        = "description",
                      
                      SetDatosX          = "Variables INDEPENDIENTES",
                      SetDatosX.NS       = "Datos por Analizar",
                      SetDatosX.MT       = "Seleccione la matriz cuadrada (incluir nombres de variables)",
                      SetDatosY          = "Variables DEPENDIENTES",
                      
                      Categorica         = "Contiene sus datos información NO NUMERICA? 1:SI, 0:NO",
                      Filtro             = "0:Incluir registro, 1:Excluir registro (0:Default)",
                      Constante          = "Incluir constante de estimación 1:SI, 0:NO (1:Default)",
                      Escala             = "Escalar datos? 1:SI, 0:NO (0:Default)",
                      Semilla            = "Valor de generación aleatoria (ej:123456)",
                      Ponderadores       = "Pesos a emplear en Minimos Cuadrados",
                      
                      K                  = "Cantidad de Clusters requeridos",
                      Koptimo            = "Cantidad maxima de Clusters por testear",
                      Centroides         = "Centros obtenidos del proceso de K-medias",
                      
                      FactorEscalamiento = "Criterio de Escalamiento Original",
                      
                      SetDatosPredecir   = "Computar datos fuera de muestra",
                      
                      Detalle.Algebra    = "Calculos matriciales para matrices pXp",
                      Detalle.Binario    = "Estima un modelo de Regresión para variable binaria: Y = {0,1}",        
                      Detalle.Poisson    = "Estima un modelo de Regresión Poisson",
                      Detalle.Lineal     = "Estima un modelo de Regresión lineal",
                      Detalle.Panel      = "Estima un modelo de Datos de Panel",
                      Detalle.Tobit      = "Estima un modelo de Regresion Tobit",
                      Detalle.Arbol      = "Estima un modelo de Arbol de Decisión",
                      Detalle.KM         = "Análisis de Conglomerados (K-Medias)",
                      Detalle.ACP        = "Análisis de Componentes Principales [ACP]",
                      
                      TipoModelo.KM      = "1:Hartigan-Wong, 2:Lloyd, 3:Forgy, 4:MacQueen",
                      TipoModelo.Binario = "0:Logit, 1:Probit",
                      TipoOutput.Binario = "1:Modelo, 2:Probabilidad Estimada, 3:Predicción, 4:Test HL, 5:Efectos Marginales",
                      TipoOutput.Lineal  = "1:Modelo, 2:Y Estimado, 3:Predicción, 4:Efectos Marginales, 5: Multicolinealidad, 6: Heterocedasticidad, 7:Estimación Robusta, 8: Outliers, 9:Especificacion, 10: Salvar Modelo",
                      TipoOutput.KM      = "1:Clasificación, 2:Centros, 3:Variabilidad INTRA clase, 4:Variabilidad Total, 5:Parametros de Proceso, 6:GAP, 7:K-Óptimo",
                      TipoOutput.ACP     = "1:Matriz de Correlación, 2:Coordenadas: Variables, 3:Coordenadas: Individuos, 4:COS^2: INDs, 5:Contribución:INDs, 6:Valores Propios, 7:COS^2: VARs, 8:Contribución:VARs, 9:Predicción, 10:Gráfico VARs|INDs",
                      TipoOutput.MT      = "1:Factorización Choleski, 2:Valores Propios, 3:Vectores Propios, 4:QR Decomposition, 5:Matriz Inversa, 6:Singular Value Decomposition, 7: Diagonal, 8:Transpuesta",
                      
                      Variable_i         = "Variable que identifica a los individuos",
                      Variable_t         = "Variable que identifica el tiempo",
                      
                      ValorTruncamiento  = "Valor umbral del truncamiento",   
                      
                      DirTruncamiento    = "Negativo: Truncamiento por la izquierda, Positivo: Truncamiento por la derecha"
                    )
   
   return(DialogosXCL)
}

R4XCL_INT_POR_INSTALAR = function()
{
    c(
      "broom",      "car",       	          "corrplot",  	 "curl",     "data.table",     	         "dplyr",    
      "fs",       	"ggplot2",   	          "here",     	 "lmtest",	 "magrittr",	               "maps",
      "margins",   	"mapdeck",   	          "mboost",   	 "mFilter",	 "PerformanceAnalytics",  	 "plm",
      "plotly",  	  "ResourceSelection",  	"readr",     	 "rlang",	   "rmarkdown",	               "rpart",     
      "rpart.plot",	"rpivotTable",	        "rworldmap",	 "sandwich", "stargazer", 	             "svDialogs",
      "tidyr",   	  "tseries",   	          "usdm",      	 "vctrs",    "VGAM",     	               "wooldridge",
      "zoo",        "dummies"
     )
  }