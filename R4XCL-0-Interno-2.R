R4XCL_INT_PROCEDIMIENTOS <- function()
{
  Reg_PanelData <- 
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
      "[10] Test de Breush-Pagan para correlaci?n contemporanea",
      "[11] Test de Pesaran para correlaci?n contemporanea",
      "[12] Test de correlacion serial",
      "[13] Test de raiz unitaria", 
      "[14] Especificaci?n empleada",
      "[15] Guardar Modelo Estimado en RDS"
    )
  
  Reg_Lineal <- 
    c(
      "[1] Estimar Modelo de Regresi?n Lineal",
      "[2] Estimar valores para la [Y] (dentro de muestra)",
      "[3] Estimar valores para la [Y] (fuera de muestra)",
      "[4] Obtener efectos marginales",
      "[5] Calcular coeficiente de Inflaci?n de Varianza VIF",
      "[6] Test de heterocedasticidad Breusch-Pagan", 
      "[7] Estimaci?n robusta de coeficientes",
      "[8] Identificar observaciones de influencia en la muestra",
      "[9] Especificaci?n empleada",
      "[10] Guardar Modelo Estimado en R",
      "[11] Modelo estimado con formato original de R",
      "[12] Obtener residuos estimados"
    )
  
  Reg_Binaria <-  
    c(
      "[1] Estimar Modelo de Regresi?n Binaria",
      "[2] Estimar P[Y=1|XB] (dentro de muestra)",
      "[3] Estimar P[Y=1|XB] (fuera de muestra)",
      "[4] Test de Hosmer & Lemeshow",
      "[5] Obtener efectos marginales",
      "[6] ANOVA",
      "[7] Especificaci?n empleada",
      "[8] Guardar Modelo Estimado en R"
    )
  
  Reg_Tobit <-  
    c(
      "[1] Estimar Modelo de Regresi?n Tobit",
      "[2] Estimar [Y] (dentro de muestra)",
      "[3] Estimar [Y] (fuera de muestra)",
      "[4] Especificaci?n empleada",
      "[5] Guardar Modelo Estimado en R"
    )
  
  Reg_Poisson <- 
    c(
      "[1] Estimar Modelo de Regresi?n Poisson",
      "[2] Estimar [Y] (dentro de muestra)",
      "[3] Estimar [Y] (fuera de muestra)",
      "[4] Especificaci?n empleada",
      "[5] Guardar Modelo Estimado en R"
    )
  
  Reg_Arboles <- 
    c(
      "[1] Estimar Modelo de Arboles de Decisi?n",
      "[2] Estimar [Y] (dentro de muestra)",
      "[3] Estimar [Y] (fuera de muestra)",
      "[4] Especificaci?n empleada",
      "[5] Guardar Modelo Estimado en R"
    )
  
  AD_KMedias <-  
    c(
      "[1] Cluster Asignado",
      "[2] Centros",
      "[3] Variabilidad intra clases",
      "[4] Variabilidad Total",
      "[5] Parametros de Proceso",
      "[6] Test GAP",
      "[7] K-Optimo"
    )
  
  AD_ACP <- 
    c(
      "[1] Matriz de Correlaciones",
      "[2] Matriz de Covarianza",
      "[3] Gr?fico de Correlaciones",
      "[4] Componentes Principales",
      "[5] Coordenadas de Individuos",
      "[6] COS^2 de Individuos",
      "[7] Contribuci?n de individuos",
      "[8] Aporte a la Varianza del CP(i)",
      "[9] COS^2 de variables",
      "[10] Contribuci?n de variables",
      "[11] Predicci?n fuera de muestra",
      "[12] Gr?fico Biplot"
    )
  
  Mat_Algebra <-  
    c(
      "[1] Factorizaci?n de Cholesky",
      "[2] Valores propios",
      "[3] Vectores propios",
      "[4] Decomposici?n QR",
      "[5] Matriz Inversa",
      "[6] Singular Value Decomposition",
      "[7] Diagonal de la matriz",
      "[8] Matriz Transpuesta"
    )
  
  Computo_Vars <-
    list(
      "[1] Crea variables Dummies",
      "[2] Centrar y/o estandarizar conjunto de Datos",
      "[3] Computar Distancias (Euclideana, M?ximo, Canberra, Binaria)"
    )
  
  Graficacion <-
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
  
  Procedimientos <- 
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

R4XCL_INT_DIALOGOS <- function()
{
  DialogosXCL <- list( 
    Descripcion        = "description",
    SetDatosX          = "Variables INDEPENDIENTES",
    SetDatosX.NS       = "Datos por Analizar",
    SetDatosX.MT       = "Seleccione la matriz cuadrada (incluir nombres de variables)",
    SetDatosY          = "Variables DEPENDIENTES",
    Categorica         = "Contiene sus datos información NO NUMERICA? 1:SI, 0:NO",
    Filtro             = "0:Incluir registro, 1:Excluir registro (0:Default)",
    Constante          = "Incluir constante de estimaci?n 1:SI, 0:NO (1:Default)",
    Escala             = "Escalar datos? 1:SI, 0:NO (0:Default)",
    Semilla            = "Valor de generaci?n aleatoria (ej:123456)",
    Ponderadores       = "Pesos a emplear en Mínimos Cuadrados",
    K                  = "Cantidad de Clusters requeridos",
    Koptimo            = "Cantidad máxima de Clusters por testear",
    Centroides         = "Centros obtenidos del proceso de K-medias",
    FactorEscalamiento = "Criterio de Escalamiento Original",
    SetDatosPredecir   = "Computar datos fuera de muestra",
    Detalle.Algebra    = "Cálculos matriciales para matrices pXp",
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
    TipoOutput.Lineal  = "1:Modelo, 2:Y Estimado, 3:Predicción, 4:Efectos Marginales, 5: Multicolinealidad, 6: Heterocedasticidad, 7:Estimaci?n Robusta, 8: Outliers, 9:Especificacion, 10: Salvar Modelo",
    TipoOutput.KM      = "1:Clasificaci?n, 2:Centros, 3:Variabilidad INTRA clase, 4:Variabilidad Total, 5:Parametros de Proceso, 6:GAP, 7:K-?ptimo",
    TipoOutput.ACP     = "1:Matriz de Correlaci?n, 2:Coordenadas: Variables, 3:Coordenadas: Individuos, 4:COS^2: INDs, 5:Contribuci?n:INDs, 6:Valores Propios, 7:COS^2: VARs, 8:Contribución:VARs, 9:Predicci?n, 10:Gr?fico VARs|INDs",
    TipoOutput.MT      = "1:Factorizaci?n Choleski, 2:Valores Propios, 3:Vectores Propios, 4:QR Decomposition, 5:Matriz Inversa, 6:Singular Value Decomposition, 7: Diagonal, 8:Transpuesta",
    Variable_i         = "Variable que identifica a los individuos",
    Variable_t         = "Variable que identifica el tiempo",
    ValorTruncamiento  = "Valor umbral del truncamiento",   
    DirTruncamiento    = "Negativo: Truncamiento por la izquierda, Positivo: Truncamiento por la derecha"
  )
  
  return(DialogosXCL)
}

R4XCL_INT_POR_INSTALAR <- function()
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