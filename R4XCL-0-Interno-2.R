R4XCL_INT_PROCEDIMIENTOS <- function()
{
  Reg_PanelData <- 
    c(
      "[01] Estimacion Datos de Panel", 
      "[02] Efectos Fijos para INDIVIDUOS",
      "[03] Test de Efectos Fijos para INDIVIDUOS",
      "[04] Test de Haussman para efectos aleatorios",
      "[05] Efectos Fijos para TIEMPO",
      "[06] Test de Breush-Pagan para efectos aleatorios en [t]",
      "[07] Test de Breush-Pagan para efectos aleatorios en [i]",
      "[08] Test de Breush-Pagan para efectos aleatorios en [t,i]",
      "[09] Test de Breush-Pagan para efectos aleatorios",
      "[10] Test de Breush-Pagan para correlación contemporánea",
      "[11] Test de Pesaran para correlación contemporánea",
      "[12] Test de correlacion serial",
      "[13] Test de raiz unitaria", 
      "[14] Especificación empleada",
      "[15] Guardar Modelo Estimado en RDS"
     )
  
  Reg_Lineal <- 
    c(
      "[01] Estimar Modelo de Regresión Lineal",
      "[02] Estimar valores para la [Y] (dentro de muestra)",
      "[03] Estimar valores para la [Y] (fuera de muestra)",
      "[04] Obtener efectos marginales",
      "[05] Calcular coeficiente de Inflación de Varianza VIF",
      "[06] Test de heterocedasticidad Breusch-Pagan", 
      "[07] Estimación robusta de coeficientes",
      "[08] Identificar observaciones de influencia en la muestra",
      "[09] Especificación empleada",
      "[10] Guardar Modelo Estimado en R",
      "[11] Modelo estimado con formato original de R",
      "[12] Obtener residuos estimados"
     )
  
  Reg_Binaria <-  
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
  
  Reg_Tobit <-  
    c(
      "[1] Estimar Modelo de Regresión Tobit",
      "[2] Estimar [Y] (dentro de muestra)",
      "[3] Estimar [Y] (fuera de muestra)",
      "[4] Especificación empleada",
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
        "[4] Especificación empleada",
        "[5] Guardar Modelo Estimado en R"
     )
  
  AD_KMedias <-  
    c(
      "[1] Cluster Asignado",
      "[2] Centros",
      "[3] Variabilidad intra clases",
      "[4] Variabilidad Total",
      "[5] Parámetros de Proceso",
      "[6] Test GAP",
      "[7] K-Optimo"
    )
  
  AD_ACP <- 
    c(
        "[01] Matriz de Correlaciones",
        "[02] Matriz de Covarianza",
        "[03] Gráfico de Correlaciones",
        "[04] Componentes Principales",
        "[05] Coordenadas de Individuos",
        "[06] COS^2 de Individuos",
        "[07] Contribución de individuos",
        "[08] Aporte a la Varianza del CP(i)",
        "[09] COS^2 de variables",
        "[10] Contribución de variables",
        "[11] Predicción fuera de muestra",
        "[12] Gráfico Biplot"
      )
  
  Mat_Algebra <-  
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
  
  Computo_Vars <-
    list(
          "[1] Crea variables Dummies",
          "[2] Centrar y/o estandarizar conjunto de Datos",
          "[3] Computar Distancias (Euclideana, Máximo, Canberra, Binaria)"
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
  
  Instalacion <-
    list(
          "[01] SVDIALOGS",
          "[02] DEVTOOLS",
          "[03] WORLDMAP",
          "[04] STARGAZER",
          "[05] PLM",
          "[06] RPART",
          "[07] RESOURCE SELECTION",
          "[08] TM",
          "[09] SNOWBALLC",
          "[10] WORLDCLOUD",
          "[11] PERFORMANCE ANALYTICS",
          "[12] RLANG",
          "[13] DUMMIES",
          "[99] TODOS"
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
      "GRAFICA" = Graficacion,
      "INSTALA" = Instalacion
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
    Categorica         = "Contiene sus datos información NO NUMÉRICA? 1:SI, 0:NO",
    Filtro             = "0:Incluir registro, 1:Excluir registro (0:Default)",
    Constante          = "Incluir constante de estimación 1:SI, 0:NO (1:Default)",
    Escala             = "Escalar datos? 1:SI, 0:NO (0:Default)",
    Semilla            = "Valor de generación aleatoria (ej:123456)",
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
    TipoOutput.Lineal  = "1:Modelo, 2:Y Estimado, 3:Predicción, 4:Efectos Marginales, 5: Multicolinealidad, 6: Heterocedasticidad, 7:Estimación Robusta, 8: Outliers, 9:Especificación, 10: Salvar Modelo",
    TipoOutput.KM      = "1:Clasificación, 2:Centros, 3:Variabilidad INTRA clase, 4:Variabilidad Total, 5:Parámetros de Proceso, 6:GAP, 7:K-?ptimo",
    TipoOutput.ACP     = "1:Matriz de Correlación, 2:Coordenadas: Variables, 3:Coordenadas: Individuos, 4:COS^2: INDs, 5:Contribución:INDs, 6:Valores Propios, 7:COS^2: VARs, 8:Contribución:VARs, 9:Predicción, 10:Gráfico VARs|INDs",
    TipoOutput.MT      = "1:Factorización Choleski, 2:Valores Propios, 3:Vectores Propios, 4:QR Decomposition, 5:Matriz Inversa, 6:Singular Value Decomposition, 7: Diagonal, 8:Transpuesta",
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