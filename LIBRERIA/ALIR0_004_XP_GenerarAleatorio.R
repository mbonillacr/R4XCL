#http://uc-r.github.io/generating_random_numbers/

XP_Generador_MUESTRA <- function(semilla, N_mue, MIN_mue, MAX_mue,REEMPLAZO)
{
  #_________________________________________________________________  
  # [1] PREPARACION DE DATOS 
  #_________________________________________________________________
  
  set.seed(semilla)
  
  #_________________________________________________________________ 
  # [2] PROCEDIMIENTO ANALITICO
  #_________________________________________________________________ 
  
  OutPut_0  = sample(MIN_mue:MAX_mue, N_mue, replace = REEMPLAZO) 
  OutPut_0  = data.frame("Seleccion Muestra: Con|Sin reemplazo"= OutPut_0)
  OutPut_1  = data.frame("Semilla empleada"= semilla)
  OutPut_A  = append(OutPut_1, OutPut_0)
  
  #_________________________________________________________________ 
  # [3] PREPARACION DE RESULTADOS
  #_________________________________________________________________       
  
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


XP_Generador_UNIFORME <- function(semilla, N_Uni, MIN_Uni, MAX_Uni)
{
  
  #_________________________________________________________________  
  # [1] PREPARACION DE DATOS 
  #_________________________________________________________________
  
  set.seed(semilla)
 
  #_________________________________________________________________ 
  # [2] PROCEDIMIENTO ANALITICO
  #_________________________________________________________________

  OutPut_0  = runif(N_Uni, min=MIN_Uni, max=MAX_Uni)
  OutPut_A  = data.frame(OutPut_0)
  
  #OJO CON ESTO, PORQUE ME PUEDE AYUDAR CON EL TEMA QUE TENGO EN ESPECIFICACION
  #EN QUE NO MUESTRO LA ECUACION COMO TAL
 
  NOMBRE_DATAFRAME = toString(paste0("F. UNIFORME con semilla: ", semilla, " , Valor Maximo:", MAX_Uni))
  colnames(OutPut_A)=NOMBRE_DATAFRAME
  #_________________________________________________________________ 
  # [3] PREPARACION DE RESULTADOS
  #_________________________________________________________________       
  
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


XP_Generador_NORMAL <- function(semilla, N_Nor, MU_Nor, SIG_Nor)
{
  
  #_________________________________________________________________  
  # [1] PREPARACION DE DATOS 
  #_________________________________________________________________
  
  set.seed(semilla)
  
  #_________________________________________________________________ 
  # [2] PROCEDIMIENTO ANALITICO
  #_________________________________________________________________ 

  OutPut_0  = rnorm(N_Nor, mean = MU_Nor, sd = SIG_Nor) 
  OutPut_A  = data.frame("Seudo Aleatorio: F. Normal"= OutPut_0)
 
  NOMBRE_DATAFRAME = toString(paste0("F. NORMAL con semilla: ", semilla,
                                     " , Media: ", MU_Nor, 
                                     " , Desviacion St: ", SIG_Nor)
                              )
  colnames(OutPut_A)=NOMBRE_DATAFRAME
   
  #_________________________________________________________________ 
  # [3] PREPARACION DE RESULTADOS
  #_________________________________________________________________       
  
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

XP_Generador_BINOMIAL <- function(semilla, N_Bin, Q_Sucesos, Prob_Bin)
{
  
  #_________________________________________________________________  
  # [1] PREPARACION DE DATOS 
  #_________________________________________________________________
  
  set.seed(semilla)
  
  #_________________________________________________________________ 
  # [2] PROCEDIMIENTO ANALITICO
  #_________________________________________________________________ 
  
  OutPut_0  = rbinom(N_Bin, size = Q_Sucesos, prob = Prob_Bin) 
  OutPut_A  = data.frame("Seudo Aleatorio: F. Normal"= OutPut_0)
  
  NOMBRE_DATAFRAME = toString(paste0("F. BINOMIAL con semilla: ", semilla, 
                                     " , Cantidad de Sucesos: ", Q_Sucesos, 
                                     " , Probabilidad: ", Prob_Bin)
                              )
  colnames(OutPut_A)=NOMBRE_DATAFRAME
  
  #_________________________________________________________________ 
  # [3] PREPARACION DE RESULTADOS
  #_________________________________________________________________       
  
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


XP_Generador_POISSON <- function(semilla, N_Poi, Lambda_Poi)
{
  
  #_________________________________________________________________  
  # [1] PREPARACION DE DATOS 
  #_________________________________________________________________
  
  set.seed(semilla)
  
  #_________________________________________________________________ 
  # [2] PROCEDIMIENTO ANALITICO
  #_________________________________________________________________ 
  
  OutPut_0  = rpois(N_Poi, lambda =  Lambda_Poi) 
  OutPut_A  = data.frame("Seudo Aleatorio: F. Normal"= OutPut_0)
  
  NOMBRE_DATAFRAME = toString(paste0("F. POISSON con semilla: ", semilla, 
                                     " Lambda: ", Lambda_Poi)
                              )
  colnames(OutPut_A)=NOMBRE_DATAFRAME
  
  #_________________________________________________________________ 
  # [3] PREPARACION DE RESULTADOS
  #_________________________________________________________________       
  
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