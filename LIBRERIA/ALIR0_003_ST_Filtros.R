ST_Filtro_BW <- function(DS, PER=1, pNFix, pDrift)
{
  library(tseries)  #revisar sacar cosas de aca
  library(mFilter)
  
  #_________________________________________________________________  
  # [1] PREPARACION DE DATOS 
  #_________________________________________________________________
  
  NOMBRE_DS=DS[1,1]
  
  DS=na.omit(as.numeric(DS))  
  
  DS_TS=ts(DS,frequency=PER, 1)
  
  nn=nrow(DS_TS)
  pp=ncol(DS_TS)
  
  DIMENSION=  paste("F: ",nn, "C:",pp, space=" ")
  
  #_________________________________________________________________ 
  # [2] PROCEDIMIENTO ANALITICO
  #_________________________________________________________________ 
 
  OutPut_0  = bwfilter(DS_TS, freq=PER, nfix=pNFix, drift=pDrift) 
    
  OutPut_1  = data.frame(paste0("Parametros: Periodicidad = ", PER,
                                " , nFix = ",pNFix,
                                " , Drift = ", pDrift 
                                )
                        )
  
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


#-----------------------------------------------------------------------


ST_Filtro_HP <- function(DS, PER=1, pDrift)
{
  library(tseries)  #revisar sacar cosas de aca
  library(mFilter)
  
  #_________________________________________________________________  
  # [1] PREPARACION DE DATOS 
  #_________________________________________________________________
  
  NOMBRE_DS=DS[1,1]
  
  DS=na.omit(as.numeric(DS))  
  
  DS_TS=ts(DS,frequency=PER, 1)
  
  nn=nrow(DS_TS)
  pp=ncol(DS_TS)
  
  DIMENSION=  paste("F: ",nn, "C:",pp, space=" ")
  
  if (PER==12){Lambda=14400}
  if (PER==4){Lambda=1600}
  if (PER==2){Lambda=800}
  if (PER==1){Lambda=100}
  
  #_________________________________________________________________ 
  # [2] PROCEDIMIENTO ANALITICO
  #_________________________________________________________________ 
  
  OutPut_0  = hpfilter(DS_TS, freq=Lambda, type="lambda", drift=pDrift) 
  
  OutPut_1  = data.frame(paste0("Parametros: Periodicidad = ", PER,
                                " , Tipo = frequency",
                                " , Drift = ", pDrift 
                                )
                         )
  
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

# ---------------------------------------------------------

ST_Filtro_MF <- function(DS, PER=1, pTipoFiltro)
{
  library(tseries)  #revisar sacar cosas de aca
  library(mFilter)
  
  #_________________________________________________________________  
  # [1] PREPARACION DE DATOS 
  #_________________________________________________________________
  
  NOMBRE_DS=DS[1,1]
  
  DS=na.omit(as.numeric(DS))  
  
  DS_TS=ts(DS,frequency=PER, 1)
  
  nn=nrow(DS_TS)
  pp=ncol(DS_TS)
  
  DIMENSION=  paste("F: ",nn, "C:",pp, space=" ")
  
  if (pTipoFiltro==0){pTipoFiltro="HP"} # Hodrick-Prescott filter
  if (pTipoFiltro==1){pTipoFiltro="BK"} # Baxter-King filter
  if (pTipoFiltro==2){pTipoFiltro="CF"} # Christiano-Fitzgerald filter 
  if (pTipoFiltro==3){pTipoFiltro="BW"} # Butterworth filter
  if (pTipoFiltro==4){pTipoFiltro="TR"} # Trigonometric regression filter
  
  message(pTipoFiltro)
  
  #_________________________________________________________________ 
  # [2] PROCEDIMIENTO ANALITICO
  #_________________________________________________________________ 
  
  OutPut_0  = mFilter(DS_TS, filter=pTipoFiltro) 

  OutPut_1  = data.frame(paste0("Parametros: Periodicidad = ", PER,
                                " , Filtro empleado = ", pTipoFiltro 
  )
  )
  
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

# ---------------------------------------------------------

ST_Filtro_TR <- function(DS, PER=1, pDrift)
{
  library(tseries)  #revisar sacar cosas de aca
  library(mFilter)
  
  #_________________________________________________________________  
  # [1] PREPARACION DE DATOS 
  #_________________________________________________________________
  
  NOMBRE_DS=DS[1,1]
  
  DS=na.omit(as.numeric(DS))  
  
  DS_TS=ts(DS,frequency=PER, 1)
  
  nn=nrow(DS_TS)
  pp=ncol(DS_TS)
  
  DIMENSION=  paste("F: ",nn, "C:",pp, space=" ")
  
  #_________________________________________________________________ 
  # [2] PROCEDIMIENTO ANALITICO
  #_________________________________________________________________ 
  
  OutPut_0  = trfilter(DS_TS, drift = pDrift) 
  
  
  OutPut_1  = data.frame(paste0("Parametros: Periodicidad = ", PER,
                                " , Tipo = frequency",
                                " , Drift = ", pDrift
                                )
                        )
  
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

#-----------------------------------------------------------

ST_Filtro_BK <- function(DS, PER=1, pDrift)
{
  #library(tseries)  revisar sacar cosas de aca
  library(mFilter)
  
  #_________________________________________________________________  
  # [1] PREPARACION DE DATOS 
  #_________________________________________________________________
  
  NOMBRE_DS=DS[1,1]
  
  DS=na.omit(as.numeric(DS))  
  
  DS_TS=ts(DS,frequency=PER, 1)
  
  nn=nrow(DS_TS)
  pp=ncol(DS_TS)
  
  DIMENSION=  paste("F: ",nn, "C:",pp, space=" ")
  
  #_________________________________________________________________ 
  # [2] PROCEDIMIENTO ANALITICO
  #_________________________________________________________________ 
  
  OutPut_0  = mFilter::bkfilter(DS_TS, drift = pDrift) 
  
  OutPut_1  = data.frame(paste0("Parametros: Periodicidad = ", PER,
                                " , Tipo = frequency",
                                " , Drift = ", pDrift
                                )
                          )
  
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