BigData <- function(pUSO,SP,pPARAMETROS)
  
  {

#-------------------------------------------------
#   LIBRERIAS
#-------------------------------------------------  

      library(RODBC)
  
#-------------------------------------------------
#   PARAMETRIZACION
#-------------------------------------------------  
  
        pHOME = Sys.getenv("HOME")
        setwd(paste0(pHOME, "/BERT2/BukloLab/OUTPUT"))
        
        PreparaAplicacion = readRDS(paste0(pHOME, "/BERT2/BukloLab/FNCN/fx.rds"))
        Datos=PreparaAplicacion(pUSO,SP,pPARAMETROS)

        ARCHIVO=paste0(SP,".csv")
        write.csv(Datos, ARCHIVO)

#-------------------------->>> 
#RESULTADO FINAL
#-------------------------->>>   

        return(TRUE)

}


#-------------------------------------------------
#   *********************************************
#   *********************************************
#-------------------------------------------------  


BigData_Selector <- function(pUSO,SP,pPARAMETROS)
  
{
  
  #-------------------------------------------------
  #   LIBRERIAS
  #-------------------------------------------------  
  
  library(RODBC)
  
  #-------------------------------------------------
  #   PARAMETRIZACION
  #-------------------------------------------------  
  
  pHOME = Sys.getenv("HOME")
  setwd(paste0(pHOME, "/BERT2/BukloLab/OUTPUT"))
  
  PreparaAplicacion = readRDS(paste0(pHOME, "/BERT2/BukloLab/FNCN/fx.rds"))
  Datos=PreparaAplicacion(pUSO,SP,pPARAMETROS)
  
  qElementos=as.matrix(Datos)
  
  #-------------------------->>> 
  #RESULTADO FINAL
  #-------------------------->>>   
  
  return(qElementos)
  
}
