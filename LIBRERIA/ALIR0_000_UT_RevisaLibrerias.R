RevisarLibrerias<- function()
{  

  main_dir_orig   = "~/BERT2/"
  NombreCarpeta   = "RESULTADOS"
  
  Ruta_Local_0    = paste0(main_dir_orig, NombreCarpeta)
  
  {
    output_dir <- file.path(Ruta_Local_0)
    if (!dir.exists(output_dir)){dir.create(output_dir)}
  }
    
  PaquetesInstalados=rownames(installed.packages())
  Q_TotalPaquetes=length(PaquetesInstalados)

        r01="zoo"
        r02="quadprog"
        r03="xts"
        r04="curl"
        r05="gtools"
        r06="caTools"
        r07="bitops"
        r08="purrr"
        r09="dplyr"
        r10="data.table"
        r11="scales"
        r12="colorspace"
        r13="lazyeval"
        r14="plyr"
        r15="TTR"
        r16="fracdiff"
        r17="lmtest"
        r18="urca"
          
  RemoverPaquetes=c(
                    r01, r02, r03, r04, r05, r06, r07,
                    r08, r09, r10, r11, r12, r13, r14, 
                    r15, r16, r17, r18
                    )
  
        i01="tseries"       
        i02="DMwR"
        i03="woeBinning"
        i04="dplyr"      
        i05='ResourceSelection'
        i06='randomForest'
        i07='e1071'
        i08='rpart' 
        i09='Information'
        i10='sandwich'
        i11='pastecs'
        i12='mFilter'
        i13='InformationValue'
        i14='rpart.plot'

      InstalarPaquetes=c(
                         i01, i02, i03, i04, i05, i06, i07,
                         i08, i09, i10, i11, i12, i13, i14
                         )
    
      InstalarPaquetes=union(InstalarPaquetes,RemoverPaquetes)
    
      Q_PaquetesInstalar=length(InstalarPaquetes)
    
#Setdiff(A,B) retorna los elementos del conjunto A que no estan presentes en el conjunto B
#la funcion entonces evalua si los paquetes están o no instalados, sino estan LENGHT incrementa 
#1 por cada paquete no instalado.
    
      COND_01=setdiff(InstalarPaquetes, PaquetesInstalados)
      COND_02=length(COND_01)
      
      if  ( COND_02 > 0) 
          {
            try(install.packages(COND_01,dependencies = TRUE), silent=TRUE)
          }
    
      
      for (i in 1: Q_PaquetesInstalar)
          {
            try(library(InstalarPaquetes[i],character.only=TRUE), silent=TRUE)
          }

      return(InstalarPaquetes)
}

#==============================================================

#--------------------------------------------------------------
