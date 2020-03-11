EliminarLibrerias<- function()
{  

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
  
  RemoverPaquetes=c(
                    r01, r02, r03, r04, r05, r06, r07,
                    r08, r09, r10, r11, r12, r13, r14, 
                    r15, r16, r17 
                    )
  
        Q_PaquetesRemover=length(RemoverPaquetes)
        
        COND_01=intersect(RemoverPaquetes, PaquetesInstalados)
        
        Valida=length(COND_01)
        
        COND_02=length(COND_01)
        
        if  ( COND_02 > 0) 
        {
          try(remove.packages(COND_01),silent=TRUE)
        }

      return(RemoverPaquetes)
}

#==============================================================

#---------------------------------------------------------------


  
