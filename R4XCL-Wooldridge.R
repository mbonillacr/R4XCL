DS_Wooldridge <- function(pPaqueteSeleccionado=0)
    
  {
  
    #-------------------------->>>   
    # VALIDACIONES
    #-------------------------->>>  
    # En este caso, no hay validaciones previas necesarias, 
    # ya que la función se encarga de obtener los datos a través de un diálogo con el usuario.
    
    #-------------------------->>>   
    # PREPARACION DE DATOS Y PARAMETROS  
    #-------------------------->>>  
    

    #-------------------------->>>   
    # [1] PREPARACION DE DATOS Y PARAMETROS  
    #-------------------------->>>  
  
      pPaquete=
  
      DS <- data.frame(R4XCL_INT_LISTA_DATASETS('wooldridge'))

       #if(pPaqueteSeleccionado==0)
         {output=DS}
       # else
       #   {
       #  pDataset <- DS[pPaqueteSeleccionado]
       #  print(paste("Intentando cargar el dataset:", pDataset)) 
       #  OutPut   <- get(pDataset, pos = paste0("package:", pPaquete))
      # }
      
    
      #-------------------------->>> 
      # OBTENER DATOS
      #-------------------------->>> 
      
      
      
      #-------------------------->>> 
      # RESULTADO FINAL
      #-------------------------->>> 
      
      return(OutPut)
    
  }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++ 