DS_ObtenerDatos <- function()
    
  {
  
    #-------------------------->>>   
    # VALIDACIONES
    #-------------------------->>>  
    # En este caso, no hay validaciones previas necesarias, 
    # ya que la función se encarga de obtener los datos a través de un diálogo con el usuario.
    
    #-------------------------->>>   
    # PREPARACION DE DATOS Y PARAMETROS  
    #-------------------------->>>  
    
    library(svDialogs)
  
    #-------------------------->>>   
    # [1] PREPARACION DE DATOS Y PARAMETROS  
    #-------------------------->>>  
  
    DS <- R4XCL_INT_POR_INSTALAR()
      A <- dlg_list(DS,title="SELECCIONAR PAQUETE",preselect = 'wooldridge')
      pPaquete=A$res
  
      DS <- R4XCL_INT_LISTA_DATASETS(pPaquete)
      if (length(DS) == 0) {
        stop("Error: No se encontraron datasets en el paquete seleccionado.")
      }
      
      A <- dlg_list(DS,title="SELECCIONAR LIBRERIA")
      TipoOutput=A$res
    
      #-------------------------->>> 
      # OBTENER DATOS
      #-------------------------->>> 
      
      OutPut <- get(pDataset, pos = paste0("package:", pPaquete))
      
      #-------------------------->>> 
      # RESULTADO FINAL
      #-------------------------->>> 
      
      return(OutPut)
    
  }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++ 