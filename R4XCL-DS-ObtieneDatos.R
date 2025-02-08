DS_ObtenerDatos <- function()
    
  {

    library(svDialogs)
  
    #-------------------------->>>   
    # [1] PREPARACION DE DATOS Y PARAMETROS  
    #-------------------------->>>  
  
    DS = R4XCL_INT_POR_INSTALAR()
      A = dlg_list(DS,title="SELECCIONAR PAQUETE",preselect = 'wooldridge')
      pPaquete=A$res
  
    DS = R4XCL_INT_LISTA_DATASETS(pPaquete)
      A = dlg_list(DS,title="SELECCIONAR LIBRERIA")
      TipoOutput=A$res
    
    #-------------------------->>> 
    # [2] PROCEDIMIENTO ANALITICO
    #-------------------------->>> 
    
    
    
    #-------------------------->>> 
    # [3] PREPARACION DE RESULTADOS
    #-------------------------->>> 
    
    a=paste0(pPaquete,"::",TipoOutput)
    OutPut = eval(parse(text=a))
    
    #_________________________________________________________________   
    # [4] RESULTADO FINAL
    #_________________________________________________________________
    
    return(OutPut)
    
  }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++ 