#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# IMPORTAR ARCHIVO EXTERNO GRANDE                      +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

UT_ArchivoExternoLRG = function()
{
  
  #-------------------------->>>   
  # VALIDACIONES
  #-------------------------->>>  
  # En este caso, no hay validaciones previas necesarias, 
  # ya que la función se encarga de solicitar la ruta del archivo al usuario.
  
  require(svDialogs)
  require(tcltk)
  require(dplyr)
  
  #-------------------------->>>   
  # [1] PREPARACION DE DATOS Y PARAMETROS  
  #-------------------------->>>  
  
  
  #-------------------------->>> 
  # [2] PROCEDIMIENTO ANALITICO
  #-------------------------->>> 
  
  ListaFunciones=c("Resumen de Datos", "Selección Muestral", "POR LLENAR CON ALGUN METODO")
  TipoDialogo = c("ok", "okcancel", "yesno", "yesnocancel")
  MSG1="Seleccione el archivo a cargar"
  
  dlg_message(message=MSG1, type = TipoDialogo[1] )
  
  pArchivo = tk_choose.files()  
  MSG2     = paste0("Archivo seleccionado: ",pArchivo," Desea Continuar?")
  msgBox   = dlg_message(message=MSG2, type = TipoDialogo[3] )
  
  if (msgBox$res == "no") {return(NULL)}
  
  SetDatosX=read.csv(pArchivo)
  p =ncol(SetDatosX)
  
  nombresX=paste0(SetDatosX[1,1:p])
  
  DatosX=SetDatosX[-1,]

  colnames(DatosX)[1:p]= nombresX[1:p]
  
  message(class(DatosX))
  
  A = dlg_list(ListaFunciones)
  seleccionado=A$res
  
  #-------------------------->>> 
  # [3] PREPARACION DE RESULTADOS
  #-------------------------->>> 
  
  if (seleccionado == "Resumen de Datos")
    
  {
    
    proceso="summary("
    A=eval(parse(text=paste0(proceso,"DatosX",")")))
    OutPut=capture.output(A)
    
  } else if(seleccionado == "Selección Muestral") {
    
    a=dlg_input(message = "Cantidad de Elementos a Seleccionar")
    N=a$res
    proceso="dplyr::sample_n("
    A=eval(parse(text=paste0(proceso,"DatosX,",N,")")))
    OutPut=A 
    
  } else if(seleccionado == "Histogramas de Datos Contenidos") {
    
    SetDatosX=as.numeric(SetDatosX)
    FX='hist'
    proceso=paste0("lapply(DatosX[2:p], FUN=",FX,")")
    
    
    A=eval(parse(text=proceso))
    OutPut="Ver Gráfico"
    
  }
  
  #-------------------------->>> 
  # [4] RESULTADO FINAL
  #-------------------------->>> 
  
  return(OutPut)  
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
Detalle = "Importa un archivo externo grande y realiza un análisis exploratorio."
attr(UT_ArchivoExternoLRG, "description" ) = 
  list( 
    Detalle,
    #TipoOutput= "0:INCLUIR DESCRIPCION, 1:INCLUIR DESCRIPCION, 2:, 3:, 4:"
  )