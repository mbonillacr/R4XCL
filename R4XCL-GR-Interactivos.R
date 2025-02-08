#------------------------------------>>> 
#  Crea grafico Interavtivo
#------------------------------------>>> 

GR_GraficoInteractivo = function(TipoOutput=1)
{

  #-------------------------->>>   
  # [1] PREPARACION DE DATOS Y PARAMETROS  
  #-------------------------->>>  
  
  # p = ncol(SetDatosX)
  # n = nrow(SetDatosX)-1
  # 
  # nombresX=paste0(SetDatosX[1,1:p])
  # 
  # DatosX=SetDatosX[-1,]
  # 
  # Procedimientos=R4XCL_INT_PROCEDIMIENTOS()
  
  #-------------------------->>> 
  # [2] PROCEDIMIENTO ANALITICO
  #-------------------------->>> 
  
  
  
  #-------------------------->>> 
  # [3] PREPARACION DE RESULTADOS
  #-------------------------->>> 
  
  if (TipoOutput <= 0){
    
    OutPut = Procedimientos$COMPUTOS
    
  }else if(TipoOutput == 1){  
    
    library(magrittr)
    library(highcharter)
    library(htmlwidgets)
    
    pPath ="~/BERT2/functions/"
    pListFiles=list.files(pPath)
    
    fig <- plot_ly(
                  type="treemap",
                  labels=c("Eve", "Cain", "Seth", "Enos", "Noam", "Abel", "Awan", "Enoch", "Azura"),
                  parents=c("", "Eve", "Eve", "Seth", "Seth", "Eve", "Eve", "Awan", "Eve")
                  )
    
    path_file=paste0(pPath,"/EjemploPlotly.html", pArchivoElegido)
    path_file=normalizePath(path_file)
    htmlwidgets::saveWidget(fig,path_file )
    
    library(fs)
    file_show(path_file)
    
    OutPut = "Ver gráfico"
  }
  #-------------------------->>> 
  # [4] RESULTADO FINAL
  #-------------------------->>> 
  
  return(OutPut)  
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
Detalle = "Crea TREEMAP"
attr(GR_GraficoInteractivo, "description" ) = 
  list( 
    Detalle,
    SetDatosIndice = "Conjunto de de datos de grupos",
    SetDatosValor  = "Contiene los valores"
  )