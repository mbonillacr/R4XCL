#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# TEST DE COINTEGRACION
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

UT_Referencias <- function()
{

  #-------------------------->>>   
  # [1] PREPARACION DE DATOS Y PARAMETROS  
  #-------------------------->>>  

  library(svDialogs)
  
  opciones= c(
              "Documentos oficiales en PDF de librerias empleadas",
              "Tabla de sugerencias"
              )
  
  Instrucciones = c(
                    "Seleccione el tipo de referencias disponibles",
                    ""
                    )
  
  
  A = dlg_list(
    opciones, 
    multiple = FALSE, 
    title = paste0(Instrucciones[1]," ")
  )
  
  pSeleccion=A$res
  #-------------------------->>> 
  # [2] PROCEDIMIENTO ANALITICO
  #-------------------------->>> 

  #-------------------------->>> 
  # [3] PREPARACION DE RESULTADOS
  #-------------------------->>> 
  
  
  
  if (pSeleccion == opciones[1]){
  
    pPath ="~/BERT2/functions/R4XCL HELP/"
    pListFiles=list.files(pPath)
    
    library(svDialogs)
    A = dlg_list(pListFiles)
    pArchivoElegido=A$res
    
    path_file=paste0(pPath,"/", pArchivoElegido)
    
    library(fs)
    file_show(normalizePath(path_file))
    
    OutPut  = "Ver documento PDF"
    
  } else if (pSeleccion == opciones[2]){  

    tabla=rbind(
              c("TEMA", "RECURSO", 'ENLACE'),
              c("Econometría Series de Tiempo", "Libro Web", "https://nwfsc-timeseries.github.io/atsa-labs/chap-mss.html"),
              c("Econometría Series de Tiempo", "Libro Web", "https://bookdown.org/ccolonescu/RPoE4/intro.html"),
              c("mFilter","Documentación R","https://cran.r-project.org/web/packages/mFilter/mFilter.pdf"),
              c("Análisis de Componentes Principales","Web","https://rstudio-pubs-static.s3.amazonaws.com/585948_abd70a6fc3e24d4fad8944197bc5dd25.html"),
              c("Estadística Aplicada","Web","https://stats.idre.ucla.edu/other/dae/"),
              c("Econometría Series de Tiempo: VEC", "Libro Web", "https://www.r-econometrics.com/"),
              c("Journal","Journal of Statistical Software" , "https://www.jstatsoft.org/")
              )  
    
  OutPut  = tabla

  }
  
  #-------------------------->>> 
  # [4] RESULTADO FINAL
  #-------------------------->>> 
 
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

Detalle = "Algunas referencias que en lo personal, considero valiosas (Minor Bonilla)"

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# TEST DE RAIZ UNITARIA DF | PP
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++