#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MAPA ISO3
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

MP_MapaISO03 <- function(NombrePaises, Valores, Titulo )
  {

  #-------------------------->>>
  # VALIDACIONES
  #-------------------------->>>
  # if (missing(NombrePaises)) {
  #   stop("Error: NombrePaises es un parámetro obligatorio.")
  # }
  # if (missing(Valores)) {
  #   stop("Error: Valores es un parámetro obligatorio.")
  # }
  # if (missing(Titulo)) {
  #   stop("Error: Titulo es un parámetro obligatorio.")
  # }
  # if (!is.character(NombrePaises)) {
  #   stop("Error: NombrePaises debe ser un vector de caracteres.")
  # }
  # if (!is.numeric(Valores)) {
  #   stop("Error: Valores debe ser un vector numérico.")
  # }
  # if (length(NombrePaises)!= length(Valores)) {
  #   stop("Error: NombrePaises y Valores deben tener la misma longitud.")
  # }
  
  #-------------------------->>>
  # PREPARACION DE DATOS Y PARAMETROS
  #-------------------------->>>
  
  library(rworldmap)
  
  BERT.graphics.device(cell = T)
  
  DF <- data.frame(
                   Pais = NombrePaises,
                   Valor = Valores  
                   )
  
  ## Re-merge
  Mapa <- joinCountryData2Map(
                              DF, 
                              joinCode = "ISO3",
                              nameJoinColumn = "Pais"
                              )
  
  ## Specify the colourPalette argument
  mapCountryData(Mapa, 
                 nameColumnToPlot="Valor", 
                 catMethod = "categorical",
                 missingCountryCol = gray(.8), 
                 colourPalette = "diverging",
                 mapTitle=Titulo,
                 oceanCol="black")
  
  dev.off();
  #-------------------------->>>
  # RESULTADO FINAL
  #-------------------------->>>
  
  return(invisible(TRUE))
  
}
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# FIN DE PROCEDIMIENTO
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++