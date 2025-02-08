MP_MapaISO03 <- function(NombrePaises, Valores, Titulo )
  {

  library(rworldmap)
  
  BERT.graphics.device(cell=T);
  
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
  T;
  
  }