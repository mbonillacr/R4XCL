#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# TEXT MINING                                          +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

TM_TextMining <- function(RUTA_FL=NULL,
                          RUTA_SW=NULL,
                          MAXWORDS=200, 
                          QPALABRASRESUMEN=100,
                          IDIOMA=1,
                          TipoOutput=0
                          )
{
  
  #-------------------------->>>   
  # VALIDACIONES
  #-------------------------->>>  
  if (!is.character(RUTA_FL) ||!file.exists(RUTA_FL)) {
    stop("Error: RUTA_FL debe ser una ruta válida a un archivo de texto.")
  }
  if (!is.null(RUTA_SW) && (!is.character(RUTA_SW) ||!file.exists(RUTA_SW))) {
    stop("Error: RUTA_SW debe ser una ruta válida a un archivo de texto.")
  }
  
  #http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know
  
  library(tm)
  library(SnowballC)
  library(wordcloud)
  library(RColorBrewer)
  
  require(svDialogs)
  require(tcltk)
  
  #-------------------------->>>   
  # [1] PREPARACION DE DATOS Y PARAMETROS  
  #-------------------------->>>  
  
  set.seed(123456)

  DIR_ORIG  = "~/BERT2/functions/INTERNO/"
  ARCHIVO   = paste0(DIR_ORIG,"R4XCL-INTERNO.R")

  source(file.path(ARCHIVO))
  
  #ListaFunciones = c("Resumen de Datos", "Selecci?n Muestral", "POR LLENAR CON ALGUN METODO")
  #TipoDialogo    = c("ok", "okcancel", "yesno", "yesnocancel")
  IDIOMA         = c("spanish","english")
  
  #-------------------------->>> 
  # [2] PROCEDIMIENTO ANALITICO
  #-------------------------->>> 

  text     = readLines(RUTA_FL)
  docs     = Corpus(VectorSource(text))
  
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  docs    <- tm_map(docs, toSpace, "/")
  docs    <- tm_map(docs, toSpace, "@")
  docs    <- tm_map(docs, toSpace, "\\|")
  
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))

  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords(IDIOMA[1]))
  
  # Remove your own stop word specify your stopwords as a character vector
  
  if (!is.null(RUTA_SW)) {
    StopWords = readLines(RUTA_SW)
    docs <- tm_map(docs, removeWords, StopWords)
  }
  
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  
  # Text stemming
  docs <- tm_map(docs, stemDocument)
  
  dtm <- TermDocumentMatrix(docs)
  m   <- as.matrix(dtm)
  v   <- sort(rowSums(m),decreasing=TRUE)
  
  d   <- data.frame(word = names(v),freq=v)
  
  #-------------------------->>> 
  # [3] PREPARACION DE RESULTADOS
  #-------------------------->>> 
  
  if (TipoOutput == 0){

    OutPut = "Ver Gráfico"
    wordcloud(
              words = d$word, 
              freq = d$freq, 
              min.freq = 1,
              max.words=MAXWORDS, 
              random.order=FALSE, 
              rot.per=0.35, 
              colors=brewer.pal(8, "Dark2")
              )

  }else if(TipoOutput == 1){   
    
    OutPut = head(d, QPALABRASRESUMEN) 
    
  }else if(TipoOutput == 2){   
  
    OutPut = "Ver Gráfico"
    
    barplot(
            d[1:10,]$freq, 
            las = 2, 
            names.arg = d[1:10,]$word,
            col  = "red", 
            main = "Palabras m?s Frecuentes",
            ylab = "Frecuencia"
           )
    
  }else if(TipoOutput == 3){   
  
    OutPut = findFreqTerms(dtm, lowfreq = 4)
    
  }else{   
  
    OutPut = "Revisar parámetros disponibles" 
    
  }  
  
  #_________________________________________________________________   
  # [4] RESULTADO FINAL
  #_________________________________________________________________
  
  return(OutPut)
  
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

Detalle = "Realiza proceso de Text Mining"

attr(TM_TextMining, "description") = 
  list(
        Detalle,
        RUTA = "Ruta: Archivo de texto a procesar",
        RUTA_SW= "Ruta: Palabras por despreciar",
        MAXWORDS="Cantidad máxima de palabras a procesar", 
        QPALABRASRESUMEN="Cantidad de palabras a mostrar como resultado",
        IDIOMA="1: Español, 2:Inglés",
        TipoOutput="0:WordClud, 1:Frecuencia (tab.), 2:Frecuencia (gr?f.), 3: Términos Frecuentes"
       )