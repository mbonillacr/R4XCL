#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# TEXT MINING                                          +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

TM_TextMining <- function(
                          RUTA_FL=NULL,
                          RUTA_SW=NULL,
                          MAXWORDS=200, 
                          QPALABRASRESUMEN=100,
                          IDIOMA=1,
                          TipoOutput=0
                          )
{
  #http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know
  
  library(tm)
  library(SnowballC)
  library(wordcloud)
  library(RColorBrewer)
  require(svDialogs)
  require(tcltk)
  
  #print(file.choose(new = FALSE))

  
  #-------------------------->>>   
  # [1] PREPARACION DE DATOS Y PARAMETROS  
  #-------------------------->>>  
  
  set.seed(123456)

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
    
    OutPut = "WordCloud"
  
  }else if(TipoOutput == 1){   
    
    BERT.graphics.device(cell=T);
    OutPut = "Word Cloud";
    wordcloud(
              words = d$word, 
              freq = d$freq, 
              min.freq = 1,
              max.words=MAXWORDS, 
              random.order=FALSE, 
              rot.per=0.35, 
              colors=brewer.pal(8, "Dark2")
              );
    
    dev.off();
    T;

  }else if(TipoOutput == 2){   
    
    OutPut = head(d, QPALABRASRESUMEN) 
    
  }else if(TipoOutput == 3){   
  
    OutPut = "Word Cloud"
    
    barplot(
            d[1:10,]$freq, 
            las = 2, 
            names.arg = d[1:10,]$word,
            col  = "red", 
            main = "Palabras m?s Frecuentes",
            ylab = "Frecuencia"
           )
    
  }else if(TipoOutput == 4){   
  
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