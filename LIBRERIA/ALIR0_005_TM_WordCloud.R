
WordCloud<-function(pTEXTO, pMAX_PALABRAS){

          ##Install Packages
          install.packages("tm")  # for text mining
          install.packages("SnowballC") # for text stemming
          install.packages("wordcloud") # word-cloud generator 
          install.packages("RColorBrewer") # color palettes
          ##Load Require Library
          library(tm)
          library(SnowballC)
          library(RColorBrewer)
          library(wordcloud)
          
          ##Read the Data
          textoDS<-read.csv(pTEXTO)
          
          textoDS<-data.frame(textoDS)
          
          textoDS.Corpus<-Corpus(VectorSource(textoDS$content))
          
          textoDS.Clean<-tm_map(textoDS.Corpus,PlainTextDocument)
          textoDS.Clean<-tm_map(textoDS.Corpus,tolower)
          
          textoDS.Clean<-tm_map(textoDS.Clean,removeNumbers)
          textoDS.Clean<-tm_map(textoDS.Clean,removeWords,stopwords("english"))
          textoDS.Clean<-tm_map(textoDS.Clean,removePunctuation)
          textoDS.Clean<-tm_map(textoDS.Clean,stripWhitespace)
          textoDS.Clean<-tm_map(textoDS.Clean,stemDocument)
          
          wordcloud(textoDS.Clean,max.words = pMAX_PALABRAS,random.color = TRUE,random.order=FALSE)

}