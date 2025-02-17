# Ejemplo de uso:
nombres_paquetes <- c(
                    
                      #Instalando SVDIALOGS
                      "svMisc_1.0-1.tar.gz", "svGUI_0.9-55.tar.gz", "svDialogs_0.9-50.tar.gz"#,
                    
                      # #Instalando DEVTOOLS
                      # "R6_2.2.2.tar.gz","openssl_1.0.1.tar.gz","httr_1.3.1.tar.gz",
                      # "digest_0.6.15.tar.gz","digest_0.6.15.tar.gz","whisker_0.3-2.tar.gz",
                      # "rstudioapi_0.7.tar.gz","git2r_0.21.0.tar.gz","withr_2.1.2.tar.gz",
                      # "devtools_1.13.5.tar.gz",
                      # 
                      # #Instalando RWORLDMAP
                      # "dotCall64_0.9-5.tar.gz","spam_2.1-4.tar.gz","fields_9.0.tar.gz",
                      # "Rcpp_0.12.16.tar.gz","raster_2.6-7.tar.gz","terra_0.5-2.tar.gz",
                      # "maptools_0.9-2.tar.gz","rworldmap_1.3-1.tar.gz",
                      # 
                      # #Instalando STARGAZER
                      # "stargazer_5.2.1.tar.gz",
                      # 
                      # #Instalando PLM 
                      # "Formula_1.2-3.tar.gz","bdsmatrix_1.3-3.tar.gz","zoo_1.8-1.tar.gz",
                      # "sandwich_2.4-0.tar.gz","lmtest_0.9-36.tar.gz","miscTools_0.6-22.tar.gz",
                      # "maxLik_1.3-4.tar.gz","plm_1.6-6.tar.gz",
                      # 
                      # # Instalando rpart.plot
                      # "rpart.plot_2.2.0.tar.gz",
                      # 
                      # # Instalando ResourceSelection
                      # "pbapply_1.3-4.tar.gz","data.table_1.11.2.tar.gz","prediction_0.3.2.tar.gz",
                      # "margins_0.3.0.tar.gz","ResourceSelection_0.3-2.tar.gz",
                      # 
                      # # Instalando VGAM
                      # "VGAM_1.0-5.tar.gz"
                      
                      )  

directorio_paquetes <- "M:/R4XCL/LIBRERIA/LIBRERIAS R/"  # Reemplaza con la ruta a tu directorio

if (R4XCL_INSTALAR_PAQUETES(nombres_paquetes, directorio_paquetes)) {
  cat("Todos los paquetes se instalaron y cargaron correctamente.\n")
} else {
  cat("La instalación de paquetes se detuvo debido a un error.\n")
}

# Instalando SVDIALOGS
install.packages("https://cran.r-project.org/src/contrib/Archive/svMisc/svMisc_1.0-1.tar.gz", repos=NULL, dependencies = TRUE)
install.packages("https://cran.r-project.org/src/contrib/Archive/svGUI/svGUI_0.9-55.tar.gz", repos=NULL, dependencies = TRUE)
install.packages("https://cran.r-project.org/src/contrib/Archive/svDialogs/svDialogs_0.9-50.tar.gz", repos=NULL, dependencies = TRUE)

# Instalando DEVTOOLS
install.packages("https://cran.r-project.org/src/contrib/Archive/R6/R6_2.2.2.tar.gz", repos=NULL, dependencies = TRUE)
install.packages("https://cran.r-project.org/src/contrib/Archive/openssl/openssl_1.0.1.tar.gz", repos=NULL, dependencies = TRUE)
install.packages("https://cran.r-project.org/src/contrib/Archive/httr/httr_1.3.1.tar.gz", repos=NULL, dependencies = TRUE)
install.packages("https://cran.r-project.org/src/contrib/Archive/digest/digest_0.6.15.tar.gz", repos=NULL, dependencies = TRUE)
install.packages("https://cran.r-project.org/src/contrib/Archive/memoise/digest_0.6.15.tar.gz", repos=NULL, dependencies = TRUE)
install.packages("https://cran.r-project.org/src/contrib/Archive/whisker/whisker_0.3-2.tar.gz", repos=NULL, dependencies = TRUE)
install.packages("https://cran.r-project.org/src/contrib/Archive/rstudioapi/rstudioapi_0.7.tar.gz", repos=NULL, dependencies = TRUE)
install.packages("https://cran.r-project.org/src/contrib/Archive/git2r/git2r_0.21.0.tar.gz", repos=NULL, dependencies = TRUE)
install.packages("https://cran.r-project.org/src/contrib/Archive/withr/withr_2.1.2.tar.gz", repos=NULL, dependencies = TRUE)
install.packages("https://cran.r-project.org/src/contrib/Archive/devtools/devtools_1.13.5.tar.gz", repos=NULL, dependencies = TRUE)

# Instalando RWORLDMAP
install.packages("https://cran.r-project.org/src/contrib/Archive/dotCall64/dotCall64_0.9-5.tar.gz", repos=NULL, dependencies = TRUE)
install.packages("https://cran.r-project.org/src/contrib/Archive/spam/spam_2.1-4.tar.gz", repos=NULL, dependencies = TRUE)
install.packages("https://cran.r-project.org/src/contrib/Archive/fields/fields_9.0.tar.gz", repos=NULL, dependencies = TRUE)
install.packages("https://cran.r-project.org/src/contrib/Archive/Rcpp/Rcpp_0.12.16.tar.gz", repos=NULL, dependencies = TRUE)
install.packages("https://cran.r-project.org/src/contrib/Archive/raster/raster_2.6-7.tar.gz", repos=NULL, dependencies = TRUE)
install.packages("https://cran.r-project.org/src/contrib/Archive/terra/terra_0.5-2.tar.gz", repos=NULL, dependencies = TRUE)
install.packages("https://cran.r-project.org/src/contrib/Archive/maptools/maptools_0.9-2.tar.gz", repos=NULL, dependencies = TRUE)
install.packages("https://cran.r-project.org/src/contrib/Archive/rworldmap/rworldmap_1.3-1.tar.gz", repos=NULL, dependencies = TRUE)

# Instalando STARGAZER
install.packages("https://cran.r-project.org/src/contrib/Archive/stargazer/stargazer_5.2.1.tar.gz", repos=NULL, dependencies = TRUE)

# Instalando PLM ('Formula', 'bdsmatrix', 'zoo', 'sandwich', 'lmtest', 'maxLik')
install.packages("https://cran.r-project.org/src/contrib/Archive/Formula/Formula_1.2-3.tar.gz", repos=NULL, dependencies = TRUE)
install.packages("https://cran.r-project.org/src/contrib/Archive/bdsmatrix/bdsmatrix_1.3-3.tar.gz", repos=NULL, dependencies = TRUE)
install.packages("https://cran.r-project.org/src/contrib/Archive/zoo/zoo_1.8-1.tar.gz", repos=NULL, dependencies = TRUE)
install.packages("https://cran.r-project.org/src/contrib/Archive/sandwich/sandwich_2.4-0.tar.gz", repos=NULL, dependencies = TRUE)
install.packages("https://cran.r-project.org/src/contrib/Archive/lmtest/lmtest_0.9-36.tar.gz", repos=NULL, dependencies = TRUE)
install.packages("https://cran.r-project.org/src/contrib/Archive/miscTools/miscTools_0.6-22.tar.gz", repos=NULL, dependencies = TRUE)
install.packages("https://cran.r-project.org/src/contrib/Archive/maxLik/maxLik_1.3-4.tar.gz", repos=NULL, dependencies = TRUE)
install.packages("https://cran.r-project.org/src/contrib/Archive/plm/plm_1.6-6.tar.gz", repos=NULL, dependencies = TRUE)

# Instalando rpart.plot
install.packages("https://cran.r-project.org/src/contrib/Archive/rpart.plot/rpart.plot_2.2.0.tar.gz", repos=NULL, dependencies = TRUE)

# Instalando ResourceSelection
install.packages("https://cran.r-project.org/src/contrib/Archive/pbapply/pbapply_1.3-4.tar.gz", repos=NULL, dependencies = TRUE)
install.packages("https://cran.r-project.org/src/contrib/Archive/data.table/data.table_1.11.2.tar.gz", repos=NULL, dependencies = TRUE)
install.packages("https://cran.r-project.org/src/contrib/Archive/prediction/prediction_0.3.2.tar.gz", repos=NULL, dependencies = TRUE)
install.packages("https://cran.r-project.org/src/contrib/Archive/margins/margins_0.3.0.tar.gz", repos=NULL, dependencies = TRUE)
install.packages("https://cran.r-project.org/src/contrib/Archive/ResourceSelection/ResourceSelection_0.3-2.tar.gz", repos=NULL, dependencies = TRUE)

# Instalando VGAM
install.packages("https://cran.r-project.org/src/contrib/Archive/VGAM/VGAM_1.0-5.tar.gz", repos=NULL, dependencies = TRUE)

# Instalando tm
install.packages("https://cran.r-project.org/src/contrib/Archive/xml2/xml2_1.2.0.tar.gz", repos=NULL, dependencies = TRUE)
install.packages("https://cran.r-project.org/src/contrib/Archive/tm/tm_0.7-3.tar.gz", repos=NULL, dependencies = TRUE)

# Instalando SnowballC
install.packages("https://cran.r-project.org/src/contrib/Archive/SnowballC/SnowballC_0.5.tar.gz", repos=NULL, dependencies = TRUE)


# Instalando wordcloud
install.packages("https://cran.r-project.org/src/contrib/Archive/RColorBrewer/RColorBrewer_1.1-2.tar.gz", repos=NULL, dependencies = TRUE)
install.packages("https://cran.r-project.org/src/contrib/Archive/wordcloud/wordcloud_2.5.tar.gz", repos=NULL, dependencies = TRUE)


library(SnowballC)
library(wordcloud)
library(RColorBrewer)