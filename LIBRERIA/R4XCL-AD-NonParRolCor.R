# repositorio_cran <- "https://packagemanager.rstudio.com/cran/2022-05-06"
# options(repos = c(CRAN = repositorio_cran))
# 
# install.packages("gtools")
# install.packages("pracma")
# install.packages("doParallel")
# 
# options(repos = c(CRAN = "https://cloud.r-project.org/"))
# 
# ruta_paquete<-"M:/R4XCL/PROCESO DOCUMENTAL/VERSION 2025/PONENCIAS/NonParRolCor_0.8.0.tar.gz"
# 
# install.packages(
#    ruta_paquete, 
#    repos = NULL, 
#    type = "source",
#    verbose = FALSE,  # Suprimir mensajes detallados
#    quiet = TRUE      # Mostrar advertencias y errores
# )

AD_NonParRolCor <- function(
      SetDatosX,
      MCSim,
      Np,
      Widthwin,
      prob
)
{
   library(NonParRolCor)
   
   BERT.graphics.device(cell = T)

   Procedimientos <- R4XCL_INT_PROCEDIMIENTOS()
    
   DT <- R4XCL_INT_DATOS(
      SetDatosY=NULL,
      SetDatosX=SetDatosX,
      Escala=NULL,
      Filtro=NULL,
      Categorica=0,
      Ponderadores = NULL
   )
   
   X_Y <- rolcor_estim_1win(
                as.matrix(DT),
                CorMethod="pearson", 
                widthwin=Widthwin, 
                Align="center",
                rmltrd=TRUE, 
                Scale=TRUE,
                MCSim=MCSim, 
                Np=Np, 
                prob=prob
               )

   plot_rolcor_estim_1win(
      as.matrix(DT),
      corcoefs= X_Y$Correlation_coefficients,
      CRITVAL=X_Y$CRITVAL, 
      widthwin=Widthwin,
      left_win=X_Y$left_win, 
      righ_win=X_Y$righ_win
   )

   dev.off();
   
   return("Ver resultado")
}

AD_plot_rolcor_estim_heatmap <- function(SetDatosX,
                                         MCSim,
                                         Np,
                                         Widthwin1,
                                         WidthwinN)
{
   library(NonParRolCor)

   TYPEWIDTHWIN="PARTIAL"
   
   # Number of Monte-Carlo simulations (MCSim), please use at least 1000.
   # WARNING: MCSim=2, it's just to test this example!

   BERT.graphics.device(cell = T)
   
   Procedimientos <- R4XCL_INT_PROCEDIMIENTOS()

   DT <- R4XCL_INT_DATOS(SetDatosX=SetDatosX)
   #DT <- syntheticdata[1:350,]
   
   X_Y <- rolcor_estim_heatmap(
                            DT, #syntheticdata[1:350,], 
                            CorMethod="pearson",
                            typewidthwin=TYPEWIDTHWIN, 
                            widthwin_1=Widthwin1,
                            widthwin_N=WidthwinN, 
                            Align="center", 
                            rmltrd=TRUE,
                            Scale=TRUE, 
                            MCSim=MCSim, 
                            Np=Np)
   
   plot_rolcor_estim_heatmap(
                             DT,#syntheticdata[1:350,], 
                             X_Y$matcor, 
                             X_Y$CRITVAL,
                             Rwidthwin=X_Y$Windows, 
                             typewidthwin=TYPEWIDTHWIN,
                             widthwin_1=Widthwin1, 
                             widthwin_N=WidthwinN)

   #dev.off()

   
   return("Ver resultado")   

}

AD_prueba = function()
{
   
   library(NonParRolCor)
   MCSim <- 2
   Np <- 2
   TYPEWIDTHWIN="PARTIAL"
   # Number of Monte-Carlo simulations (MCSim), please use at least 1000.
   # WARNING: MCSim=2, it's just to test this example!
   X_Y <- rolcor_estim_heatmap(syntheticdata[1:350,], CorMethod="pearson",
                               typewidthwin=TYPEWIDTHWIN, widthwin_1=29,
                               widthwin_N=51, Align="center", rmltrd=TRUE,
                               Scale=TRUE, MCSim=MCSim, Np=Np)
   plot_rolcor_estim_heatmap(syntheticdata[1:350,], X_Y$matcor, X_Y$CRITVAL,
                             Rwidthwin=X_Y$Windows, typewidthwin=TYPEWIDTHWIN,
                             widthwin_1=29, widthwin_N=51)
}