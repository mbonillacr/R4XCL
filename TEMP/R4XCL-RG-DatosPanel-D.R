MR_PanelData.D <- function(
                        SetDatosY, 
                        SetDatosX,
                        Variable_i,
                        Variable_t,
                        Filtro=NULL
                       )
{
  
  #-------------------------->>>
  # VALIDACIONES
  #-------------------------->>>
  if (missing(SetDatosX)) {
    stop("Error: SetDatosX es un parámetro obligatorio.")
  }
  if (missing(SetDatosY)) {
    stop("Error: SetDatosY es un parámetro obligatorio.")
  }
  if (missing(Variable_i)) {
    stop("Error: Variable_i es un parámetro obligatorio.")
  }
  if (missing(Variable_t)) {
    stop("Error: Variable_t es un parámetro obligatorio.")
  }
  if (!is.data.frame(SetDatosX)) {
    stop("Error: SetDatosX debe ser un data frame.")
  }
  if (!is.data.frame(SetDatosY)) {
    stop("Error: SetDatosY debe ser un data frame.")
  }
  if (!is.vector(Variable_i)) {
    stop("Error: Variable_i debe ser un vector.")
  }
  if (!is.vector(Variable_t)) {
    stop("Error: Variable_t debe ser un vector.")
  }
  
  #-------------------------->>>
  # PREPARACION DE DATOS Y PARAMETROS
  #-------------------------->>>
  
  require(plm)
  require(stargazer)
  require(svDialogs)
  
  # ---------------------
  #[0] NOTAS MEMO TECNICAS
  # ---------------------

  Nota001 <- "R4XCL"
  
  #-------------------------->>>   
  # [1] PREPARACION DE DATOS Y PARAMETROS  
  #-------------------------->>>  

  Constante <-1
  Escala    <-0

  FX  <- R4XCL_INT_FUNCION(SetDatosX,SetDatosY)

  Procedimientos <-R4XCL_INT_PROCEDIMIENTOS()
  
  especificacion  <- eval(parse(text=FX))

  if (Constante==0){FX=paste0(FX,"-1")}

  DT  <- R4XCL_INT_DATOS(
                       SetDatosY=SetDatosY,                   
                       SetDatosX=SetDatosX,
                       Escala=Escala,
                       Filtro=Filtro
                       )

  Variable_i  <- Variable_i[2:nrow(Variable_i),1]
  Variable_i  <- Variable_i[Filtro[2:nrow(Filtro)]==0]
  Variable_t  <- as.integer(Variable_t[2:nrow(Variable_t),1])
  Variable_t  <- Variable_t[Filtro[2:nrow(Filtro)]==0]
                        
  DT <-cbind(
           DT,
           "i"= Variable_i,
           "t"= Variable_t
           )

  #-------------------------->>> 
  # [2] PROCEDIMIENTO ANALITICO
  #-------------------------->>> 
  
  OLS   <- lm(
            formula = especificacion, 
            data = DT
            )
  
  pooling  <- plm(
             formula = especificacion,
             data = DT,
             index=c("i", "t"),
             model="pooling"
             )

  within  <- plm(
               formula = especificacion,
               data = DT,
               index=c("i", "t"),
               model="within"
               )

  between  <- plm(
                formula = especificacion,
                data = DT,
                index=c("i", "t"),
                model="between"
                )

  random  <- plm(
               formula = especificacion,
               data = DT,
               index=c("i", "t"),
               model="random"
               )

  FX.t <- paste0(FX, "+ factor(t)-1")
  especificacion <- eval(parse(text=FX.t))

  Pool.Fixed.t <- plm(
                      especificacion,
                      data=DT,
                      index=c("i", "t"),
                      model="pooling"
                      )

  #-------------------------->>> 
  # [3] PREPARACION DE RESULTADOS
  #-------------------------->>> 

  A  <- dlg_list(Procedimientos$PANEL)
  TipoOutput <-A$res
  
  if (TipoOutput == Procedimientos$PANEL[1]){

    OutPut <-stargazer(
                     OLS, pooling, within, between, random, Pool.Fixed.t,
                     column.labels=c("OLS", "pooling","within","between","random", "Pooled Fixed (t)"),
                     type="text",
                     ci=TRUE, ci.level=0.95,single.row=FALSE,
                     align=TRUE,
                     notes = Nota001,
                     notes.append = TRUE
                     )

  } else if (TipoOutput == Procedimientos$PANEL[2]){
    
    OutPut  <- capture.output(fixef(Within))

  } else if (TipoOutput == Procedimientos$PANEL[3]){

    OutPut  <- capture.output(pFtest(Within, OLS))

  } else if (TipoOutput == Procedimientos$PANEL[4]){

    OutPut  <- capture.output(phtest(Within, Random))

  } else if (TipoOutput == Procedimientos$PANEL[5]){

    OutPut  <- capture.output(pFtest(Fixed.t, Within))
    OutPut  <- capture.output(fixef(Within))

  } else if (TipoOutput == Procedimientos$PANEL[6]){

    OutPut  <- capture.output(plmtest(Pool, c("time"), type=("bp")))

  } else if (TipoOutput == Procedimientos$PANEL[7]){

    OutPut  <- capture.output(plmtest(Pool, c("individual"), type=("bp")))

  } else if (TipoOutput == Procedimientos$PANEL[8]){

    OutPut  <- capture.output(plmtest(Pool, c("twoways"), type=("bp")))

  }else if(TipoOutput == Procedimientos$PANEL[9]){

    OutPut  <- capture.output(plmtest(Pool, type=c("bp")))

  } else if (TipoOutput == Procedimientos$PANEL[10]){

    OutPut  <- capture.output(pcdtest(Within, test = c("lm")))

  } else if (TipoOutput == Procedimientos$PANEL[11]){

    OutPut  <- capture.output(pcdtest(Within, test = c("cd")))

  } else if (TipoOutput == Procedimientos$PANEL[12]){

    OutPut  <- capture.output(pbgtest(Within))

  } else if (TipoOutput == Procedimientos$PANEL[13]){

    library(tseries)
    Panel.set <- plm.data(DT, index = c("i", "t"))
    OutPut  <- capture.output(adf.test(Panel.set$y, k=2))

  } else if (TipoOutput == ListaFunciones$PANEL[14]){
    
    OutPut   <- R4XCL_INT_INFO_EJECUCION(FX, DT)
    
  } else if (TipoOutput == ListaFunciones$PANEL[15]){

    ListaM  <- c(OLS,pooling,within,between,random)
    ListaN  <- c("OLS","POOL","WITHIN","BETWEEN","RANDOM")

    OutPut  <- R4XCL_INT_CREARDS(ListaM, ListaN)
  }
  
  #-------------------------->>> 
  # [4] RESULTADO FINAL
  #-------------------------->>> 

  return(OutPut)  
  
}

DialogosXCL=R4XCL_INT_DIALOGOS()

attr(MR_PanelData.D, DialogosXCL$Descripcion) = 
  list(
       Detalle    = DialogosXCL$Detalle.Panel,
       SetDatosY  = DialogosXCL$SetDatosY,
       SetDatosX  = DialogosXCL$SetDatosX,
       Variable_i = DialogosXCL$Variable_i,
       Variable_t = DialogosXCL$Variable_t,
       Filtro     = DialogosXCL$Filtro
      )

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++