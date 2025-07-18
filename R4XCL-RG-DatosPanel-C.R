MR_PanelData.C <- function(
    SetDatosY, 
    SetDatosX,
    Variable_i,
    Variable_t,
    Filtro=NULL,
    TipoOutput=1
)
{
  
  #-------------------------->>>
  # PREPARACION DE DATOS Y PARAMETROS
  #-------------------------->>>
  
  require(plm)
  require(stargazer)
  require(svDialogs)
  
  # ---------------------
  #[0] NOTAS MEMO T?CNICAS
  # ---------------------
  
  Nota001 <- "R4XCL"
  
  #-------------------------->>>   
  # [1] PREPARACION DE DATOS Y PARAMETROS  
  #-------------------------->>>  
  
  Constante <- 1
  Escala <- 0
  
  FX <- R4XCL_INT_FUNCION(SetDatosX,SetDatosY)
  
  Procedimientos <- R4XCL_INT_PROCEDIMIENTOS()
  
  especificacion <- eval(parse(text=FX))
  
  if (Constante==0){FX=paste0(FX,"-1")}
  
  DT <- R4XCL_INT_DATOS(
                        SetDatosY=SetDatosY,                   
                        SetDatosX=SetDatosX,
                        Escala=Escala,
                        Filtro=Filtro
                      )
  
  Variable_i <- Variable_i[2:nrow(Variable_i),1]
  Variable_i <- Variable_i[Filtro[2:nrow(Filtro)]==0]
  Variable_t <- as.integer(Variable_t[2:nrow(Variable_t),1])
  Variable_t <- Variable_t[Filtro[2:nrow(Filtro)]==0]
  
  DT <- cbind(
    DT,
    "i"= Variable_i,
    "t"= Variable_t
  )
  
  #-------------------------->>> 
  # [2] PROCEDIMIENTO ANALITICO
  #-------------------------->>> 
  
  OLS  <- lm(
            formula = especificacion, 
            data = DT
          )
  
  pooling <- plm(
                formula = especificacion,
                data = DT,
                index=c("i", "t"),
                model="pooling"
              )
  
  within = plm(
    formula = especificacion,
    data = DT,
    index=c("i", "t"),
    model="within"
  )
  
  between <- plm(
                formula = especificacion,
                data = DT,
                index=c("i", "t"),
                model="between"
              )
  
  random <- plm(
                formula = especificacion,
                data = DT,
                index=c("i", "t"),
                model="random"
              )
  
  FX.t <- paste0(FX, "+ factor(t)-1")
  especificacion <- eval(parse(text=FX.t))
  
  Pool.Fixed.t<-plm(
                    especificacion,
                    data=DT,
                    index=c("i", "t"),
                    model="pooling"
                  )
  
  #-------------------------->>> 
  # [3] PREPARACION DE RESULTADOS
  #-------------------------->>> 
  
  if (TipoOutput <= 0){
    
    OutPut <- Procedimientos$PANEL
    
  } else if (TipoOutput == 1){  
    
    OutPut <- stargazer(
                      OLS, pooling, within, between, random, Pool.Fixed.t,
                      column.labels=c("OLS", "pooling","within","between","random", "Pooled Fixed (t)"),
                      type="text",
                      ci=TRUE, ci.level=0.95,single.row=FALSE,
                      align=TRUE,
                      notes = Nota001,
                      notes.append = TRUE
                    )
    
  } else if (TipoOutput == 2){
    
    OutPut <- capture.output(fixef(Within))
    
  } else if (TipoOutput == 3){
    
    OutPut <- capture.output(pFtest(Within, OLS))
    
  } else if (TipoOutput == 4){
    
    OutPut <- capture.output(phtest(Within, Random))
    
  } else if (TipoOutput == 5){
    
    OutPut <- capture.output(pFtest(Fixed.t, Within))
    OutPut <- capture.output(fixef(Within))
    
  } else if (TipoOutput == 6){
    
    OutPut <- capture.output(plmtest(Pool, c("time"), type=("bp")))
    
  }else if(TipoOutput == 7){
    
    OutPut <- capture.output(plmtest(Pool, c("individual"), type=("bp")))
    
  } else if (TipoOutput == 8){
    
    OutPut <- capture.output(plmtest(Pool, c("twoways"), type=("bp")))
    
  } else if (TipoOutput == 9){
    
    OutPut <- capture.output(plmtest(Pool, type=c("bp")))
    
  } else if (TipoOutput == 10){
    
    OutPut <- capture.output(pcdtest(Within, test = c("lm")))
    
  }else if(TipoOutput == 11){
    
    OutPut <- capture.output(pcdtest(Within, test = c("cd")))
    
  } else if (TipoOutput == 12){
    
    OutPut <- capture.output(pbgtest(Within))
    
  }else if(TipoOutput == 13){
    
    library(tseries)
    Panel.set <- plm.data(DT, index = c("i", "t"))
    OutPut    <- capture.output(adf.test(Panel.set$y, k=2))
    
  } else if (TipoOutput == 14){
    
    OutPut  <- R4XCL_INT_INFO_EJECUCION(FX, DT)
    
  } else if (TipoOutput == 15){
    
    ListaM <- c(OLS,pooling,within,between,random)
    ListaN <- c("OLS","POOL","WITHIN","BETWEEN","RANDOM")
    OutPut <- R4XCL_INT_CREARDS(ListaM,ListaN)

  }else if(TipoOutput > 15){   
    
    OutPut <- "Revisar par?metros disponibles" 
    
  }
  
  #-------------------------->>> 
  # [4] RESULTADO FINAL
  #-------------------------->>> 
  
  return(OutPut) 
  
}

DialogosXCL=R4XCL_INT_DIALOGOS()

attr(MR_PanelData.C, DialogosXCL$Descripcion) = 
  list(
    Detalle    = DialogosXCL$Detalle.Panel,
    SetDatosY  = DialogosXCL$SetDatosY,
    SetDatosX  = DialogosXCL$SetDatosX,
    Variable_i = DialogosXCL$Variable_i,
    Variable_t = DialogosXCL$Variable_t,
    Filtro     = DialogosXCL$Filtro,
    TipoOutput = DialogosXCL$TipoOutput
  )

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++