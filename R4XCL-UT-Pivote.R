DB_Unicos <- function(SetDatosX)
{
  a=data.frame(unique(SetDatosX))
  return(a)
  
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# UNE DOS DFs EMPLEANDO MERGE
# INNER OUTER LEFT RIGHT
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

DB_Union <- function(
                     SetDatosX,
                     SetDatosY, 
                     TipoOutput=0
                     )
{

  library(svDialogs)
  
  list_Opcion <- c("[1] Inner join",
                   "[2] Outer join",
                   "[3] Cross join",
                   "[4] Left outer",
                   "[5] Right outer")
  
  pX          <- ncol(SetDatosX)
  pY          <- ncol(SetDatosY)
  
  nombresX    <- as.character(SetDatosX[1, 1:pX])
  nombresY    <- as.character(SetDatosY[1, 1:pY])
  
  SetDatosX   <- data.frame(SetDatosX[-1, ])
  SetDatosY   <- data.frame(SetDatosY[-1, ])
  
  colnames(SetDatosX) <- nombresX
  colnames(SetDatosY) <- nombresY

  Llaves_X <- dlg_list(
                       choices = nombresX,
                       multiple = TRUE,
                       title = "Seleccione la (s) llave (s) de tabla A"
                       )$res
               
  Llaves_Y <- dlg_list(
                       choices = nombresY,
                       multiple = TRUE,
                       title = "Seleccione la (s) llave (s) de tabla B"
                       )$res
  
  SetDatosX[[Llaves_X]] <- as.character(unlist(SetDatosX[[Llaves_X]]))
  SetDatosY[[Llaves_Y]] <- as.character(unlist(SetDatosY[[Llaves_Y]]))
  
    if(TipoOutput == 0){
      
      SetUnido = data.frame(Opciones = list_Opcion)
  
  }else if(TipoOutput == 1){   
    
      SetUnido <- merge(
                        x    = SetDatosX,   y = SetDatosY, 
                        by.x = Llaves_X, by.y = Llaves_Y,
                        all  = FALSE)
  
  }else if(TipoOutput == 2){
      print("entra")
    
      SetUnido <- merge(
                        x     = SetDatosX,   y = SetDatosY, 
                        by.x  = Llaves_X, by.y = Llaves_Y,
                        all   = TRUE)
    
  }else if(TipoOutput == 3){
    
      SetUnido <- merge(
                        x     = SetDatosX,   y = SetDatosY, 
                        by.x  = NULL,     by.y = NULL)
  
  }else if(TipoOutput == 4){
    
      SetUnido <- merge(
                        x     = SetDatosX,   y = SetDatosY, 
                        by.x  = Llaves_X, by.y = Llaves_Y,
                        all.x = TRUE)
  
  }else if(TipoOutput == 5){
    
      SetUnido <- merge(
                        x     = SetDatosX,   y = SetDatosY, 
                        by.x  = Llaves_X, by.y = Llaves_Y,
                        all.y = TRUE)}
  
  OutPut <- as.matrix(SetUnido)

}  

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# CREA TABLA AGRUPADA
# DEBO CORREGIR QUE LOS DATOS DEBAN ENTRAR EN ORDEN
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

DB_Pivote <- function(
                      SetDatosX,
                      SetDatosY, 
                      Filtro=0,
                      TipoOutput=1
                      )
{
  library(svDialogs)
  
  pX          <- ncol(SetDatosX)
  pY          <- ncol(SetDatosY)
  nX          <- nrow(SetDatosX)
  k           <- (pX+1)
  
  nombresX    <- as.character(SetDatosX[1, 1:pX])
  nombresY    <- as.character(SetDatosY[1, 1:pY])
  
  SetFiltrado <-R4XCL_INT_FILTRAR(
                                  SetDatosX=SetDatosX,
                                  Filtro = Filtro,
                                  pX=pX,nX=nX
                                 )
  
  SetFiltrado=SetFiltrado[,-k]

  colnames(SetFiltrado) <- nombresX
  columnas_agregacion   <- setdiff(nombresX, nombresY)
  posicion_agrupacion   <- which(nombresX==nombresY)

  SetFiltrado <- data.frame(SetFiltrado[-1, ])

  for (col_name in columnas_agregacion) 
  {
  
  SetFiltrado[[col_name]] <- suppressWarnings(as.numeric(
                             as.character(SetFiltrado[[col_name]])))
  }

  for (i in posicion_agrupacion) 
  {
    SetFiltrado[[i]] <- as.factor(as.character(SetFiltrado[[i]]))
  }

  VarsX=colnames(SetFiltrado[, -posicion_agrupacion])
  VarsY=colnames(SetFiltrado[,  posicion_agrupacion])

  A=paste(VarsX,collapse = " , ")
  B=paste(VarsY,collapse = " + ")
  C=paste0("cbind(",A,")~",B)  
  
  especificacion=eval(parse(text=C))
  
  if (TipoOutput == 0){
    
    OutPut <- "EN PROCESO"
    
  } else if (TipoOutput == 1){  
    
    a <- aggregate(
                  formula = especificacion,
                  data    = data.frame(SetFiltrado),
                  FUN     = sum
                  )
    OutPut <- a
    
  }else if(TipoOutput == 2){
    
    a <- aggregate(
                  formula = especificacion,
                  data    = data.frame(SetFiltrado),
                  FUN     = mean
                  )
    OutPut <- a
    
  }else if(TipoOutput == 3){ 
    
    a <- aggregate(
                  formula = especificacion,
                  data    = data.frame(SetFiltrado),
                  FUN     = median
                  )
    OutPut <- a
    
  }else if(TipoOutput == 4){
    
    a <- aggregate(
                  formula = especificacion,
                  data    = data.frame(SetFiltrado),
                  FUN     = length 
                  )
    OutPut=a
  
  }else if(TipoOutput == 5){
    

    skew <- function(x, na.rm = FALSE)
          {
            if(na.rm) x <- x[!is.na(x)]
            n <- length(x)
            sum((x - mean(x))^3)/(n - 2)/var(x)^(3/2)
          }
    
    a <- aggregate(
                  formula = especificacion,
                  data    = data.frame(SetFiltrado),
                  FUN     = skew 
                  )
    OutPut=a  
    
  }else if(TipoOutput == 6){
    
    momento_k    <- function(x, k) 
    {
      n          <- length(x)
      mu         <- mean(x)
      sum_desv_k <- sum((x - mu)^k)
      
      return(sum_desv_k / n)
    }
    
    JarqueBera   <- function(x)     
    {  
      n          <- length(x)
      mu2        <- momento_k(x, 2)
      mu3        <- momento_k(x, 3)
      mu4        <- momento_k(x, 4)
          
      S          <- mu3 / (mu2^(3/2))
      K          <- mu4 / (mu2^2)
      JB         <- (n / 6) * (S^2 + (K - 3)^2 / 4)
      pvalue     <- 1 - pchisq(JB, 2)
      
      return(pvalue)
    }

    a <- aggregate(
                  formula = especificacion,
                  data    = data.frame(SetFiltrado),
                  FUN     = JarqueBera
                  )
    OutPut=a  
        
  }else if(TipoOutput == 9){  
    
    Texto <- "Ingrese el valor porcentual umbral (Valor 0-100): "
    Prob  <- as.numeric(dlgInput(Texto, default =50)$res)/100

    a <- aggregate(
                  formula = especificacion,
                  data    = data.frame(SetFiltrado),
                  FUN     = function(x) quantile(x, prob=Prob, na.rm=TRUE)
                  )
    
    Comentario   <- paste0("--- Umbral: ",Prob*100," ---")
    pFil         <- nrow(a)+1
    pCol         <- ncol(SetDatosY)+1

    a[pFil,1:ncol(a)] <- ""
    a[pFil,pCol]      <- Comentario
    
    OutPut= a
    
  }else if(TipoOutput > 9){  
    
    OutPut <- "Revisar parámetros disponibles" 
    
  }  
  
  #-------------------------->>> 
  # [4] RESULTADO FINAL
  #-------------------------->>> 
  
  return(OutPut);  
}


DialogosXCL <- R4XCL_INT_DIALOGOS()

attr(DB_Pivote, DialogosXCL$Descripcion) = 
  
  list(
        SetDatosY       = DialogosXCL$SetDatosY,
        SetDatosX       = DialogosXCL$SetDatosX,
        Filtro          = DialogosXCL$Filtro,
        TipoOutput      = "{1:Suma, 2:Media, 3:Mediana, 4:Conteo, 5:Percentil} "
      )

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# FIN DE PROCEDIMIENTO                                 +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++