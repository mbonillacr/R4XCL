TBL_UNION = function(TABLA1, TABLA2)
{
  
  #-------------------------->>>
  # VALIDACIONES
  #-------------------------->>>
  
  if (missing(TABLA1)) {
    stop("Error: TABLA1 es un parámetro obligatorio.")
  }
  if (missing(TABLA2)) {
    stop("Error: TABLA2 es un parámetro obligatorio.")
  }
  if (!is.data.frame(TABLA1)) {
    stop("Error: TABLA1 debe ser un data frame.")
  }
  if (!is.data.frame(TABLA2)) {
    stop("Error: TABLA2 debe ser un data frame.")
  }
  
  #-------------------------->>>
  # PREPARACION DE DATOS Y PARAMETROS
  #-------------------------->>>
  
  TABLA.1=R4XCL_INT_DATOS_TEXTO(TABLA1)
  TABLA.2=R4XCL_INT_DATOS_TEXTO(TABLA2)
    
  pVars1=TABLA1[1,]
  pVars2=TABLA2[1,]
  
  library(svDialogs)
  
  TipoUnion.Disponible = c(
                          "[INNER Join] Que estén en AMBAS tablas",
                          "[LEFT  Join] Que estén SÓLO en tabla 1",
                          "[RIGHT Join] Que estén SÓLO en tabla 2",
                          "[OUTER Join] ",
                          "[CROSS Join] Combinación de AMBAS tablas"
                          )
  
  Instrucciones = c(
                    "Variable de unión TABLA",
                    "Tipo de Unión que desea realizar"
                    )
  
  A = dlg_list(
               TipoUnion.Disponible, 
               multiple = FALSE, 
               title = paste0(Instrucciones[2]," ")
               )
  
  TipoUnion.Seleccionada=A$res
  
  A = dlg_list(
               pVars1, 
               multiple = TRUE, 
               title = paste0(Instrucciones[1]," 1")
               )
  ID1=A$res
  
  A = dlg_list(
               pVars2, 
               multiple = TRUE, 
               title = paste0(Instrucciones[1]," 2")
               )
  ID2=A$res
  
  #-------------------------->>>
  # UNIR TABLAS
  #-------------------------->>>
  
  if (TipoUnion.Seleccionada == TipoUnion.Disponible[1]){
    
  # Que esté en ambas tablas
      
      OutPut = merge(
                     x = TABLA.1,
                     y = TABLA.2,
                     by.x = ID1,
                     by.y = ID2
                     )
                      
  } else if  (TipoUnion.Seleccionada ==TipoUnion.Disponible[2]){
  
  # Que esté en sólo tabla 1: LEFT JOIN
  
    OutPut = merge(x=TABLA.1, 
                   y=TABLA.2, 
                   by.x = ID1,
                   by.y = ID2,
                   all.x = TRUE)
  
  } else if  (TipoUnion.Seleccionada ==TipoUnion.Disponible[3]){
    
  # Que esté en sólo tabla 2: RIGTH JOIN
  
    OutPut = merge(x=TABLA.1, 
                   y=TABLA.2, 
                   by.x = ID1,
                   by.y = ID2,
                   all.y = TRUE)
  } else if  (TipoUnion.Seleccionada ==TipoUnion.Disponible[4]){
    
  #  OUTER JOIN
    
    OutPut = merge(x=TABLA.1, 
                   y=TABLA.2, 
                   by.x = ID1,
                   by.y = ID2,
                   all.y = TRUE,
                   all.x = TRUE)
  } else if  (TipoUnion.Seleccionada ==TipoUnion.Disponible[5]){
    
  # Producto cartesiano : CROSS JOIN
    
    OutPut = merge(x=TABLA.1, 
                   y=TABLA.2, 
                   all.y = TRUE,
                   all.x = TRUE)
  } 
  
  #-------------------------->>>
  # RESULTADO FINAL
  #-------------------------->>>
  
    if (nrow(OutPut)==0){OutPut=t(colnames(OutPut))}
    
  return(OutPut)

}