#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# INSTALAR PAQUETES
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

UT_InstalaPaquetes = function(PQT=NULL)
{  
  
  #-------------------------->>>
  # VALIDACIONES
  #-------------------------->>>
  
  if (!is.null(PQT) &&!is.character(PQT)) {
    stop("Error: PQT debe ser un vector de caracteres.")
  }
  
  #-------------------------->>>
  # PREPARACION DE DATOS Y PARAMETROS
  #-------------------------->>>
  
  RepoMsft='http://cran.us.r-project.org'
  
  if (is.null(PQT)){
  
                  InstalarPaquetes = R4XCL_INT_POR_INSTALAR()
                   
                  
  } else {InstalarPaquetes = c(PQT)}
  
  #-------------------------->>>
  # INSTALAR PAQUETES
  #-------------------------->>>
  
  install.packages(InstalarPaquetes, dependencies = TRUE, repos = RepoMsft)
  
  #-------------------------->>>
  # RESULTADO FINAL
  #-------------------------->>>
  
  return(InstalarPaquetes)
   
}
