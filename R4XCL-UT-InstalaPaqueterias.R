
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++    
# Instala Paquetes Necesarios para R4XCL               +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

UT_InstalaPaquetes = function(PQT=NULL)
{  
  #RepoMsft= 'https://cran.microsoft.com/snapshot/2019-04-15/'
  #OBTENER DE: https://cran.microsoft.com/snapshot/

  RepoMsft= 'https://cran.microsoft.com/snapshot/2021-11-05/'
  
  RepoMsft='http://cran.us.r-project.org'
  
  if (is.null(PQT)){
  
                  InstalarPaquetes = R4XCL_INT_POR_INSTALAR()
                   
                  
  } else {InstalarPaquetes = c(PQT)}
  
   install.packages(InstalarPaquetes, dependencies = TRUE, repos=RepoMsft)

   return(InstalarPaquetes)
   
}
