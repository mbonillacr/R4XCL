#_________________________________________________________________
#|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|
#|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|
#_________________________________________________________________

xcl2xml=function(UBICACION)
  {

  #__________________________________
  #
  # INSTALACION | CARGA DE LIBRERIAS
  #__________________________________
  
    packs=c("readxl","kulife","xml2")
    
    if(length(new.pkgs <- setdiff(packs, rownames(installed.packages())))) install.packages(new.pkgs)
    lapply(packs, require, character.only = TRUE)
    
    setwd(dirname(UBICACION))
    
    DS = read_excel(UBICACION)
    DS = as.data.frame(DS)

#__________________________________
#
# MANEJO NOMBRE OUTPUT
#__________________________________    
     
    NombreIn=basename(UBICACION)
    NombreIn=gsub("[.]", "_", NombreIn)
    
    NombreOut=paste0(NombreIn,"_a.xml")
    
  #__________________________________
  #    
  # CARGA|TRANSFORMA a XML
  #__________________________________  
  
    xml2::write_xml(DS, file = NombreOut, options = "format", encoding = "UTF-8")
    
  #__________________________________
  #  
  # FIN
  #__________________________________       
  
}

#_________________________________________________________________
#|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|
#|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|
#_________________________________________________________________


xml2csv=function(UBICACION)
{

#__________________________________
#
# INSTALACION | CARGA DE LIBRERIAS
#__________________________________
    
  packs=c("xml")
  
  if(length(new.pkgs <- setdiff(packs, rownames(installed.packages())))) install.packages(new.pkgs)
  lapply(packs, require, character.only = TRUE)

#__________________________________
#    
# CARGA|TRANSFORMA XML a R
#__________________________________  

  doc = xmlParse(UBICACION)
  DS  = xmlToDataFrame(nodes = getNodeSet(doc, "//cbc"))

#__________________________________
#  
# MANEJO DE NOMBRE OUTPUT
#__________________________________   
  
  NombreIn=basename(UBICACION)
  PathIn=dirname(UBICACION)
  NombreIn=gsub("[.]", "_", NombreIn)
  
  NombreOut=paste0(PathIn,"/",NombreIn,"_a.csv")
  
  print(NombreOut)
  write.csv(DS, file = NombreOut)

#__________________________________
#  
# FIN
#__________________________________    
  
}

#_________________________________________________________________
#|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|
#|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|_|-|
#_________________________________________________________________

