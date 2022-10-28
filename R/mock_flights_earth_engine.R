#Load packages
  library(rgee)
  library(targets)


#install rgee python dependencies
  rgee::ee_install()
  rgee::ee_install_upgrade()
  reticulate::py_install('earthengine-api==0.1.323', envname='rgee',ignore_installed = TRUE)
  reticulate::py_install('earthengine-api==0.1.323')
  reticulate::py_
  rgee::ee_check()
  
  ?py_install
  #Initialize rgee
  ee_Initialize(drive = TRUE)
  
