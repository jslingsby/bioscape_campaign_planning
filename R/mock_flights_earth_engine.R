#Load packages
  library(rgee)
  library(targets)


#install rgee python dependencies
  rgee::ee_clean_pyenv()
  rgee::ee_install()
  rgee::ee_install_upgrade()
  rgee::ee_check()
  
  ?py_install
  #Initialize rgee
  #ee_Initialize(drive = TRUE)
  ee_Initialize()
