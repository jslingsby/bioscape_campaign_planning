#Load packages
  library(rgee)
  library(targets)


#install rgee python dependencies
  rgee::ee_install()
  rgee::ee_install_upgrade()
  rgee::ee_check()


  #Initialize rgee
  ee_Initialize(drive = TRUE)


  #curl -O https://dl.google.com/dl/cloudsdk/channels/rapid/downloads/google-cloud-cli-387.0.0-linux-x86_64.tar.gz
  #tar -xf google-cloud-cli-387.0.0-linux-x86_64.tar.gz
  #./google-cloud-sdk/install.sh
