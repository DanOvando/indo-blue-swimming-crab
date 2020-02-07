# Preparation for Indonesia BSC Workshop ----------------------------------
# This script will install required R packages for the upcoming workshop that 
# are not already installed on your computer

# You will need an internet connection for this to work. Some of these may take a bit of
# time to install. Say "yes" to any prompts from R

required_packages <- c("tidyverse","here","lme4", "LBSPR")

missign_packages = required_packages[!required_packages %in% rownames(installed.packages())]

# install any missing packages
install.packages(missign_packages)