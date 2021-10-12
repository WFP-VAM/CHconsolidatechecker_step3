#install these packages in line below if they are not already
#install.packages(c("tidyverse", "openxlsx","DT","shinyBS")

#this will run shiny app from github repository https://github.com/WFP-VAM/CHconsolidatechecker_step3 on local machine
library(shiny)

runUrl('https://github.com/WFP-VAM/CHconsolidatechecker_step3/archive/main.tar.gz')
