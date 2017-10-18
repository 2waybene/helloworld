
library(rmarkdown)
library(knitr)


##  column7: breastfeeding status
render("/Users/li11/myGit/helloworld/IB_group_meeting/RmdProj1/proj1_in_pdf.rmd", 
       params = list( brfStatus = 7, 
                      sesStatus = 4, 
                      genStatus = 2, 
                      regStatus = 3), 
       output_file = "/Users/li11/myGit/helloworld/IB_group_meeting/RSV_GoldenGate.pdf")

