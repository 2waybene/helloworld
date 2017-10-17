
library(rmarkdown)
library(knitr)


##  column7: breastfeeding status
render("x:/myGit/helloworld/IB_group_meeting/RmdProj1/proj1_in_pdf.rmd", 
       params = list( brfStatus = 7, 
                      sesStatus = 4, 
                      genStatus = 2, 
                      regStatus = 3), 
       output_file = "x:/myGit/helloworld/IB_group_meeting/analysisResults/RSV_GoldenGate.pdf")

