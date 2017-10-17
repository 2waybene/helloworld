setwd("/Users/li11/myGit/helloworld/IB_group_meeting/RmdProj1")
suppressWarnings(require(pander))
panderOptions('table.split.table', Inf)
set.caption("Genotype case I")
my.data <- "
Disease-level |   AA  |   AB    |   BB   
Severe        | SNPCnt| SNPCnt  | SNPCnt  
Mild          | SNPCnt| SNPCnt  | SNPCnt 
"
df <- read.delim(textConnection(my.data),header=FALSE,sep="|",strip.white=TRUE,stringsAsFactors=FALSE)
names(df) <- unname(as.list(df[1,])) # put headers on
df <- df[-1,] # remove first row
row.names(df)<-NULL
pander(df, style = 'rmarkdown')

panderOptions('table.split.table', Inf)
set.caption("Genotype case II")
my.data <- "
Disease-level |   AA  |   AB/BB    
Severe        | SNPCnt| SNPCnt    
Mild          | SNPCnt| SNPCnt   
"
df <- read.delim(textConnection(my.data),header=FALSE,sep="|",strip.white=TRUE,stringsAsFactors=FALSE)
names(df) <- unname(as.list(df[1,])) # put headers on
df <- df[-1,] # remove first row
row.names(df)<-NULL
pander(df, style = 'rmarkdown')

panderOptions('table.split.table', Inf)
set.caption("Genotype case III")
my.data <- "
Disease-level |   AA/AB |   BB       
Severe        | SNPCnt  | SNPCnt    
Mild          | SNPCnt  | SNPCnt   
"
df <- read.delim(textConnection(my.data),header=FALSE,sep="|",strip.white=TRUE,stringsAsFactors=FALSE)
names(df) <- unname(as.list(df[1,])) # put headers on
df <- df[-1,] # remove first row
row.names(df)<-NULL
pander(df, style = 'rmarkdown')

#suppressWarnings(library(HardyWeinberg))
#suppressWarnings(library(XLConnect))
source("helperScripts/analysisFuncs.R")


##  Phenotype data
dt <- read.table("data/RSV_cohort_data_ASCII.txt",header = TRUE, sep= "\t")

##  ONLY use for RSV positive candidates
row2use <- which (dt[,5] == 1)

##  This is for reading Excel file
##  We need to set memeory at 24 GB
options(java.parameters = "-Xmx24g" )

snpFiles <- "data/ASCII_genotype_data_GoldenGate_for_JYL_07122015.xlsx"

##  There are five sheets of data
sheets <- c (seq (1,5))


sig.asso.SAT     <- c (list())
results.logit <- logisticAnalysis (sig.asso.SAT, snpFiles, sheets, dt, 6, row2use)
results.logit

 
sig.asso.SAT     <- c (list())
results.logit <- logisticAnalysisWCorrV (sig.asso.SAT, snpFiles, sheets, dt, 6, row2use, params$genStatus)
results.logit


sig.asso.SAT     <- c (list())
results.logit <- logisticAnalysisWCorrV (sig.asso.SAT, snpFiles, sheets, dt, 6, row2use, params$regStatus)
results.logit

sig.asso.SAT     <- c (list())
results.logit <- logisticAnalysisWCorrV (sig.asso.SAT, snpFiles, sheets, dt, 6, row2use, params$sesStatus)
results.logit

sig.asso.SAT     <- c (list())
results.logit <- logisticAnalysisWCorrV (sig.asso.SAT, snpFiles, sheets, dt, 6, row2use, params$brfStatus)
results.logit

sig.asso.SAT     <- c (list())
results.chisqure <- AssoAnalysis     (sig.asso.SAT, snpFiles, sheets, dt, 6, row2use)
results.chisqure
