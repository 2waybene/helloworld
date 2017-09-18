##===================================================
##  prepareData.R
##  Processing snp data and applying TAGSter results
##
##===================================================
setwd("x:/project2017/GeneticAssociationAnalysis/with_TAGSter/workingDir/")
load ("../../workingDir/Genotype_file_223_strains_623054_SNPs.rda")
dim(genoJenData)


##========================================
##  TAGSter SNPs
##========================================

nuSNP.mitoCarta.tagged <- "x:/project2017/GeneticAssociationAnalysis/TAGSter/tagster/outdir/multipop_tags.txt"
nuSNP.full.tagged <- "x:/project2017/GeneticAssociationAnalysis/TAGSter/tagster/outdir/metaRun/multipop_tags.txt"

dt <- read.table(nuSNP.mitoCarta.tagged, header = TRUE, row.names=NULL)
genoJenData.mitoCarta.TAGstered <- genoJenData[which(genoJenData$JAX.SNP.ID %in% as.character(dt$gene)),]
dim(genoJenData.mitoCarta.TAGstered)
save (genoJenData.mitoCarta.TAGstered, file = "genoJenData_mitoCarta_TAGstered_4495_SNPs.rda")

dt <- read.table(nuSNP.full.tagged, header = TRUE, row.names=NULL)
genoJenData.full.TAGstered <- genoJenData[which(genoJenData$JAX.SNP.ID %in% as.character(dt$gene)),]
dim(genoJenData.full.TAGstered)
save (genoJenData.full.TAGstered, file = "genoJenData_full_TAGstered_130351_SNPs.rda")

##========================================
##  Phenotype data from Jen's paper
##  P4 -- air; PE3 -- hyperoxia exposure
##  Total 36 strains
##========================================

dt <- read.csv("x:/project2017/GeneticAssociationAnalysis/phenotypes/phenotyp-from-Jen.csv")
dim(dt)
phenotypeFromJen <- dt
dt$Strain


##==============================================
##  SNP data from history
##  Save binary and load for use in the future
##==============================================

dt.strain <- read.csv("x:/project2017/GeneticAssociationAnalysis/JennData/strain-in-study.csv")
(dt.strain)


#genoJenData <- read.csv("x:/project2017/GeneticAssociationAnalysis/JennData/Genotype_file_223_strains_623054_SNPs.csv")
#dim(genoJenData)
#save (genoJenData, file = "Genotype_file_223_strains_623054_SNPs.rda")


##==========================================================================
load("genoJenData_full_TAGstered_130351_SNPs.rda")
load("genoJenData_mitoCarta_TAGstered_4495_SNPs.rda")
##=====================================
##  modify column names for strain ID
##=====================================
columns2retain <- c (
  "X129X1.SvJ",
  "X129S1SvlmJ",
  "A.J",
  "AKR.J",
  "BALB.cByJ",
  "BALB.cJ",
  "BTBRT.tf.J",
  "BUB.BnJ",
  "C3H.HeJ",
  "C57BL.10J",
  "C57BL.6J",
  "C57BR.cdJ",
  "C57L.J",
  "C58.J",
  "CAST.EiJ",
  "CBA.J",
  "CE.J",
  "DBA.2J",
  "FVB.NJ",
  "I.LnJ",
  "KK.HIJ",
  "MA.MyJ",
  "MOLF.EiJ",
  "MRL.MpJ",
  "NOR.LtJ",
  "NZO.HILtJ",
  "NZW.LacJ",
  "P.J",
  "PL.J",
  "PWD.PhJ",
  "PWK.PhJ",
  "RIIIS.J",
  "SJL.J",
  "SM.J",
  "SWR.J",
  "WSB.EiJ",
  "NOD.LtJ"
)

length(which(colnames(genoJenData.full.TAGstered) %in% columns2retain))
columns2retain[(which(columns2retain %in% colnames(genoJenData.full.TAGstered)))]
length(which(columns2retain %in% colnames(genoJenData.full.TAGstered)))

genoData <- genoJenData.full.TAGstered[,c(1:16, which(colnames(genoJenData.full.TAGstered)%in% columns2retain))]
save (genoData, file = "Genotype_file_37_strains_130351_TAGster_SNPs.rda")


##=======================================================
##  Clean Genotype data by removing the mono-morphic SNP
##  
##=======================================================
dim(genoData)
#[1] 130351    53
snp.2.exclude <- c()
index = 1
for (i in 1:dim(genoData)[1])
{
  if (length(unique(as.character(as.vector(genoData [i,17:53])))) ==1 )
  {
    snp.2.exclude[index] = i
    index = index + 1
  }
}

if (!is.null(snp.2.exclude)) {
  snp.4.study.filtered <- genoData[-snp.2.exclude,]
}else{
  snp.4.study.filtered <-  genoData
}


dim(snp.4.study.filtered)

#[1] 130351    53

save (snp.4.study.filtered, file = "processed_Genotype_file_37_strains_130351_TAGster_SNPs.rda")




##====================================================================

##  Modify genopty information for EMMA kinship computation
##  

# n0 <- sum(snps==0,na.rm=TRUE)    <--- HomoReference
# nh <- sum(snps==0.5,na.rm=TRUE)  <--- Heterzygote
# n1 <- sum(snps==1,na.rm=TRUE)    <--- HomoSNP
# nNA <- sum(is.na(snps))          <--- Missing genoptye

genotypeInNumeric <- matrix (0, nrow = dim(snp.4.study.filtered)[1], ncol = 37)
for (j in 1:dim(snp.4.study.filtered)[1])
{
  tempRow     = snp.4.study.filtered[j,]
  genotypeNum <- c (rep(0, length(as.vector(tempRow[c(17:53)]))))
  for (i in 1:length(as.vector(tempRow[c(17:53)])))
  {
    temp <- as.vector(sapply(tempRow[c(17:53)],as.character))[i]
    if (temp == as.character(tempRow$allele.B)) {genotypeNum[i] = 1}
    else if (temp == "N" | temp == "NA") {genotypeNum[i] = NA}
    else if (temp == "H") {genotypeNum[i] = 0.5}
  }
  genotypeInNumeric[j,] <- genotypeNum
} 

head(genotypeInNumeric)
dim(genotypeInNumeric)
colnames(genotypeInNumeric) <- colnames(snp.4.study.filtered)[-c(1:16)]
rownames(genotypeInNumeric) <- as.character(snp.4.study.filtered$JAX.SNP.ID)
save (genotypeInNumeric, file = "genotypeInNumeric_tagged_130351.rda")


##===========================================================================
load ("genotypeInNumeric.rda")
dim(genotypeInNumeric)
#[1] 596933     37

snp.NA.exclude <- c()

index = 1
for (i in 1:dim(genotypeInNumeric)[1])
{
  if ( length(which(is.na(genotypeInNumeric[i,]))) >= 1 )
  {
    snp.NA.exclude[index] = i
    index = index + 1
  }
}

length(snp.NA.exclude) 

snp.NA.exclude.2 <- c(rep (0,dim(genotypeInNumeric)[1]))
index = 1
apply (genotypeInNumeric, 1, function (x)
  {
  if (is.na(x) >=1)
  {
    snp.NA.exclude.2[index] = 1
    index = index + 1
  }else{
    index = index + 1
  }
})

which(snp.NA.exclude.2 == 1) 


foreach 

#[1] 116134 # NAs
head(genotypeInNumeric[snp.NA.exclude,])


##=============================================
##  Here are codes performing the emma
##  version 1
##=============================================
snp.NA.exclude <- c()
index = 1
for (i in 1:dim(genotypeInNumeric)[1])
{
  if ( length(which(is.na(genotypeInNumeric[i,]))) > 5 )
  {
    snp.NA.exclude[index] = i
    index = index + 1
  }
}

xs <- genotypeInNumeric[- snp.NA.exclude,]
dim(xs)
#[1] 587668     37

dim(snp.4.study.filtered)
snp.info <- snp.4.study.filtered[- snp.NA.exclude,]
dim(snp.info)
#[1] 587668     53
##=======================================
##  clean up genotype data by filtering 
##  minor allel frequency < 1% or NAs
##======================================

allele.frequency <- apply (xs, 1, function (x) {sum(x,na.rm=T)})
allele.var <- apply (xs, 1, function (x) {var(x,na.rm=T)})


                     
xs.maf_1 <- xs [-which(allele.frequency <=1),]


final.exclude <- c (which(allele.frequency <=1),  which(is.na(allele.frequency)))
xs.problem.free <- xs [-final.exclude,]
snp.info <- snp.info  [-final.exclude,]

snp.numeric.info <- cbind(snp.info[,c(1:16)],as.data.frame(xs.problem.free))
dim(snp.numeric.info)


xs <- as.data.frame(xs.problem.free)
dim(xs)


##======================================
##  Anchoring on phenotypes/strains
##  Jen's data ONLY have 36 strains
##  Without phenotype from "NOD/LtJ"
##=====================================

load("../JennData/JenData4EMMA.rda")
final.dt <- JenData4Emma$phenos


which(colnames(final.dt) %in%  "NOD/LtJ" )
dim(final.dt)

##===========================================
##  Wil use phenotypeFromJen as phenotypes
##===========================================

t(phenotypeFromJen)[1,]
dim(phenotypeFromJen)

Y = t(phenotypeFromJen[-1])
colnames(Y) <- t(phenotypeFromJen)[1,]
rownames(Y)

##======================================

dim(xs)



xs.in.model <- xs[,-25]
dim(xs.in.model)

##================================
##  Need to refilter low MAF <=1 

allele.frequency <- apply (xs.in.model, 1, sum)
allele.var <- apply (xs.in.model, 1, var)

xs.maf_1 <- xs.in.model [-which(allele.frequency <=1),]


xs.in.model <- xs.maf_1

##===================================
##  Getting kinship
##===================================


source("x:/project2017/GeneticAssociationAnalysis/Rscripts/emma.R")
# Start the clock!
ptm <- proc.time()
K <- emma.kinship(data.matrix(xs.in.model),"additive","all")
# Stop the clock
proc.time() - ptm


##=========================================================
Y.in.model <- matrix(sapply(data.matrix(Y), as.numeric), nrow = dim(data.matrix(Y))[1], ncol = dim(data.matrix(Y))[2])

rs <- emma.ML.LRT(Y.in.model, as.matrix(xs.in.model), K)
pval <- rbind(pval,rs$ps)
#str(pval)
save(pval, file="emma-ML-LRT-395384-pvals.rda")



#Error in if ((mean(xv) <= 0) || (mean(xv) >= 1)) { : 
#    missing value where TRUE/FALSE needed
#  In addition: Warning messages:
#    1: In mean.default(xv) : argument is not numeric or logical: returning NA
#  2: In mean.default(xv) : argument is not numeric or logical: returning NA


rs <- emma.REML.t(Y.in.model, as.matrix(xs.in.model), K)
#Error in if ((mean(xv) <= 0) || (mean(xv) >= 1)) { : 
#    missing value where TRUE/FALSE needed
#  In addition: Warning messages:
#    1: In mean.default(xv) : argument is not numeric or logical: returning NA
#  2: In mean.default(xv) : argument is not numeric or logical: returning NA

##==============================================================================

##===========================================
xs.in.model <- as.matrix(xs.in.model)
dim(xs.in.model)[1] - 395*1000

for (i in 1:397) 
{
  j = (i-1)*1000 + 1
  k = i*1000
  xs <- xs.in.model [j:k,]
  rs <- emma.ML.LRT(Y.in.model, xs, K)
  
  if (i == 1){
    pval <- rs$ps
  }else{
    pval <- rbind(pval,rs$ps)
  }
}

##  Add the last 467 tests

xs <- (xs.in.model) [397001:397467,]
rs <- emma.ML.LRT(Y.in.model, xs, K)
#str(rs$ps)
#plot(rs$ps[,1],rs$ps[,2])

pval <- rbind(pval,rs$ps)
#str(pval)

save(pval, file="emma-ML-LRT-397467-pvals.rda")


##===========================================
dim(Y.in.model)
str(Y.in.model)
dim(xs.in.model)
dim(K)

##====================================
##  Test which snp has problem
##====================================
for (i in 57001:58000){
  xs <- xs.in.model [i,]
  rs <- emma.ML.LRT(Y.in.model, xs, K)
  
  if (i == 57001){
    pval <- rs$ps
  }else{
    pval <- rbind(pval,rs$ps)
  }
}

