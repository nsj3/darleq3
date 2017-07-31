library(readxl)

darleq3_data <- list()
darleq3_data$defaults <- list()

darleq3_data$defaults$minAlkTDI3=6
darleq3_data$defaults$maxAlkTDI3=150
darleq3_data$defaults$minAlkTDI4=5
darleq3_data$defaults$maxAlkTDI4=250
darleq3_data$defaults$minAlkTDI5LM=5
darleq3_data$defaults$maxAlkTDI5LM=250
darleq3_data$defaults$minAlkTDI5NGS=5
darleq3_data$defaults$maxAlkTDI5NGS=250
darleq3_data$defaults$minCa = 5
darleq3_data$defaults$maxCa = 500
darleq3_data$defaults$minDOC = 0
darleq3_data$defaults$maxDOC = 50
darleq3_data$defaults$defaultAlkalinity = 100
darleq3_data$defaults$defaultCa = 100
darleq3_data$defaults$defaultDOC = 1.0
darleq3_data$defaults$boundariesTDI3 <- c(HG=0.93, GM=0.78, MP=0.52, PB=0.26)
darleq3_data$defaults$boundariesTDI4 <- c(HG=0.8, GM=0.6, MP=0.4, PB=0.2)
darleq3_data$defaults$boundariesTDI5LM <- c(HG=0.8, GM=0.6, MP=0.4, PB=0.2)
darleq3_data$defaults$boundariesTDI5NGS <- c(HG=0.8, GM=0.6, MP=0.4, PB=0.2)
darleq3_data$defaults$boundariesLTDI1_HA <- c(HG=0.9, GM=0.66, MP=0.44, PB=0.22)
darleq3_data$defaults$boundariesLTDI1_MA <- c(HG=0.9, GM=0.66, MP=0.44, PB=0.22)
darleq3_data$defaults$boundariesLTDI1_LA <- c(HG=0.9, GM=0.66, MP=0.44, PB=0.22)
darleq3_data$defaults$boundariesLTDI2_HA <- c(HG=0.92, GM=0.7, MP=0.46, PB=0.23)
darleq3_data$defaults$boundariesLTDI2_MA <- c(HG=0.93, GM=0.66, MP=0.46, PB=0.23)
darleq3_data$defaults$boundariesLTDI2_LA <- c(HG=0.92, GM=0.7, MP=0.46, PB=0.23)
darleq3_data$defaults$boundariesDAM <- c(HG=0.81, GM=0.65, MP=0.44, PB=0.22)
darleq3_data$defaults$medianTDI_LTDI1 <- c(HA=25, MA=25, LA=20)
darleq3_data$defaults$medianTDI_LTDI2 <- c(HA=42, MA=35, LA=22)
darleq3_data$defaults$defaultLakeType <- "MA"
darleq3_data$defaults$TDI_Norm_Factor <- c(TDI3=1.0, TDI4=0.8, TDI5LM=0.8, TDI5NGS=0.8)
darleq3_data$defaults$CoC_LTDI <- c(A0=0.03, B1=0.273, B2=-0.253, Power=1.96)
darleq3_data$defaults$CoC_TDI <- c(A0=0.03, B1=0.177, B2=-0.157, Power=5.73)
darleq3_data$metric.codes <- c("TDI3", "TDI4", "TDI5LM", "TDI5NGS", "LTDI1", "LTDI2", "DAM")
darleq3_data$metric.types <- c("TDILM", "LTDILM", "DAMLM", "TDINGS")

col_types <- c(rep("text", 8), rep("numeric", 13))
darleq3_taxa <- as.data.frame(read_excel("\\Data\\R_Libraries\\People\\Martyn_Kelly\\Barcoding\\TaxonLists\\DarleqTaxonList2017_Master.xlsx", col_types=col_types))
rm(col_types)

load("..\\darleq3_test\\NGS_ma.Rda")
load("..\\darleq3_test\\NGS_mono_mod.Rda")

darleq3_data$ma.coef <- ma
darleq3_data$mono.mod <- mono.mod

save(darleq3_taxa, darleq3_data, file="data\\darleq3_data.rda")

