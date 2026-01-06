####Robust PCA#
#Install Packages and Set Libraries
#install.packages("haven", dependencies=TRUE)
#install.packages("emmeans")
#install.packages("statmod")
#install.packages("lme4")
#install.packages("lmerTest")
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("rospca")
#install.packages("robustbase")
#install.packages("rrcov")
#install.packages("moments")

#Load Libraries
library ("tidyverse")
library("haven")
library("emmeans")
library("lme4")
library("lmerTest")
library ("dplyr")
library ("rospca")
library("robustbase")
library("rrcov")
library("moments")

#######Loading Full (Unfiltered) Dataset #####
NeuroImage <- read_sav("Input_For_ROBPCA.sav")

#############################################################################################
############################    SST (Destrieux and Aseg Atlas)   ############################
############################    ALL Correct Stop vs Correct Go   ############################
#############################################################################################
SST_QC <- filter(NeuroImage, Exclude_SST2 == 0)
SST_Vars_dst <- c("ID", "SST_BL_isvcg_IFG_dst", "SST_BL_isvcg_Precuneus_dst",
                  "SST_BL_isvcg_dlPFC_dst", "SST_BL_isvcg_AntInsula_dst",  
                  "SST_BL_isvcg_dACC_dst", "SST_BL_isvcg_SubParLob_dst",
                  "SST_BL_isvcg_SupFronGyr_dst",
                  "SST_BL_isvcg_Caudate_aseg", "SST_BL_isvcg_vmPFC_dst",
                  "SST_BL_isvcg_Putamen_aseg", "SST_BL_isvcg_Thalamus_aseg")
SST_Data_dst <- SST_QC %>%
  select(all_of(SST_Vars_dst))
SST_Data_dst_Clean <- SST_Data_dst[complete.cases(SST_Data_dst),]
nrow(SST_Data_dst) - nrow(SST_Data_dst_Clean)

#Skewness
SST_Skew <- as.data.frame(skewness(SST_Data_dst_Clean[, -1]))

#Mean Centering
SST <- scale(SST_Data_dst_Clean[, -1], center = TRUE, scale = TRUE)
(n <- nrow(SST))
(p <- ncol(SST))

#Preliminary ROBPCA
SST_Prelim_rpca <- robpca(SST, k = 4, alpha = 0.75, mcd = TRUE, ndir = 5000, skew = FALSE)
SST_Prelim_Eigenvalues <- SST_Prelim_rpca$eigenvalues
plot(SST_Prelim_Eigenvalues, type = "b",
     main = "SST_Prelim Screeplot - ROBPCA",
     xlab = "Principal Component",
     ylab = "Eigenvalue")

#ROBPCA: Preliminary Model Results
SST_Prelim_eigenvalues <- SST_Prelim_rpca$eigenvalues
SST_Prelim_loadings <- as.data.frame(SST_Prelim_rpca$loadings)
SST_Prelim_prop_var <- SST_Prelim_eigenvalues/sum(SST_Prelim_eigenvalues)
SST_Prelim_prop_var <- as.data.frame(SST_Prelim_prop_var)

#Print results
SST_Prelim_eigenvalues
SST_Prelim_loadings
SST_Prelim_prop_var

###DECISION: 4 COMPONENTS
#ROBPCA
SST_dst_rpca <- robpca(SST, k = 4, alpha = 0.75, mcd = TRUE, ndir = 5000, skew = FALSE)

#ROBPCA: Model Results
SST_dst_eigenvalues <- SST_dst_rpca$eigenvalues
SST_dst_loadings <- as.data.frame(SST_dst_rpca$loadings)
SST_dst_prop_var <- SST_dst_eigenvalues/sum(SST_dst_eigenvalues)


plot(SST_dst_prop_var, type = "b",
     main = "SST_Proportional Variance Plot - ROBPCA",
     xlab = "Principal Component",
     ylab = "Proportion of Variance Explained")

SST_dst_prop_var <- as.data.frame(SST_dst_prop_var)

#############################################################################################
#############################    MID (Desikan and Aseg Atlas)   #############################
##########################    ALL Reward Anticipation vs Neutral   ##########################
#############################################################################################

MID_QC <- filter(NeuroImage, Exclude_MID == 0)
MID_Vars_dsk <- c("ID", "MID_BL_arvn_rACC_dsk", "MID_BL_arvn_cACC_dsk", "MID_BL_arvn_Insula_dsk", 
                  "MID_BL_arvn_LatOccCortex_dsk", "MID_BL_arvn_Medobfr_dsk", "MID_BL_arvn_Latobfr_dsk", 
                  "MID_BL_arvn_PCC_dsk", "MID_BL_arvn_PrecentralGy_dsk", "MID_BL_arvn_SupFronGyr_dsk", 
                  "MID_BL_arvn_Amygdala_aseg", "MID_BL_arvn_Caudate_aseg", "MID_BL_arvn_VentDienc_aseg", 
                  "MID_BL_arvn_NucleusAcc_aseg", "MID_BL_arvn_Putamen_aseg", "MID_BL_arvn_Thalamus_aseg", 
                  "MID_BL_arvn_Hippocampus_aseg")
MID_Data_dsk <- MID_QC %>%
  select(all_of(MID_Vars_dsk))

##Removing Null Values
MID_Data_dsk_Clean <- MID_Data_dsk[complete.cases(MID_Data_dsk),]
nrow(MID_Data_dsk) - nrow(MID_Data_dsk_Clean)

#Skewness
MID_Skew <- as.data.frame(skewness(MID_Data_dsk_Clean[, -1]))

#Mean Centering
Y <- scale(MID_Data_dsk_Clean[, -1], center = TRUE, scale = TRUE)
(n <- nrow(Y))
(p <- ncol(Y))

#Preliminary ROBPCA
MID_Prelim_rpca <- robpca(Y, k = 0, alpha = 0.75, mcd = TRUE, ndir = 5000, skew = FALSE)
MID_Prelim_Eigenvalues <- MID_Prelim_rpca$eigenvalues
plot(MID_Prelim_Eigenvalues, type = "b",
     main = "MID_Prelim Screeplot - ROBPCA",
     xlab = "Principal Component",
     ylab = "Eigenvalue")

#ROBPCA: Preliminary Model Results
MID_Prelim_eigenvalues <- MID_Prelim_rpca$eigenvalues
MID_Prelim_loadings <- as.data.frame(MID_Prelim_rpca$loadings)
MID_Prelim_prop_var <- MID_Prelim_eigenvalues/sum(MID_Prelim_eigenvalues)
MID_Prelim_prop_var <- as.data.frame(MID_Prelim_prop_var)

#Print results
MID_Prelim_eigenvalues
MID_Prelim_loadings
MID_Prelim_prop_var

###DECISION: 5 COMPONENTS
#ROBPCA
MID_dsk_rpca <- robpca(Y, k = 5, alpha = 0.75, mcd = TRUE, ndir = 5000, skew = FALSE)

#ROBPCA: Model Results
MID_dsk_eigenvalues <- MID_dsk_rpca$eigenvalues
MID_dsk_loadings <- MID_dsk_rpca$loadings
MID_dsk_prop_var <- MID_dsk_eigenvalues/sum(MID_dsk_eigenvalues)

MID_dsk_eigenvalues
MID_dsk_loadings
MID_dsk_prop_var


#############################################################################################
############################    EN-Back (Desikan and Aseg Atlas)   ##########################
###########################    All Emotional vs Neutral Faces       #########################
#############################################################################################
##############nBack dsk + ASEG
nBackQC <- filter(NeuroImage, Exclude_nBack == 0)
nBackVars_emovntf <- c("ID", "nBack_BL_emovntf_IFG_dsk", 
                       "nBack_BL_emovntf_dlPFC_dsk", "nBack_BL_emovntf_Insula_dsk", 
                       "nBack_BL_emovntf_cACC_dsk", "nBack_BL_emovntf_InfPariet_dsk",
                       "nBack_BL_emovntf_Caudate_aseg", "nBack_BL_emovntf_Putamen_aseg", 
                       "nBack_BL_emovntf_Pallidum_aseg", "nBack_BL_emovntf_Hippocampus_aseg", 
                       "nBack_BL_emovntf_Amygdala_aseg", "nBack_BL_emovntf_NucleusAcc_aseg")
nBackData_emovntf <- nBackQC %>%
  select(all_of(nBackVars_emovntf))

##Removing Null Values
nBackData_emovntf_Clean <- nBackData_emovntf[complete.cases(nBackData_emovntf),]
nrow(nBackData_emovntf) - nrow(nBackData_emovntf_Clean)

#Skewness
Emovntf_Skew <- as.data.frame(skewness(nBackData_emovntf_Clean[, -1]))

#Mean Centering
Z1 <- scale (nBackData_emovntf_Clean[, -1], center = TRUE, scale = TRUE)
(n <- nrow(Z1))
(p <- ncol(Z1))

#Preliminary ROBPCA
Emovntf_Prelim_rpca <- robpca(Z1, k = 0, alpha = 0.75, mcd = TRUE, ndir = 5000, skew = FALSE)
Emovntf_Prelim_Eigenvalues <- Emovntf_Prelim_rpca$eigenvalues
plot(Emovntf_Prelim_Eigenvalues, type = "b",
     main = "Emovntf_Prelim Screeplot - ROBPCA",
     xlab = "Principal Component",
     ylab = "Eigenvalue")

#ROBPCA: Preliminary Model Results
Emovntf_Prelim_eigenvalues <- Emovntf_Prelim_rpca$eigenvalues
Emovntf_Prelim_loadings <- as.data.frame(Emovntf_Prelim_rpca$loadings)
Emovntf_Prelim_prop_var <- Emovntf_Prelim_eigenvalues/sum(Emovntf_Prelim_eigenvalues)
Emovntf_Prelim_prop_var <- as.data.frame(Emovntf_Prelim_prop_var)

#Print results
Emovntf_Prelim_eigenvalues
Emovntf_Prelim_loadings
Emovntf_Prelim_prop_var

##DECISION: 4 COMPONENTS
#ROBPCA
nBack_emovntf_rpca <- robpca(Z1, k = 3, alpha = 0.75, mcd = TRUE, ndir = 5000, skew = FALSE)

#ROBPCA: Model Results
nBack_emovntf_eigenvalues <- nBack_emovntf_rpca$eigenvalues
nBack_emovntf_loadings <- nBack_emovntf_rpca$loadings
nBack_emovntf_prop_var <- nBack_emovntf_eigenvalues/sum(nBack_emovntf_eigenvalues)


#############################################################################################
############################    EN-Back (Desikan and Aseg Atlas)   ##########################
###########################    All Positive vs Neutral Faces        #########################
#############################################################################################
nBackQC <- filter(NeuroImage, Exclude_nBack == 0)
nBackVars_psfvntf <- c("ID",   "nBack_BL_psfvntf_IFG_dsk", 
                       "nBack_BL_psfvntf_dlPFC_dsk", "nBack_BL_psfvntf_Insula_dsk", 
                       "nBack_BL_psfvntf_cACC_dsk", "nBack_BL_psfvntf_InfPariet_dsk",
                       "nBack_BL_psfvntf_Caudate_aseg", "nBack_BL_psfvntf_Putamen_aseg", 
                       "nBack_BL_psfvntf_Pallidum_aseg", "nBack_BL_psfvntf_Hippocampus_aseg", 
                       "nBack_BL_psfvntf_Amygdala_aseg", "nBack_BL_psfvntf_NucleusAcc_aseg")
nBackData_psfvntf <- nBackQC %>%
  select(all_of(nBackVars_psfvntf))

##Removing Null Values
nBackData_psfvntf_Clean <- nBackData_psfvntf[complete.cases(nBackData_psfvntf),]
nrow(nBackData_psfvntf) - nrow(nBackData_psfvntf_Clean)

#Skewness
Psfvntf_Skew <- as.data.frame(skewness(nBackData_psfvntf_Clean[, -1]))

#Mean Centering
Z2 <- scale (nBackData_psfvntf_Clean[, -1], center = TRUE, scale = TRUE)
(n <- nrow(Z2))
(p <- ncol(Z2))

#ROBPCA: Preliminary Model
Psfvntf_Prelim_rpca <- robpca(Z2, k = 0, alpha = 0.75, mcd = TRUE, ndir = 5000, skew = FALSE)
Psfvntf_Prelim_Eigenvalues <- Psfvntf_Prelim_rpca$eigenvalues
plot(Psfvntf_Prelim_Eigenvalues, type = "b",
     main = "Psfvntf_Prelim Screeplot - ROBPCA",
     xlab = "Principal Component",
     ylab = "Eigenvalue")

#ROBPCA: Preliminary Model Results
Psfvntf_Prelim_eigenvalues <- Psfvntf_Prelim_rpca$eigenvalues
Psfvntf_Prelim_loadings <- as.data.frame(Psfvntf_Prelim_rpca$loadings)
Psfvntf_Prelim_prop_var <- Psfvntf_Prelim_eigenvalues/sum(Psfvntf_Prelim_eigenvalues)
Psfvntf_Prelim_prop_var <- as.data.frame(Psfvntf_Prelim_prop_var)

#Print results
Psfvntf_Prelim_eigenvalues
Psfvntf_Prelim_loadings
Psfvntf_Prelim_prop_var

#ROBPCA
nBack_psfvntf_rpca <- robpca(Z2, k = 3, alpha = 0.75, mcd = TRUE, ndir = 5000, skew = FALSE)

#ROBPCA: Model Results
nBack_psfvntf_eigenvalues <- nBack_psfvntf_rpca$eigenvalues
nBack_psfvntf_loadings <- nBack_psfvntf_rpca$loadings
nBack_psfvntf_prop_var <- nBack_psfvntf_eigenvalues/sum(nBack_psfvntf_eigenvalues)

#############################################################################################
############################    EN-Back (Desikan and Aseg Atlas)   ##########################
###########################    All Negative vs Neutral Faces        #########################
#############################################################################################
nBackQC <- filter(NeuroImage, Exclude_nBack == 0)
nBackVars_ngfvntf <- c("ID", "nBack_BL_ngfvntf_IFG_dsk", 
                       "nBack_BL_ngfvntf_dlPFC_dsk", "nBack_BL_ngfvntf_Insula_dsk", 
                       "nBack_BL_ngfvntf_cACC_dsk", "nBack_BL_ngfvntf_InfPariet_dsk",
                       "nBack_BL_ngfvntf_Caudate_aseg", "nBack_BL_ngfvntf_Putamen_aseg", 
                       "nBack_BL_ngfvntf_Pallidum_aseg", "nBack_BL_ngfvntf_Hippocampus_aseg", 
                       "nBack_BL_ngfvntf_Amygdala_aseg", "nBack_BL_ngfvntf_NucleusAcc_aseg")
nBackData_ngfvntf <- nBackQC %>%
  select(all_of(nBackVars_ngfvntf))

##Removing Null Values
nBackData_ngfvntf_Clean <- nBackData_ngfvntf[complete.cases(nBackData_ngfvntf),]
nrow(nBackData_ngfvntf) - nrow(nBackData_ngfvntf_Clean)

#Skewness
Ngfvntf_Skew <- as.data.frame(skewness(nBackData_ngfvntf_Clean[, -1]))

#Mean Centering
Z3 <- scale (nBackData_ngfvntf_Clean[, -1], center = TRUE, scale = TRUE)
(n <- nrow(Z3))
(p <- ncol(Z3))

#ROBPCA: Preliminary Model
Ngfvntf_Prelim_rpca <- robpca(Z3, k = 0, alpha = 0.75, mcd = TRUE, ndir = 5000, skew = FALSE)
Ngfvntf_Prelim_Eigenvalues <- Ngfvntf_Prelim_rpca$eigenvalues
plot(Ngfvntf_Prelim_Eigenvalues, type = "b",
     main = "Ngfvntf_Prelim Screeplot - ROBPCA",
     xlab = "Principal Component",
     ylab = "Eigenvalue")

#ROBPCA: Preliminary Model Results
Ngfvntf_Prelim_eigenvalues <- Ngfvntf_Prelim_rpca$eigenvalues
Ngfvntf_Prelim_loadings <- as.data.frame(Ngfvntf_Prelim_rpca$loadings)
Ngfvntf_Prelim_prop_var <- Ngfvntf_Prelim_eigenvalues/sum(Ngfvntf_Prelim_eigenvalues)
Ngfvntf_Prelim_prop_var <- as.data.frame(Ngfvntf_Prelim_prop_var)

#Print results
Ngfvntf_Prelim_eigenvalues
Ngfvntf_Prelim_loadings
Ngfvntf_Prelim_prop_var

#ROBPCA
nBack_ngfvntf_rpca <- robpca(Z3, k = 3, alpha = 0.75, mcd = TRUE, ndir = 5000, skew = FALSE)

#ROBPCA: Model Results
nBack_ngfvntf_eigenvalues <- nBack_ngfvntf_rpca$eigenvalues
nBack_ngfvntf_loadings <- nBack_ngfvntf_rpca$loadings
nBack_ngfvntf_prop_var <- nBack_ngfvntf_eigenvalues/sum(nBack_ngfvntf_eigenvalues)

