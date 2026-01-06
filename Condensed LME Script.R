# ##########Mixed Effects Models
##Load Libraries
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
#library("datawizard")

#Set Working Directory
NeuroImage <- read_sav("Input_LME.sav")
##################################### SST #####################################
# Filter good cases
SST_QC <- NeuroImage

# Variables of interest
SST_Vars_LME <- c("ID", "SST_dst_PC1_scores", "Age_0", "Male", "race_ethnicity", "FamilyCluster", "SiteStrata", "itAlc0", "ScannerID", 
                  "SST_FD", "CMOD", "PDS", "ExtProbT_0", "WithDepT_0", "HH_ParEduc_0")
# Subset
SST_Data_LME <- SST_QC %>% select(all_of(SST_Vars_LME))
LME_SST <- SST_Data_LME[complete.cases(SST_Data_LME), ]
nrow(SST_Data_LME) - nrow(LME_SST)

# Mean center continuous covariates
LME_SST <- LME_SST %>%
  mutate(
    Age_0_c = scale(Age_0, center = TRUE, scale = TRUE),
    SST_FD_c = scale(SST_FD, center = TRUE, scale = TRUE),
    PDS_c = scale(PDS, center = TRUE, scale = TRUE),
    ExtProbT_0_c = scale(ExtProbT_0, center = TRUE, scale = TRUE),
    WithDepT_0_c = scale(WithDepT_0, center = TRUE, scale = TRUE),
    itAlc0_c = scale(itAlc0, center = TRUE, scale = TRUE)
  )

# Convert to factors
LME_SST <- LME_SST %>%
  mutate(
    ScannerID = as.factor(ScannerID),
    HH_ParEduc_0 = as.factor(HH_ParEduc_0),
    race_ethnicity = as.factor(race_ethnicity),
    Male = as.factor(Male),
    SiteStrata = as.factor(SiteStrata),
    FamilyCluster = as.factor(FamilyCluster),
    CMOD = relevel(as.factor(CMOD), ref = "4")
  )

# Fit the model
SST_Model1 <- lmer(
  SST_dst_PC1_scores ~ CMOD + itAlc0_c + ExtProbT_0_c + WithDepT_0_c + SST_FD_c + Age_0_c + PDS_c 
  + race_ethnicity + HH_ParEduc_0 + Male + (1|FamilyCluster),
  data = LME_SST
)

# Model summary and diagnostics
summary(SST_Model1)
SST_F <- anova(SST_Model1)
table(LME_SST$CMOD)
# Post hoc
emmeans_results_SST <- emmeans(SST_Model1, pairwise ~ CMOD, pbkrtest.limit = 7000)
emmeans_results_SST$contrasts
plot(emmeans_results_SST, comparisons = TRUE)

##################################### MID #####################################
# Filter good cases
MID_QC <- NeuroImage

# Variables of interest
MID_Vars_LME <- c("ID", "MID_dsk_PC1_scores", "Age_0", "Male", "race_ethnicity", "FamilyCluster", "SiteStrata", "itAlc0", "ScannerID", 
                  "MID_FD", "CMOD", "PDS", "ExtProbT_0", "WithDepT_0", "HH_ParEduc_0")

# Subset
MID_Data_LME <- MID_QC %>% select(all_of(MID_Vars_LME))
LME_MID <- MID_Data_LME[complete.cases(MID_Data_LME), ]
nrow(MID_Data_LME) - nrow(LME_MID)

# Mean center continuous covariates
LME_MID <- LME_MID %>%
  mutate(
    Age_0_c = scale(Age_0, center = TRUE, scale = TRUE),
    MID_FD_c = scale(MID_FD, center = TRUE, scale = TRUE),
    PDS_c = scale(PDS, center = TRUE, scale = TRUE),
    ExtProbT_0_c = scale(ExtProbT_0, center = TRUE, scale = TRUE),
    WithDepT_0_c = scale(WithDepT_0, center = TRUE, scale = TRUE),
    itAlc0_c = scale(itAlc0, center = TRUE, scale = TRUE)
  )

# Convert to factors
LME_MID <- LME_MID %>%
  mutate(
    ScannerID = as.factor(ScannerID),
    HH_ParEduc_0 = as.factor(HH_ParEduc_0),
    race_ethnicity = as.factor(race_ethnicity),
    Male = as.factor(Male),
    SiteStrata = as.factor(SiteStrata),
    FamilyCluster = as.factor(FamilyCluster),
    CMOD = relevel(as.factor(CMOD), ref = "4")
  )

# Fit the model
MID_Model2 <- lmer(
  MID_dsk_PC1_scores ~ CMOD + itAlc0_c + ExtProbT_0_c + WithDepT_0_c + MID_FD_c + Age_0_c + PDS_c 
  + race_ethnicity + HH_ParEduc_0 + Male  + (1| ScannerID/FamilyCluster),
  data = LME_MID
)

# Model summary and diagnostics
summary(MID_Model2)
MID_F <- anova(MID_Model2)

# Post hoc
emmeans_results_MID <- emmeans(MID_Model2, pairwise ~ CMOD, pbkrtest.limit = 7000)
emmeans_results_MID$contrasts
plot(emmeans_results_MID, comparisons = TRUE)

##################################### Emovntf #####################################
# Filter good cases
Emovntf_QC <- NeuroImage

# Variables of interest
Emovntf_Vars_LME <- c("ID", "nBack_emovntf_PC1_scores", "Age_0", "Male", "race_ethnicity", "FamilyCluster", "SiteStrata", "itAlc0", "ScannerID", 
                      "nBack_FD", "CMOD", "PDS", "ExtProbT_0", "WithDepT_0", "HH_ParEduc_0")


# Subset
Emovntf_Data_LME <- Emovntf_QC %>% select(all_of(Emovntf_Vars_LME))
LME_Emovntf <- Emovntf_Data_LME[complete.cases(Emovntf_Data_LME), ]
nrow(Emovntf_Data_LME) - nrow(LME_Emovntf)

# Mean center continuous covariates
LME_Emovntf <- LME_Emovntf %>%
  mutate(
    Age_0_c = scale(Age_0, center = TRUE, scale = TRUE),
    nBack_FD_c = scale(nBack_FD, center = TRUE, scale = TRUE),
    PDS_c = scale(PDS, center = TRUE, scale = TRUE),
    ExtProbT_0_c = scale(ExtProbT_0, center = TRUE, scale = TRUE),
    WithDepT_0_c = scale(WithDepT_0, center = TRUE, scale = TRUE),
    itAlc0_c = scale(itAlc0, center = TRUE, scale = TRUE)
  )

# Convert to factors
LME_Emovntf <- LME_Emovntf %>%
  mutate(
    ScannerID = as.factor(ScannerID),
    HH_ParEduc_0 = as.factor(HH_ParEduc_0),
    race_ethnicity = as.factor(race_ethnicity),
    Male = as.factor(Male),
    SiteStrata = as.factor(SiteStrata),
    FamilyCluster = as.factor(FamilyCluster),
    CMOD = relevel(as.factor(CMOD), ref = "4")
  )
# Fit the model
Emovntf_Model1 <- lmer(
  nBack_emovntf_PC1_scores ~ CMOD + itAlc0_c + ExtProbT_0_c + WithDepT_0_c + nBack_FD_c + Age_0_c + PDS_c 
  + race_ethnicity + HH_ParEduc_0 + Male  + (1|FamilyCluster),
  data = LME_Emovntf
)

# Model summary and diagnostics
summary(Emovntf_Model1)
Emovntf_F <- anova(Emovntf_Model1)

# Post hoc
emmeans_results_Emovntf <- emmeans(Emovntf_Model1, pairwise ~ CMOD, pbkrtest.limit = 7000)
emmeans_results_Emovntf$contrasts
plot(emmeans_results_Emovntf, comparisons = TRUE)

##################################### Psfvntf #####################################
# Filter good cases
Psfvntf_QC <- NeuroImage

# Variables of interest
Psfvntf_Vars_LME <- c("ID", "nBack_psfvntf_PC1_scores", "Age_0", "Male", "race_ethnicity", "FamilyCluster", "SiteStrata", "itAlc0", "ScannerID", 
                      "nBack_FD", "CMOD", "PDS", "ExtProbT_0", "WithDepT_0", "HH_ParEduc_0")

# Subset
Psfvntf_Data_LME <- Psfvntf_QC %>% select(all_of(Psfvntf_Vars_LME))
LME_Psfvntf <- Psfvntf_Data_LME[complete.cases(Psfvntf_Data_LME), ]
nrow(Psfvntf_Data_LME) - nrow(LME_Psfvntf)

# Mean center continuous covariates
LME_Psfvntf <- LME_Psfvntf %>%
  mutate(
    Age_0_c = scale(Age_0, center = TRUE, scale = TRUE),
    nBack_FD_c = scale(nBack_FD, center = TRUE, scale = TRUE),
    PDS_c = scale(PDS, center = TRUE, scale = TRUE),
    ExtProbT_0_c = scale(ExtProbT_0, center = TRUE, scale = TRUE),
    WithDepT_0_c = scale(WithDepT_0, center = TRUE, scale = TRUE),
    itAlc0_c = scale(itAlc0, center = TRUE, scale = TRUE)
  )

# Convert to factors
LME_Psfvntf <- LME_Psfvntf %>%
  mutate(
    ScannerID = as.factor(ScannerID),
    HH_ParEduc_0 = as.factor(HH_ParEduc_0),
    race_ethnicity = as.factor(race_ethnicity),
    Male = as.factor(Male),
    SiteStrata = as.factor(SiteStrata),
    FamilyCluster = as.factor(FamilyCluster),
    CMOD = relevel(as.factor(CMOD), ref = "4")
  )

# Fit the model
Psfvntf_Model1 <- lmer(
  nBack_psfvntf_PC1_scores ~ CMOD + itAlc0_c + ExtProbT_0_c + WithDepT_0_c + nBack_FD_c + Age_0_c + PDS_c 
  + race_ethnicity + HH_ParEduc_0 + Male + (1|FamilyCluster),
  data = LME_Psfvntf
)

# Model summary and diagnostics
summary(Psfvntf_Model1)
Psvntf_F <- anova(Psfvntf_Model1)

# Post hoc
emmeans_results_Psfvntf <- emmeans(Psfvntf_Model1, pairwise ~ CMOD, pbkrtest.limit = 7000)
emmeans_results_Psfvntf$contrasts
plot(emmeans_results_Psfvntf, comparisons = TRUE)


##################################### Ngfvntf #####################################
# Filter good cases
Ngfvntf_QC <- NeuroImage

# Variables of interest
Ngfvntf_Vars_LME <- c("ID", "nBack_ngfvntf_PC1_scores", "Age_0", "Male", "race_ethnicity", "FamilyCluster", "SiteStrata", "itAlc0", "ScannerID", 
                      "nBack_FD", "CMOD", "PDS", "ExtProbT_0", "WithDepT_0", "HH_ParEduc_0")

# Subset
Ngfvntf_Data_LME <- Ngfvntf_QC %>% select(all_of(Ngfvntf_Vars_LME))
LME_Ngfvntf <- Ngfvntf_Data_LME[complete.cases(Ngfvntf_Data_LME), ]
nrow(Ngfvntf_Data_LME) - nrow(LME_Ngfvntf)

# Mean center continuous covariates
LME_Ngfvntf <- LME_Ngfvntf %>%
  mutate(
    Age_0_c = scale(Age_0, center = TRUE, scale = TRUE),
    nBack_FD_c = scale(nBack_FD, center = TRUE, scale = TRUE),
    PDS_c = scale(PDS, center = TRUE, scale = TRUE),
    ExtProbT_0_c = scale(ExtProbT_0, center = TRUE, scale = TRUE),
    WithDepT_0_c = scale(WithDepT_0, center = TRUE, scale = TRUE),
    itAlc0_c = scale(itAlc0, center = TRUE, scale = TRUE)
  )

# Convert to factors
LME_Ngfvntf <- LME_Ngfvntf %>%
  mutate(
    ScannerID = as.factor(ScannerID),
    HH_ParEduc_0 = as.factor(HH_ParEduc_0),
    race_ethnicity = as.factor(race_ethnicity),
    Male = as.factor(Male),
    SiteStrata = as.factor(SiteStrata),
    FamilyCluster = as.factor(FamilyCluster),
    CMOD = relevel(as.factor(CMOD), ref = "4")
  )

# Fit the model
Ngfvntf_Model1 <- lmer(
  nBack_ngfvntf_PC1_scores ~ CMOD + itAlc0_c + ExtProbT_0_c + WithDepT_0_c + nBack_FD_c + Age_0_c + PDS_c 
  + race_ethnicity + HH_ParEduc_0 + Male  + (1|FamilyCluster),
  data = LME_Ngfvntf
)

# Model summary and diagnostics
summary(Ngfvntf_Model1)
Ngfvntf_F <- anova(Ngfvntf_Model1)

# Post hoc
emmeans_results_Ngfvntf <- emmeans(Ngfvntf_Model1, pairwise ~ CMOD, pbkrtest.limit = 7000)
emmeans_results_Ngfvntf$contrasts
plot(emmeans_results_Ngfvntf, comparisons = TRUE)
