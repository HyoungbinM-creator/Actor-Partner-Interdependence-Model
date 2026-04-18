options(scipen = 999)

library(haven)
library(tidyverse)

#setting working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#reading the data
data <- read_sav('final_clean_data1.sav') 

#filtering
data$W_Relstatus_T1 <- as.factor(data$W_Relstatus_T1)
data$H_Relstatus_T1 <- as.factor(data$H_Relstatus_T1)
data.m <- data[data$W_Relstatus_T1 == 1 & data$H_Relstatus_T1 == 1, ]

library(lavaan)
library(psych)
library(ggplot2)

##APIM predicting RSAT with variables from wave 0
fit1a1 <- 'H_RSAT1_T0 ~  H_DMP_w_T0 + H_DMP_v_T0 + H_DMP_s_T0 + W_DMP_w_T0 + W_DMP_v_T0 + W_DMP_s_T0 

         W_RSAT1_T0 ~ W_DMP_w_T0 + W_DMP_v_T0 + W_DMP_s_T0 + H_DMP_w_T0 + H_DMP_v_T0 + H_DMP_s_T0 

         H_RSAT1_T0 ~~ W_RSAT1_T0'
apim1a1 <- sem(fit1a1, fixed.x=FALSE, data = data.m, missing = "fiml")
summary(apim1a1, fit.measures = T)

##APIM predicting RSAT with gender constraints
fit1c1 <- 'H_RSAT1_T0 ~  a1*H_DMP_w_T0 + a2*H_DMP_v_T0 + a3*H_DMP_s_T0 + p1*W_DMP_w_T0 + p2*W_DMP_v_T0 + p3*W_DMP_s_T0 

         W_RSAT1_T0 ~ a1*W_DMP_w_T0 + a2*W_DMP_v_T0 + a3*W_DMP_s_T0 + p1*H_DMP_w_T0 + p2*H_DMP_v_T0 + p3*H_DMP_s_T0 

         H_RSAT1_T0 ~~ W_RSAT1_T0'
apim1c1 <- sem(fit1c1, fixed.x=FALSE, data = data.m, missing = "fiml")
summary(apim1c1, fit.measures = T)

anova(apim1c1, apim1a1)


#two-wave longitudinal analyses 
##APIM predicting RSAT_T2 with independent variables from Time 1
fit1a2 <- 'H_RSAT1_T1 ~  H_DMP_w_T0 + H_DMP_v_T0 + H_DMP_s_T0 + W_DMP_w_T0 + W_DMP_v_T0 + W_DMP_s_T0 + H_RSAT1_T0 

         W_RSAT1_T1 ~ W_DMP_w_T0 + W_DMP_v_T0 + W_DMP_s_T0 + H_DMP_w_T0 + H_DMP_v_T0 + H_DMP_s_T0 + W_RSAT1_T0

         H_RSAT1_T1 ~~ W_RSAT1_T1'
apim1a2 <- sem(fit1a2, fixed.x=FALSE, data = data.m, missing = "fiml")
summary(apim1a2, fit.measures = T)

##APIM predicting RSAT_T2 with independent variables from Time 1
fit1c2 <- 'H_RSAT1_T1 ~  b1*H_DMP_w_T0 + b2*H_DMP_v_T0 + b3*H_DMP_s_T0 + bp1*W_DMP_w_T0 + bp2*W_DMP_v_T0 + bp3*W_DMP_s_T0 + H_RSAT1_T0 
           
           W_RSAT1_T1 ~ b1*W_DMP_w_T0 + b2*W_DMP_v_T0 + b3*W_DMP_s_T0 + bp1*H_DMP_w_T0 + bp2*H_DMP_v_T0 + bp3*H_DMP_s_T0 + W_RSAT1_T0
           
           H_RSAT1_T1 ~~ W_RSAT1_T1

'

apim1c2 <- sem(fit1c2, fixed.x=FALSE, data = data.m, missing = "fiml")
summary(apim1c2, fit.measures = T)

anova(apim1c2, apim1a2)