# Cat OUTCOME variables----
# if this is an association study, maybe MCR is my OUTCOME variable

# RECODE the data----

# save a renamed copy to play about with
MCRcombinedRecoded <- MCRcombined

MCRcombinedRecoded <- MCRcombined %>% 
  mutate(Sex.factor = # make new variable
           factor(sex) %>% # by factorising the existing 'sex' variable
           fct_recode("Male" = "1", #forcats fx, new on left, old on right
                      "Female" = "2") %>% 
           ff_label("Sex"), # optional label for finalfit
         
         MCR.factor.w3 = 
           factor(mcr_mem_w3) %>% 
           fct_recode("MCR" = "1", 
                      "No MCR" = "0") %>% 
           ff_label("MCR_w3"), 
         
         MCR.factor.w4 = 
           factor(mcr_mem_w4) %>% 
           fct_recode("MCR" = "1", 
                      "No MCR" = "0") %>% 
           ff_label("MCR_w4"), 
         
         MCR.factor.w5 = 
           factor(mcr_mem_w5) %>% 
           fct_recode("MCR" = "1", 
                      "No MCR" = "0") %>% 
           ff_label("MCR_w5"),
         
         ApoE.factor = 
           factor(APOEe4) %>% 
           fct_recode("No ApoE4" = "0", 
                      "ApoE4" = "1") %>% 
           ff_label("ApoE4"), 
         
         Smoking.factor.w3 = 
           factor(smokcurr_w3) %>% 
           fct_recode("Never" = "0",
                      "Ex-smoker" = "1",
                      "Current" = "2") %>% 
           ff_label("Smoke_w3"),
         
         Smoking.factor.w4 = 
           factor(smokcurr_w4) %>% 
           fct_recode("Never" = "0",
                      "Ex-smoker" = "1",
                      "Current" = "2") %>% 
           ff_label("Smoke_w4"),
         
         Smoking.factor.w5 = 
           factor(smokcurr_w5) %>% 
           fct_recode("Never" = "0",
                      "Ex-smoker" = "1",
                      "Current" = "2") %>% 
           ff_label("Smoke_w5"),
         
         Alcohol.factor.w3 = 
           factor(alcohol_w3) %>% 
           fct_recode("No alcohol" = "0",
                      "Drinks alcohol" = "1",) %>% 
           ff_label("Alcohol_w3"),
         
         Alcohol.factor.w4 = 
           factor(alcohol_w4) %>% 
           fct_recode("No alcohol" = "0",
                      "Drinks alcohol" = "1",) %>% 
           ff_label("Alcohol_w4"),
         
         Alcohol.factor.w5 = 
           factor(alcohol_w5) %>% 
           fct_recode("No alcohol" = "0",
                      "Drinks alcohol" = "1",) %>% 
           ff_label("Alcohol_w5"),
         
         CVD.factor.w3 = 
           factor(cvdhist_w3) %>% 
           fct_recode("No CVD" = "0",
                      "CVD" = "1",) %>% 
           ff_label("CVD_w3"),
         
         CVD.factor.w4 = 
           factor(cvdhist_w4) %>% 
           fct_recode("No CVD" = "0",
                      "CVD" = "1",) %>% 
           ff_label("CVD_w4"),
         
         CVD.factor.w5 = 
           factor(cvdhist_w5) %>% 
           fct_recode("No CVD" = "0",
                      "CVD" = "1",) %>% 
           ff_label("CVD_w5"),
         
         Stroke.factor.w3 = 
           factor(stroke_w3) %>% 
           fct_recode("No stroke" = "0",
                      "Stroke" = "1",) %>% 
           ff_label("Stroke_w3"),
         
         Stroke.factor.w4 = 
           factor(stroke_w4) %>% 
           fct_recode("No stroke" = "0",
                      "Stroke" = "1",) %>% 
           ff_label("Stroke_w4"),
         
         Stroke.factor.w5 = 
           factor(stroke_w5) %>% 
           fct_recode("No stroke" = "0",
                      "Stroke" = "1",) %>% 
           ff_label("Stroke_w5"),
         
         PD.factor.w3 = 
           factor(parkin_w3) %>% 
           fct_recode("No PD" = "0",
                      "PD" = "1",) %>% 
           ff_label("PD_w3"),
         
         PD.factor.w4 = 
           factor(parkin_w4) %>% 
           fct_recode("No PD" = "0",
                      "PD" = "1",) %>% 
           ff_label("PD_w4"),
         
         PD.factor.w5 = 
           factor(parkinsons_w5) %>% 
           fct_recode("No PD" = "0",
                      "PD" = "1",) %>% 
           ff_label("PD_w5"),
         
         Diabetes.factor.w3 = 
           factor(diab_w3) %>% 
           fct_recode("No Diabetes" = "0",
                      "Diabetes" = "1",) %>% 
           ff_label("Diabetes_w3"),
         
         Diabetes.factor.w4 = 
           factor(diab_w4) %>% 
           fct_recode("No Diabetes" = "0",
                      "Diabetes" = "1",) %>% 
           ff_label("Diabetes_w4"),
         
         Diabetes.factor.w5 = 
           factor(diab_w5) %>% 
           fct_recode("No Diabetes" = "0",
                      "Diabetes" = "1",) %>% 
           ff_label("Diabetes_w5"),
         
         Dementia.factor.w3 = 
           factor(dement_w3) %>% 
           fct_recode("No Dementia" = "0",
                      "Dementia" = "1",) %>% 
           ff_label("Dementia_w3"),
         
         Dementia.factor.w4 = 
           factor(dement_w4) %>% 
           fct_recode("No Dementia" = "0",
                      "Dementia" = "1",) %>% 
           ff_label("Dementia_w4"),
         
         Dementia.factor.w5 = 
           factor(dementia_w5) %>% 
           fct_recode("No Dementia" = "0",
                      "Dementia" = "1",) %>% 
           ff_label("Dementia_w5"),
         
         MMSE.factor.w3 = 
           factor(mmse_w3) %>% 
           fct_recode("No Dementia" = ">=24",
                      "Dementia" = "<24") %>% 
           ff_label("MMSE_w3<24"),
         
         MMSE.factor.w4 = 
           factor(mmse_w4) %>% 
           fct_recode("No Dementia" = ">=24",
                      "Dementia" = "<24") %>% 
           ff_label("MMSE_w4<24"),
         
         MMSE.factor.w5 = 
           factor(mmse_w5) %>% 
           fct_recode("No Dementia" = ">=24",
                      "Dementia" = "<24") %>% 
           ff_label("MMSE_w5<24")
  )



