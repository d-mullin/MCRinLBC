# Plot the data----
# To start, we simply count the number of patients with MCR who got (dementia/outcome) - or maybe reverse this. if this is an association study, maybe MCR is my OUTCOME variable. It is useful to plot this as counts but also as proportions. It is proportions you are comparing, but you really want to know the absolute numbers as well.


p1 <- MCRcombinedRecoded %>% 
  ggplot(aes(x = MCR.factor.w3, fill = Alcohol.factor.w3)) +
  geom_bar() +
  theme(legend.position = "none")


p2 <- MCRcombinedRecoded %>% 
  ggplot(aes(x = MCR.factor.w3, fill = Alcohol.factor.w3)) +
  geom_bar(position = "fill") +
  ylab("proportion")

library(patchwork)
MCRw3_Alcoholw3 <- p1 + p2
MCRw3_Alcoholw3
ggsave(MCRw3_Alcoholw3, filename = "MCRw3_Alcoholw3.png", height = 5, width = 8)


# try a another variable
p3 <- MCRcombinedRecoded %>% 
  ggplot(aes(x = MCR.factor.w3, fill = Dementia.factor.w5)) +
  geom_bar() +
  theme(legend.position = "none")


p4 <- MCRcombinedRecoded %>% 
  ggplot(aes(x = MCR.factor.w3, fill = Dementia.factor.w5)) +
  geom_bar(position = "fill") +
  ylab("proportion")

MCRw3_Demw5 <- p3 + p4
MCRw3_Demw5

ggsave(MCRw3_Demw5, filename = "MCRw3_Demw5.png", height = 5, width = 8)

# plot potential effect modification, interactions and confounders to explore prior to modelling
p5 <- MCRcombinedRecoded %>% 
  ggplot(aes(x = MCR.factor.w3, fill = Dementia.factor.w5)) +
  geom_bar() +
  facet_grid(Sex.factor ~ ApoE.factor) +
  theme(legend.position = "none")


p6 <- MCRcombinedRecoded %>% 
  ggplot(aes(x = MCR.factor.w3, fill = Dementia.factor.w5)) +
  geom_bar(position = "fill") +
  facet_grid(Sex.factor ~ ApoE.factor) +
  ylab("proportion") +
  theme(legend.position = "bottom")

MCRw3_Demw5ApoE <- p5/p6
MCRw3_Demw5ApoE

ggsave(MCRw3_Demw5ApoE, filename = "MCRw3_Demw5ApoE.png", height = 10, width = 8)

# Smoking

p7 <- MCRcombinedRecoded %>% 
  ggplot(aes(x = MCR.factor.w3, fill = Dementia.factor.w5)) +
  geom_bar() +
  facet_grid(Sex.factor ~ Smoking.factor.w3) +
  theme(legend.position = "none")


p8 <- MCRcombinedRecoded %>% 
  ggplot(aes(x = MCR.factor.w3, fill = Dementia.factor.w5)) +
  geom_bar(position = "fill") +
  facet_grid(Sex.factor ~ Smoking.factor.w3) +
  ylab("proportion") +
  theme(legend.position = "bottom")

MCRw3_Demw5smoking <- p7/p8
MCRw3_Demw5smoking

ggsave(MCRw3_Demw5smoking, filename = "MCRw3_Demw5smoking.png", height = 10, width = 8)

# CVD

p9 <- MCRcombinedRecoded %>% 
  ggplot(aes(x = MCR.factor.w3, fill = Dementia.factor.w5)) +
  geom_bar() +
  facet_grid(Sex.factor ~ CVD.factor.w3) +
  theme(legend.position = "none")


p10 <- MCRcombinedRecoded %>% 
  ggplot(aes(x = MCR.factor.w3, fill = Dementia.factor.w5)) +
  geom_bar(position = "fill") +
  facet_grid(Sex.factor ~ CVD.factor.w3) +
  ylab("proportion") +
  theme(legend.position = "bottom")

MCRw3_Demw5cvd <- p9/p10
MCRw3_Demw5cvd

ggsave(MCRw3_Demw5cvd, filename = "MCRw3_Demw5cvd.png", height = 10, width = 8)

# Stroke

p11 <- MCRcombinedRecoded %>% 
  ggplot(aes(x = MCR.factor.w3, fill = Dementia.factor.w5)) +
  geom_bar() +
  facet_grid(Sex.factor ~ Stroke.factor.w3) +
  theme(legend.position = "none")


p12 <- MCRcombinedRecoded %>% 
  ggplot(aes(x = MCR.factor.w3, fill = Dementia.factor.w5)) +
  geom_bar(position = "fill") +
  facet_grid(Sex.factor ~ Stroke.factor.w3) +
  ylab("proportion") +
  theme(legend.position = "bottom")

MCRw3_Demw5stroke <- p11/p12
MCRw3_Demw5stroke

ggsave(MCRw3_Demw5stroke, filename = "MCRw3_Demw5stroke.png", height = 10, width = 8)

# PD

p13 <- MCRcombinedRecoded %>% 
  ggplot(aes(x = MCR.factor.w3, fill = Dementia.factor.w5)) +
  geom_bar() +
  facet_grid(Sex.factor ~ PD.factor.w3) +
  theme(legend.position = "none")


p14 <- MCRcombinedRecoded %>% 
  ggplot(aes(x = MCR.factor.w3, fill = Dementia.factor.w5)) +
  geom_bar(position = "fill") +
  facet_grid(Sex.factor ~ PD.factor.w3) +
  ylab("proportion") +
  theme(legend.position = "bottom")

MCRw3_Demw5pd <- p13/p14
MCRw3_Demw5pd

ggsave(MCRw3_Demw5pd, filename = "MCRw3_Demw5pd.png", height = 10, width = 8)

# Diabetes

p15 <- MCRcombinedRecoded %>% 
  ggplot(aes(x = MCR.factor.w3, fill = Dementia.factor.w5)) +
  geom_bar() +
  facet_grid(Sex.factor ~ Diabetes.factor.w3) +
  theme(legend.position = "none")


p16 <- MCRcombinedRecoded %>% 
  ggplot(aes(x = MCR.factor.w3, fill = Dementia.factor.w5)) +
  geom_bar(position = "fill") +
  facet_grid(Sex.factor ~ Diabetes.factor.w3) +
  ylab("proportion") +
  theme(legend.position = "bottom")

MCRw3_Demw5diabetes <- p15/p16
MCRw3_Demw5diabetes

ggsave(MCRw3_Demw5diabetes, filename = "MCRw3_Demw5diabetes.png", height = 10, width = 8)

# Correlation Matrix (of continuous variables) ----
# https://www.displayr.com/how-to-create-a-correlation-matrix-in-r/
# really should check if the cont vars are para aka normal (Pearson) or nonpara (Spearman) first to decide on which test to run
# in this instance, I'm just getting a rough idea of which cog tests correlate 
# Step 1 - subset data to only include cont variables

cor_matrix_data <- MCRcombinedRecoded %>% 
  select( "agedays_w1", "agedays_w2", "agedays_w3", "agedays_w4","yrsedu_w1", "alcunitwk_w1", "alcunitwk_w2", "alcunitwk_w3", "alcunitwk_w4", "hadsa_w1", "hadsa_w2", "hadsa_w3", "HADS_A_w4","hadsd_w1" , "hadsd_w2", "hadsd_w3", "HADS_D_w4","mmse_w1", "mmse_w2", "mmse_w3","mmse_w4" , "age11IQ" , "age70IQ_w1", "mht1947", "mht_w1", "lm1_re_w1", "lm1_re_w2" , "lm1_re_w3", "lm1_re_w4", "lm2_re_w1", "lm2_re_w2"  , "lm2_re_w3", "lm2_re_w4", "lm2_tu_w1", "lm2_tu_w2", "lm2_tu_w3", "lm2_tu_w4", "lmtotal_w1", "lmtotal_w2", "lmtotal_w3", "lmtotal_w4", "vpatotal_w1", "vpatotal_w2" ,  "vpatotal_w3",  "vpatotal_w4", "spantot_w1" , "spantot_w2"       , "spantot_w3", "spantot_w4", "matreas_w1", "matreas_w2", "matreas_w3", "matreas_w4", "vftot_w1", "vftot_w2", "vftot_w3",  "vftot_w4" , "digback_w1", "digback_w2", "digback_w3", "digback_w4", "nart_w1", "nart_w2", "nart_total_w3", "nart_total_w4", "wtar_w1", "wtar_w2", "wtar_total_w3", "wtar_total_w4", "blkdes_w1", "blkdes_w2", "blkdes_w3", "blkdes_w4", "ittotal_w1", "ittotal_w2", "ittotal_w3", "ittotal_w4" , "digsym_w1", "digsym_w2","digsym_w3", "digsym_w4" ,"srtmean_w1" ,"srtmean_w2", "srtmean_w3", "srtmean_w4", "crtmean_w1","crtmean_w4" ,"trailmakingtime_w3", "trailmakingtime_w4", "height_w1", "height_w2", "height_w3", "height_w4", "weight_w1", "weight_w2", "weight_w3","weight_w4", "bmi_w1", "bmi_w2","bmi_w3", "bmi_w4" , "sixmwk_w1", "sixmwk_w2", "sixmwk_w3", "sixmwk_w4","sitstnd_w1", "dbp1sit_w1" , "dbp1sit_w2", "dbp1sit_w3", "dbp1sit_w4", "sbp1sit_w1", "sbp1sit_w2", "sbp1sit_w3" ,"sbp1sit_w4" ,"dbp1std_w1", "dbp1std_w2" , "dbp1std_w3", "dbp1std_w4", "sbp1std_w1", "sbp1std_w2" , "sbp1std_w3", "sbp1std_w4", "fev_w1", "fev_w2", "fev_w3", "fev_w4", "griprh_w1" , "griprh_w2", "griprh_w3", "griprh_w4" , "griplh_w1", "bld_crprot_w2" , "bld_crprot_w3", "bld_crprot_w4", "APOEe4"      , "alcunitwk_w5" ,"HADS_A_w5", "HADS_D_w5", "HADS_total_w5"    , "mmse_w5" , "lm1_re_w5" , "lm2_re_w5" , "lm2_tu_w5"           , "lmtotal_w5", "vpa_total_w5" ,  "spantot_w5" , "matreas_w5" , "vftot_w5" , "digback_w5" , "nart_total_w5"     ,"wtar_total_w5" , "blkdes_w5","ittotal_w5", "digsym_w5" , "srtmean_w5" , "crtmean_w5" , "trailmakingtime_w5" , "height_w5" , "weight_w5", "bmi_w5","sixmwk_w5", "dbp1sit_w5" ,"sbp1sit_w5" ,"dbp1std_w5", "sbp1std_w5","fev_w5", "griprh_w5", "chairst_w5" , "chairst_sec_w5", "bld_choles_w5", "bld_hba1c_IFFC_w5"        ,"bld_crprot_w5")

str(cor_matrix_data)
head(cor_matrix_data, 3)
cor_matrix_data.cor = cor(cor_matrix_data, method = c("spearman"), use = "complete.obs")

# Significance levels (p-values) can also be generated using the rcorr function which is found in the Hmisc package. 
install.packages("Hmisc")
library("Hmisc")
cor_matrix_data.rcorr = rcorr(as.matrix(cor_matrix_data)) #This generates one table of correlation coefficients (the correlation matrix) and another table of the p-values. 
cor_matrix_data.rcorr 

# extract the values from this object into a useable data structure
cor_matrix_data.coeff = cor_matrix_data.rcorr$r
cor_matrix_data.p = cor_matrix_data.rcorr$P

# Visualizing the correlation matrix
install.packages("corrplot")
library(corrplot)
corrplot(cor_matrix_data.cor)


# Summarising factors with finalfit ----
# if this is an association study, maybe MCR is my OUTCOME variable

MCRcombinedRecoded %>% 
  summary_factorlist(dependent = "Dementia.factor.w5",
                     explanatory = "MCR.factor.w3")

# Chi-squared / Fisher’s exact test using finalfit. Including p = TRUE in summary_factorlist() adds a hypothesis test to each included comparison. This defaults to chi-squared tests with a continuity correction for categorical variables.
MCRcombinedRecoded %>% 
  summary_factorlist(dependent = "Dementia.factor.w5",
                     explanatory = "MCR.factor.w3",
                     p = TRUE)



# Adding further variables
MCRcombinedRecoded %>% 
  summary_factorlist(dependent = "Dementia.factor.w5",
                     explanatory = c("MCR.factor.w3", "Sex.factor", "Smoking.factor.w3", "Alcohol.factor.w3", "Stroke.factor.w3", "CVD.factor.w3", "MMSE.factor.w3"),
                     p = TRUE)

# using fisher's instead of Chi2 as numbers are small (<1000 in sample, or <5 in over 20% of the groups) https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5426219/
MCRcombinedRecoded %>% 
  summary_factorlist(dependent = "Dementia.factor.w5",
                     explanatory = c("MCR.factor.w3", "Sex.factor", "Smoking.factor.w3", "Alcohol.factor.w3", "Stroke.factor.w3", "CVD.factor.w3", "MMSE.factor.w3"),
                     p = TRUE,
                     p_cat = "fisher")

# Demographics Table ----
# https://argoshare.is.ed.ac.uk/healthyr_book/including-missing-data-in-demographics-tables.html 
table1 <- MCRcombinedRecoded %>% 
  summary_factorlist(dependent = "Dementia.factor.w5",
                     explanatory = c("MCR.factor.w3", "Sex.factor", "Smoking.factor.w3", "Alcohol.factor.w3", "Stroke.factor.w3", "CVD.factor.w3", "MMSE.factor.w3"),
                     p = TRUE,
                     p_cat = "fisher",
                     digits = c(1,1,4,2), #1: mean/median, 2: SD/IQR 
                     # 3: p-value, 4: count percentage 
                     na_include = TRUE, # include missing data from the explanatory variables (but not dependent) in final table
                     na_include_dependent = TRUE, # include missing data from the dependent variable
                     total_col = TRUE, #Including a total column 
                     add_col_totals = TRUE, # including column totals
                     add_dependent_label = TRUE)

table1

# save using here::here - on a Mac you would otherwise do read_csv("data/melanoma.csv") and on Windows you would have to do read_csv("data\melanoma.csv"). Having to include either / (GNU/Linux, macOS) or \ (Windows) in your script means it will have to be changed by hand when running on a different system. What here::here("data_raw", "melanoma.csv"), however, works on any system, as it will use an appropriate one ‘behind the scenes’ without you having to change anything.
save(table1, dependent, explanatory,
     file = here::here("data", "out.rda"))