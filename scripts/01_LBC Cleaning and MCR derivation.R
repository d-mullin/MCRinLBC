# Title: LBC waves 1 - 5, Deriving the MCR variable
# Donncha Mullin (d.mullin@ed.ac.uk)
# Following coding etiquette https://ourcodingclub.github.io/tutorials/etiquette/index.html
Sys.time()
Sys.Date()

# Set wd----
setwd("/Users/dmullin/Dropbox/Academic/PhD/LBC/mcr_project")

# Packages----
# install.packages in separate script
library(tidyverse) # formatting data for analysis (includes ggplot)
library(broom) # We use the tidy() function from library(broom) to get the variable(s) and specific values in a nice tibble:
library(DescTools) # descriptive statistics and exploratory data analysis
library(naniar) # replace_with_na function amongst others 
# try with foreign and try to account for NAs when reading in
library(finalfit) # glimpse, missing_glimpse, ff_glimpse
library(patchwork) # patching graphs together as one image
library(Hmisc)
library(corrplot)
library(rstatix)
library(lme4)


getOption("max.print") # determines what max print number is (default 1000 rows/observations)
options(max.print=3000)


# Load data----
RawLBCdata <- read_csv("LBC1936_GaitSpeedAndCognitiveImpairmentDec2021_DM_16DEC2021.csv")

# View column types
spec(RawLBCdata)


# make a copy of the data
MCRdata <-RawLBCdata
MCRdata

# Pre-processing of LBC data----
# create age in years variable (I explored the lubridate package but there was no suitable alternative using it). This will be needed in demographic tables later. 

MCRdata <- MCRdata %>% 
  mutate(ageyears_w1 = agedays_w1/365.25,
         ageyears_w2 = agedays_w2/365.25,
         ageyears_w3 = agedays_w3/365.25,
         ageyears_w4 = agedays_w4/365.25,
         ageyears_w5 = agedays_w5/365.25) #365.25 allows for leap year every 4 years

#check it worked okay
summary(MCRcombinedRecoded$ageyears_w1)
summary(MCRcombinedRecoded$ageyears_w2)
summary(MCRcombinedRecoded$ageyears_w3)
summary(MCRcombinedRecoded$ageyears_w4)
summary(MCRcombinedRecoded$ageyears_w5)

# check for duplicates in the unique identifier
MCRdata %>% 
  count(lbc36no, sort = TRUE) # sort will bring any lbc36no used more than once to the top of the list - there are none!

# First, list the various missing values used in LBC: 
na_values <- c(-777, -777.00, -888, -888.00, -999, -999.00, 
                -999.000, 999, 999.00, 999.000) 

# Secondly, list the other missing values used in some variables in LBC:
na_values_others <- c(88, 89, 98, 99, -88, -89, -98, -99)
# Used in the following variables: mht1947, ipip28_w1, ipip28_w3, ipip28_w5, memprob1_w3, memprob2_w3, memory1_w5, memory2_w5, wemwbs7_w3, wemwbs7_w4, wemwbs7_w5, phyactiv_w3, phyactiv_w4, phyactiv_w5, Days_significant_exercise_w3, Days_significant_exercise_w4, Days_significant_exercise_w5

# Use naniar package to convert this vector of values all to NA - https://cran.r-project.org/web/packages/naniar/vignettes/replace-with-na.html
MCRdata <- MCRdata %>% 
 replace_with_na_all(condition = ~.x %in% na_values)

# now replace those other missing variables in the specific columns in which they are used
MCRdata <- MCRdata %>% 
  replace_with_na(replace = list(mht1947 = c(88, 89, 98, 99, -88, -89, -98, -99), 
                                 ipip28_w1 = c(88, 89, 98, 99, -88, -89, -98, -99),
                                 ipip28_w3 = c(88, 89, 98, 99, -88, -89, -98, -99), 
                                 ipip28_w5 = c(88, 89, 98, 99, -88, -89, -98, -99),
                                 memprob1_w3 = c(88, 89, 98, 99, -88, -89, -98, -99),
                                 memprob2_w3 = c(88, 89, 98, 99, -88, -89, -98, -99),
                                 memory1_w5 = c(88, 89, 98, 99, -88, -89, -98, -99),
                                 memory2_w5 = c(88, 89, 98, 99, -88, -89, -98, -99),
                                 wemwbs7_w3 = c(88, 89, 98, 99, -88, -89, -98, -99),
                                 wemwbs7_w4 = c(88, 89, 98, 99, -88, -89, -98, -99),
                                 wemwbs7_w5 = c(88, 89, 98, 99, -88, -89, -98, -99),
                                 phyactiv_w3 = c(88, 89, 98, 99, -88, -89, -98, -99),
                                 phyactiv_w4 = c(88, 89, 98, 99, -88, -89, -98, -99),
                                 phyactiv_w5 = c(88, 89, 98, 99, -88, -89, -98, -99),
                                 Days_significant_exercise_w3 = c(88, 89, 98, 99, -88, -89, -98, -99),
                                 Days_significant_exercise_w4 = c(88, 89, 98, 99, -88, -89, -98, -99),
                                 Days_significant_exercise_w5 = c(88, 89, 98, 99, -88, -89, -98, -99),
                                 alcunitwk_w1 = c(-88, -89, -98, -99),
                                 alcunitwk_w2 = c(-88, -89, -98, -99),
                                 alcunitwk_w3 = c(-88, -89, -98, -99),
                                 alcunitwk_w4 = c(-88, -89, -98, -99),
                                 alcunitwk_w5 = c(-88, -89, -98, -99),
                                 bld_crprot_w3 = c(-1, -3),
                                 bld_crprot_w4 = c(-1, -3)
                                 ))


# check to see if this has worked
# initial na_values conversion check
summary(MCRdata$sixmwk_w1)
summary(MCRdata$sixmwk_w2)
summary(MCRdata$sixmwk_w3)
summary(MCRdata$sixmwk_w4)
summary(MCRdata$sixmwk_w5)

# second, na_values_others conversion check
summary(MCRdata$ipip28_w3)
summary(MCRdata$memprob1_w3)
summary(MCRdata$wemwbs7_w3) 

## exclude the 3 diagnosed w dementia by start of w3 (LBC361138, LBC361170, LBC360176) only run this when confirmed I am working with w3 MCRdata----
# MCRdata<-MCRdata[-c(954,975, 152), ]

#for future table of missing data need to establish those who attended each wave
Wave1attenders<- complete.cases(MCRdata$agedays_w1)
table(Wave1attenders)
Wave2attenders<- complete.cases(MCRdata$agedays_w2)
table(Wave2attenders)
Wave3attenders<- complete.cases(MCRdata$agedays_w3)
table(Wave3attenders)
Wave4attenders<- complete.cases(MCRdata$agedays_w4)
table(Wave4attenders)
Wave5attenders<- complete.cases(MCRdata$agedays_w5)
table(Wave5attenders)

# MCR Criteria ####
# to create MCR variable participants will need to meet 4 criteria: 
# 1. slow gait: first convert six metre walk speed, simwk_w1/2/3/4/5 etc to gaitspd in m/s then determined slow walkers as being >= 1SD slower than mean for each sex.

# 2. LBC contains three different measures of subjective cognitive complaint: 
# a) ipip28_w1/2/3/4/5 > 2 
# b) memprob1_w3/4/5 answer of 1 
# c) wemwbs7_w2 < 5 (Warwick Edinburgh "I've been thinking clearly" 1 = None of the time; 2 = Rarely; 3 = Some of the time; 4 = Often; 5 = All of the time - NOTE: wemwbs7_w3/4/5 0 = "none..." and 4 = "All of the time", so in these waves <4 is positive) i.e. we'd be taking all those who answer anything other than 'all of the time' as having SCC

# 3. independence with ADLs (adl_w1 <=1.5 SD greater than the age-matched mean (ie add 1.5 SD to the mean score at each wave, then take everyone below this)

# 4. no dementia  (dement_w1/2/3/4/5 == 0 AND score >=24 in MMSE)

# Inspect data and fix annoying variables, create gaitspd variable----
# inspecting gait variable - six metre walk time (seconds taken to do six metres)
summary(MCRdata$sixmwk_w1)
summary(MCRdata$sixmwk_w2)
summary(MCRdata$sixmwk_w3)
summary(MCRdata$sixmwk_w4)
summary(MCRdata$sixmwk_w5)

# w1 has impossible speed value (1.05 sec for 6m = 21km/h)
hist(MCRdata$sixmwk_w1)

# replace impossible min value with NA
MCRdata <- MCRdata %>% replace_with_na(replace = list(sixmwk_w1= 1.050))

# w3 has impossible speed value (6 metres in .40 sec = 54km/h)
boxplot(MCRdata$sixmwk_w3)

# replace impossible min value with NA
MCRdata <- MCRdata %>% replace_with_na(replace = list(sixmwk_w3= 0.45))

# create speed in m/s variable

# mutate new column gaitspd by dividing 6 by sixmwk_w1 (to calculate speed in metres/sec), name it gaitspd (note, dividing sixmwk_w1 by 6 gives seconds per metre - not what I want)
MCRdata <- MCRdata %>% 
  mutate(gaitspd_w1 = 6/sixmwk_w1, 
         gaitspd_w2 = 6/sixmwk_w2,
         gaitspd_w3 = 6/sixmwk_w3,
         gaitspd_w4 = 6/sixmwk_w4,
         gaitspd_w5 = 6/sixmwk_w5)

# Visualise outliers and use $out call to print the outlier values, then save this as vector "outliers..." https://www.r-bloggers.com/2020/01/how-to-remove-outliers-in-r/  Don't necessarily remove these outliers, only remove obvious errors/impossible speeds. This method identifies outliers using the IQR - The interquartile range is the central 50% or the area between the 75th and the 25th percentile of a distribution. A point is an outlier if it is above the 75th or below the 25th percentile by a factor of 1.5 times the IQR.
# outliers_gaitspd_w1 <- boxplot(MCRdata$gaitspd_w1)$out
# outliers_gaitspd_w2 <- boxplot(MCRdata$gaitspd_w2)$out
# outliers_gaitspd_w3 <- boxplot(MCRdata$gaitspd_w3)$out
# outliers_gaitspd_w4 <- boxplot(MCRdata$gaitspd_w4)$out
# outliers_gaitspd_w5 <- boxplot(MCRdata$gaitspd_w5)$out

# Have a look at the new gaitspd column in MCRdata tibble - examine these to see outliers. 
summary(MCRdata$gaitspd_w1)
summary(MCRdata$gaitspd_w2)
summary(MCRdata$gaitspd_w3)
summary(MCRdata$gaitspd_w4)
summary(MCRdata$gaitspd_w5)


# STEP 1 Create male and female cohorts #####
# Quick overview of each sex's summed gaitspd_w1 for example
# MCRdata %>% 
# group_by(sex) %>% 
# summarise(sum(gaitspd_w1, na.rm = TRUE))

# males
lbc_male <- MCRdata %>% 
  filter(sex == 1)

# Females
lbc_female <- MCRdata %>% 
  filter(sex == 2)

# Create list of wave attenders for each sex
## Male ####
wave1attenders_male <- complete.cases(lbc_male$agedays_w1)
table(wave1attenders_male)
wave2attenders_male <- complete.cases(lbc_male$agedays_w2)
table(wave2attenders_male)
wave3attenders_male <- complete.cases(lbc_male$agedays_w3)
table(wave3attenders_male)
wave4attenders_male <- complete.cases(lbc_male$agedays_w4)
table(wave4attenders_male)
wave5attenders_male <- complete.cases(lbc_male$agedays_w5)
table(wave5attenders_male)

## Females ####
wave1attenders_female <- complete.cases(lbc_female$agedays_w1)
table(wave1attenders_female)
wave2attenders_female <- complete.cases(lbc_female$agedays_w2)
table(wave2attenders_female)
wave3attenders_female <- complete.cases(lbc_female$agedays_w3)
table(wave3attenders_female)
wave4attenders_female <- complete.cases(lbc_female$agedays_w4)
table(wave4attenders_female)
wave5attenders_female <- complete.cases(lbc_female$agedays_w5)
table(wave5attenders_female)



# STEP 2 Create slow gait variable ##########
# determine slow gait cutoff for each group at each wave (Slow gait definition: >= 1SD below (ie slower than) the sex and age-matched mean)
## Males ####
# 2.1.1 Calculate mean gaitspd(m/s) 
mean_gait_male_w1 <- lbc_male$gaitspd_w1 %>% 
  mean(na.rm = TRUE)
mean_gait_male_w2 <- lbc_male$gaitspd_w2 %>% 
  mean(na.rm = TRUE)
mean_gait_male_w3 <- lbc_male$gaitspd_w3 %>% 
  mean(na.rm = TRUE)
mean_gait_male_w4 <- lbc_male$gaitspd_w4 %>% 
  mean(na.rm = TRUE)
mean_gait_male_w5 <- lbc_male$gaitspd_w5 %>% 
  mean(na.rm = TRUE)

# 2.1.2 Calculate SD gaitspd(m/s) 
sd_gait_male_w1 <- lbc_male$gaitspd_w1 %>% 
  sd(na.rm = TRUE)
sd_gait_male_w2 <- lbc_male$gaitspd_w2 %>% 
  sd(na.rm = TRUE)
sd_gait_male_w3 <- lbc_male$gaitspd_w3 %>% 
  sd(na.rm = TRUE)
sd_gait_male_w4 <- lbc_male$gaitspd_w4 %>% 
  sd(na.rm = TRUE)
sd_gait_male_w5 <- lbc_male$gaitspd_w5 %>% 
  sd(na.rm = TRUE)

# 2.1.3 Calculate >=1SD slower than average for gaitspd(m/s) 
slow_gait_cutoff_male_w1 <- lbc_male$gaitspd_w1 %>% 
  mean(na.rm = TRUE) - sd(lbc_male$gaitspd_w1, na.rm = TRUE) # = 1.3564837326 m/s
slow_gait_cutoff_male_w2 <- lbc_male$gaitspd_w2 %>% 
  mean(na.rm = TRUE) - sd(lbc_male$gaitspd_w2, na.rm = TRUE)
slow_gait_cutoff_male_w3 <- lbc_male$gaitspd_w3 %>% 
  mean(na.rm = TRUE) - sd(lbc_male$gaitspd_w3, na.rm = TRUE)
slow_gait_cutoff_male_w4 <- lbc_male$gaitspd_w4 %>% 
  mean(na.rm = TRUE) - sd(lbc_male$gaitspd_w4, na.rm = TRUE)
slow_gait_cutoff_male_w5 <- lbc_male$gaitspd_w5 %>% 
  mean(na.rm = TRUE) - sd(lbc_male$gaitspd_w5, na.rm = TRUE)

# 2.1.4 Create new slow_male variable using if_else statement and mutate it to tibble
lbc_male <- lbc_male %>% 
  mutate(slow_male_w1 = if_else(gaitspd_w1 <= slow_gait_cutoff_male_w1, 1,0))
table(lbc_male$slow_male_w1) # slow_male_w1 n = 71 
lbc_male <- lbc_male %>% 
  mutate(slow_male_w2 = if_else(gaitspd_w2 <= slow_gait_cutoff_male_w2, 1,0))
table(lbc_male$slow_male_w2) # slow_male_w2 n = 52 
lbc_male <- lbc_male %>% 
  mutate(slow_male_w3 = if_else(gaitspd_w3 <= slow_gait_cutoff_male_w3, 1,0))
table(lbc_male$slow_male_w3) # slow_male_w3 n = 55 
lbc_male <- lbc_male %>% 
  mutate(slow_male_w4 = if_else(gaitspd_w4 <= slow_gait_cutoff_male_w4, 1,0))
table(lbc_male$slow_male_w4) # slow_male_w4 n = 36
lbc_male <- lbc_male %>% 
  mutate(slow_male_w5 = if_else(gaitspd_w5 <= slow_gait_cutoff_male_w5, 1,0))
table(lbc_male$slow_male_w5) # slow_male_w5 n = 28 

## Females ####
# 2.2.1 Calculate mean gaitspd(m/s) Females
mean_gait_female_w1 <- lbc_female$gaitspd_w1 %>% 
  mean(na.rm = TRUE)
mean_gait_female_w2 <- lbc_female$gaitspd_w2 %>% 
  mean(na.rm = TRUE)
mean_gait_female_w3 <- lbc_female$gaitspd_w3 %>% 
  mean(na.rm = TRUE)
mean_gait_female_w4 <- lbc_female$gaitspd_w4 %>% 
  mean(na.rm = TRUE)
mean_gait_female_w5 <- lbc_female$gaitspd_w5 %>% 
  mean(na.rm = TRUE)

# 2.2.2 Calculate SD gaitspd(m/s) Females
sd_gait_female_w1 <- lbc_female$gaitspd_w1 %>% 
  sd(na.rm = TRUE)
sd_gait_female_w2 <- lbc_female$gaitspd_w2 %>% 
  sd(na.rm = TRUE)
sd_gait_female_w3 <- lbc_female$gaitspd_w3 %>% 
  sd(na.rm = TRUE)
sd_gait_female_w4 <- lbc_female$gaitspd_w4 %>% 
  sd(na.rm = TRUE)
sd_gait_female_w5 <- lbc_female$gaitspd_w5 %>% 
  sd(na.rm = TRUE)

# 2.2.3 Calculate >=1SD slower than average for gaitspd(m/s) males
slow_gait_cutoff_female_w1 <- lbc_female$gaitspd_w1 %>% 
  mean(na.rm = TRUE) - sd(lbc_female$gaitspd_w1, na.rm = TRUE) # = 1.3564837326 m/s
slow_gait_cutoff_female_w2 <- lbc_female$gaitspd_w2 %>% 
  mean(na.rm = TRUE) - sd(lbc_female$gaitspd_w2, na.rm = TRUE)
slow_gait_cutoff_female_w3 <- lbc_female$gaitspd_w3 %>% 
  mean(na.rm = TRUE) - sd(lbc_female$gaitspd_w3, na.rm = TRUE)
slow_gait_cutoff_female_w4 <- lbc_female$gaitspd_w4 %>% 
  mean(na.rm = TRUE) - sd(lbc_female$gaitspd_w4, na.rm = TRUE)
slow_gait_cutoff_female_w5 <- lbc_female$gaitspd_w5 %>% 
  mean(na.rm = TRUE) - sd(lbc_female$gaitspd_w5, na.rm = TRUE)

# 2.2.4 Create new slow female variable using if_else statement and mutate it to tibble
lbc_female <- lbc_female %>% 
  mutate(slow_female_w1 = if_else(gaitspd_w1 <= slow_gait_cutoff_female_w1, 1,0))
table(lbc_female$slow_female_w1) # slow_female_w1 n = 79
lbc_female <- lbc_female %>% 
  mutate(slow_female_w2 = if_else(gaitspd_w2 <= slow_gait_cutoff_female_w2, 1,0))
table(lbc_female$slow_female_w2) # slow_female_w2 n = 59 
lbc_female <- lbc_female %>% 
  mutate(slow_female_w3 = if_else(gaitspd_w3 <= slow_gait_cutoff_female_w3, 1,0))
table(lbc_female$slow_female_w3) # slow_female_w3 n = 57 
lbc_female <- lbc_female %>% 
  mutate(slow_female_w4 = if_else(gaitspd_w4 <= slow_gait_cutoff_female_w4, 1,0))
table(lbc_female$slow_female_w4) # slow_female_w4 n = 44
lbc_female <- lbc_female %>% 
  mutate(slow_female_w5 = if_else(gaitspd_w5 <= slow_gait_cutoff_female_w5, 1,0))
table(lbc_female$slow_female_w5) # slow_female_w5 n = 35 




# STEP 3 Create SCC variables ####
# create SCC cohorts for each sex (SCC definition: ipip28_w1/2/3/4/5 > 2 OR wemwbs7_w2 < 5 wemwbs7/3/4/5 < 4 (see notes under MCR criteria for explanation) OR memprob1_w3/4/5 == 1)

#### Males ####
# First explore each SCC variable, including NAs. sum() adds up the total n with specified value of the variable: 
# 3.1.1 ipip28_w1/2/3/4/5 variable
table(lbc_male$ipip28_w1)
sum(lbc_male$ipip28_w1 >2, na.rm = TRUE) # positive n = 148
summary(lbc_male$ipip28_w1) # NAs n = 76

table(lbc_male$ipip28_w2)  
sum(lbc_male$ipip28_w2 >2, na.rm = TRUE) # positive n = 134
summary(lbc_male$ipip28_w2) # NAs n = 104

table(lbc_male$ipip28_w3) 
sum(lbc_male$ipip28_w3 >2, na.rm = TRUE) # positive n = 95
summary(lbc_male$ipip28_w3) # NAs n = 197

table(lbc_male$ipip28_w4) 
sum(lbc_male$ipip28_w4 >2, na.rm = TRUE) # positive n = 64
summary(lbc_male$ipip28_w4) # NAs n = 280

table(lbc_male$ipip28_w5) 
sum(lbc_male$ipip28_w5 >2, na.rm = TRUE) # positive n = 69
summary(lbc_male$ipip28_w5) # NAs n = 345

#  3.1.2 wemwbs7_w2/3/4/5 variable
table(lbc_male$wemwbs7_w2) 
sum(lbc_male$wemwbs7_w2 < 5, na.rm = TRUE) # positive n = 354
summary(lbc_male$wemwbs7_w2) # NAs n = 105

table(lbc_male$wemwbs7_w3) 
sum(lbc_male$wemwbs7_w3 < 4, na.rm = TRUE) # positive n = 280
summary(lbc_male$wemwbs7_w3) # NAs n = 196

table(lbc_male$wemwbs7_w4)  
sum(lbc_male$wemwbs7_w4 < 4, na.rm = TRUE) # positive n = 212
summary(lbc_male$wemwbs7_w4) # NAs n = 278

table(lbc_male$wemwbs7_w5) 
sum(lbc_male$wemwbs7_w5 < 4, na.rm = TRUE) # positive n = 162
summary(lbc_male$wemwbs7_w5) # NAs n = 345

#  3.1.3 memprob1_w3/4/5 variable
table(lbc_male$memprob1_w3) # positive n = 185
summary(lbc_male$memprob1_w3) # NAs n = 197
table(lbc_male$memory1_w4) # positive n = 161
summary(lbc_male$memory1_w4) # NAs n = 280
table(lbc_male$memory1_w5) # positive n = 137
summary(lbc_male$memory1_w5) # NAs n = 346


# 3.2 mutate males SCC variables - new column for each SCC type at each wave
# 3.2.1 ipip28_w1/2/3/4/5 variable
lbc_male <- lbc_male %>% 
  mutate(scc_male_ipip28_w1 = if_else(ipip28_w1 > 2, 1, 0)) %>% 
  mutate(scc_male_ipip28_w2 = if_else(ipip28_w2 > 2, 1, 0)) %>% 
  mutate(scc_male_ipip28_w3 = if_else(ipip28_w3 > 2, 1, 0)) %>% 
  mutate(scc_male_ipip28_w4 = if_else(ipip28_w4 > 2, 1, 0)) %>% 
  mutate(scc_male_ipip28_w5 = if_else(ipip28_w5 > 2, 1, 0))

# 3.2.2 wemwbs7_w2/3/4/5 variable
lbc_male <- lbc_male %>% 
  mutate(scc_male_wemwbs7_w2 = if_else(wemwbs7_w2 < 5, 1, 0)) %>% 
  mutate(scc_male_wemwbs7_w3 = if_else(wemwbs7_w3 < 4, 1, 0)) %>% 
  mutate(scc_male_wemwbs7_w4 = if_else(wemwbs7_w4 < 4, 1, 0)) %>% 
  mutate(scc_male_wemwbs7_w5 = if_else(wemwbs7_w5 < 4, 1, 0)) 

# 3.2.3 memprob1_w3/4/5 variable
lbc_male <- lbc_male %>% 
  mutate(scc_male_memprob1_w3 = if_else(memprob1_w3 == 1, 1, 0)) %>% 
  mutate(scc_male_memprob1_w4 = if_else(memory1_w4 == 1, 1, 0)) %>%
  mutate(scc_male_memprob1_w5 = if_else(memory1_w5 == 1, 1, 0))
  
# Explore how many males have both slow gait and SCC depending on SCC variable 
# ipip28_w1/2/3/4/5 variable
lbc_male %>% 
  filter(lbc_male$scc_male_ipip28_w1 == 1 & lbc_male$slow_male_w1 == 1) # (n = 18/545 males)

lbc_male %>% 
  filter(lbc_male$scc_male_ipip28_w2 == 1 & lbc_male$slow_male_w2 == 1) # (n = 15/445 males)

lbc_male %>% 
  filter(lbc_male$scc_male_ipip28_w3 == 1 & lbc_male$slow_male_w3 == 1) # (n = 18/357 males)

lbc_male %>% 
  filter(lbc_male$scc_male_ipip28_w4 == 1 & lbc_male$slow_male_w4 == 1) # (n = 12/272 males) 

lbc_male %>% 
  filter(lbc_male$scc_male_ipip28_w5 == 1 & lbc_male$slow_male_w5 == 1) # (n = 13/208 males)

# wemwbs7_w2/3/4/5 variable
lbc_male %>% 
  filter(lbc_male$scc_male_wemwbs7_w2 == 1 & lbc_male$slow_male_w2 == 1) # (n = 41/445 males)

lbc_male %>% 
  filter(lbc_male$scc_male_wemwbs7_w3 == 1 & lbc_male$slow_male_w3 == 1) # (n = 45/357 males)

lbc_male %>% 
  filter(lbc_male$scc_male_wemwbs7_w4 == 1 & lbc_male$slow_male_w4 == 1) # (n = 30/272 males)

lbc_male %>% 
  filter(lbc_male$scc_male_wemwbs7_w5 == 1 & lbc_male$slow_male_w5 == 1) # (n = 23/208 males)

# memprob1_w3/4/5 variable
lbc_male %>% 
  filter(lbc_male$scc_male_memprob1_w3 == 1 & lbc_male$slow_male_w3 == 1) # (n = 36/357 males)

lbc_male %>% 
  filter(lbc_male$scc_male_memprob1_w4 == 1 & lbc_male$slow_male_w4 == 1) # (n = 27/272 males)

lbc_male %>% 
  filter(lbc_male$scc_male_memprob1_w5 == 1 & lbc_male$slow_male_w5 == 1) # (n = 17/208 males)

# create scc_any for each wave
# w1 just has the ipip28_w1 variable so take that combination with slow gait if using wave 1. 
scc_any_male_w1 <- lbc_male$scc_male_ipip28_w1
table(scc_any_male_w1)

table(lbc_male$scc_male_ipip28_w3, lbc_male$scc_male_memprob1_w3)

# w2 has ipip28_w2 and wemwbs7_w2
scc_any_male_w2 <- lbc_male$scc_male_ipip28_w2 + lbc_male$scc_male_wemwbs7_w2
table(scc_any_male_w2)

# w3/4/5 have all three
scc_any_male_w3 <- lbc_male$scc_male_ipip28_w3 + lbc_male$scc_male_wemwbs7_w3 + lbc_male$scc_male_memprob1_w3
table(scc_any_male_w3)

scc_any_male_w4 <- lbc_male$scc_male_ipip28_w4 + lbc_male$scc_male_wemwbs7_w4 + lbc_male$scc_male_memprob1_w4
table(scc_any_male_w4)

scc_any_male_w5 <- lbc_male$scc_male_ipip28_w5 + lbc_male$scc_male_wemwbs7_w5 + lbc_male$scc_male_memprob1_w5
table(scc_any_male_w5)

# Mutate a positive or negative for any SCC column
lbc_male <- lbc_male %>% 
  mutate(scc_any_male_w1 = if_else(scc_any_male_w1 >= 1, 1, 0)) %>% 
  mutate(scc_any_male_w2 = if_else(scc_any_male_w2 >= 1, 1, 0)) %>% 
  mutate(scc_any_male_w3 = if_else(scc_any_male_w3 >= 1, 1, 0)) %>% 
  mutate(scc_any_male_w4 = if_else(scc_any_male_w4 >= 1, 1, 0)) %>% 
  mutate(scc_any_male_w5 = if_else(scc_any_male_w5 >= 1, 1, 0))

# Tabulate scc_any variable
table(lbc_male$scc_any_male_w1)
table(lbc_male$scc_any_male_w2)
table(lbc_male$scc_any_male_w3)
table(lbc_male$scc_any_male_w4)
table(lbc_male$scc_any_male_w5)

# Explore combination of scc_any variable with slow gait
lbc_male %>% 
  filter(lbc_male$scc_any_male_w1 == 1 & lbc_male$slow_male_w1 == 1) # (n = 18/545 males) 

lbc_male %>% 
  filter(lbc_male$scc_any_male_w2 == 1 & lbc_male$slow_male_w2 == 1) # (n = 43/445 males) 

lbc_male %>% 
  filter(lbc_male$scc_any_male_w3 == 1 & lbc_male$slow_male_w3 == 1)  # (n = 52/357 males)

lbc_male %>% 
  filter(lbc_male$scc_any_male_w4 == 1 & lbc_male$slow_male_w4 == 1)  # (n = 33/272 males)

lbc_male %>% 
  filter(lbc_male$scc_any_male_w5 == 1 & lbc_male$slow_male_w5 == 1)  # (n = 27/208 males)
  

#### Females ####
# First explore each SCC variable, including NAs. The function sum() adds up the total n with specified value of the variable: 
# 3.1.1 ipip28_w1/2/3/4/5 variable
table(lbc_female$ipip28_w1)
sum(lbc_female$ipip28_w1 >2, na.rm = TRUE) # positive n = 116
summary(lbc_female$ipip28_w1) # NAs n = 54

table(lbc_female$ipip28_w2)  
sum(lbc_female$ipip28_w2 >2, na.rm = TRUE) # positive n = 91
summary(lbc_female$ipip28_w2) # NAs n = 132

table(lbc_female$ipip28_w3) 
sum(lbc_female$ipip28_w3 >2, na.rm = TRUE) # positive n = 67
summary(lbc_female$ipip28_w3) # NAs n = 212

table(lbc_female$ipip28_w4) 
sum(lbc_female$ipip28_w4 >2, na.rm = TRUE) # positive n = 67
summary(lbc_female$ipip28_w4) # NAs n = 278

table(lbc_female$ipip28_w5) 
sum(lbc_female$ipip28_w5 >2, na.rm = TRUE) # positive n = 49
summary(lbc_female$ipip28_w5) # NAs n = 328

#  3.1.2 wemwbs7_w2/3/4/5 variable
table(lbc_female$wemwbs7_w2) 
sum(lbc_female$wemwbs7_w2 < 5, na.rm = TRUE) # positive n = 312
summary(lbc_female$wemwbs7_w2) # NAs n = 132

table(lbc_female$wemwbs7_w3) 
sum(lbc_female$wemwbs7_w3 < 4, na.rm = TRUE) # positive n = 253
summary(lbc_female$wemwbs7_w3) # NAs n = 212

table(lbc_female$wemwbs7_w4)  
sum(lbc_female$wemwbs7_w4 < 4, na.rm = TRUE) # positive n = 195
summary(lbc_female$wemwbs7_w4) # NAs n = 276

table(lbc_female$wemwbs7_w5) 
sum(lbc_female$wemwbs7_w5 < 4, na.rm = TRUE) # positive n = 155
summary(lbc_female$wemwbs7_w5) # NAs n = 328

#  3.1.3 memprob1_w3/4/5 variable
table(lbc_female$memprob1_w3) # positive n = 186
summary(lbc_female$memprob1_w3) # NAs n = 212
table(lbc_female$memory1_w4) # positive n = 151
summary(lbc_female$memory1_w4) # NAs n = 280
table(lbc_female$memory1_w5) # positive n = 121
summary(lbc_female$memory1_w5) # NAs n = 327


# 3.2 mutate females SCC variables - new column for each SCC type at each wave
# 3.2.1 ipip28_w1/2/3/4/5 variable
lbc_female <- lbc_female %>% 
  mutate(scc_female_ipip28_w1 = if_else(ipip28_w1 > 2, 1, 0)) %>% 
  mutate(scc_female_ipip28_w2 = if_else(ipip28_w2 > 2, 1, 0)) %>% 
  mutate(scc_female_ipip28_w3 = if_else(ipip28_w3 > 2, 1, 0)) %>% 
  mutate(scc_female_ipip28_w4 = if_else(ipip28_w4 > 2, 1, 0)) %>% 
  mutate(scc_female_ipip28_w5 = if_else(ipip28_w5 > 2, 1, 0))

# 3.2.2 wemwbs7_w2/3/4/5 variable
lbc_female <- lbc_female %>% 
  mutate(scc_female_wemwbs7_w2 = if_else(wemwbs7_w2 < 5, 1, 0)) %>% 
  mutate(scc_female_wemwbs7_w3 = if_else(wemwbs7_w3 < 4, 1, 0)) %>% 
  mutate(scc_female_wemwbs7_w4 = if_else(wemwbs7_w4 < 4, 1, 0)) %>% 
  mutate(scc_female_wemwbs7_w5 = if_else(wemwbs7_w5 < 4, 1, 0)) 

# 3.2.3 memprob1_w3/4/5 variable
lbc_female <- lbc_female %>% 
  mutate(scc_female_memprob1_w3 = if_else(memprob1_w3 == 1, 1, 0)) %>% 
  mutate(scc_female_memprob1_w4 = if_else(memory1_w4 == 1, 1, 0)) %>% 
  mutate(scc_female_memprob1_w5 = if_else(memory1_w5 == 1, 1, 0))


# Explore how many females have both slow gait and SCC depending on SCC variable 
# ipip28_w1/2/3/4/5 variable

lbc_female %>% 
  filter(lbc_female$scc_female_ipip28_w1 == 1 & lbc_female$slow_female_w1 == 1) # (n = 23/538 females)

lbc_female %>% 
  filter(lbc_female$scc_female_ipip28_w2 == 1 & lbc_female$slow_female_w2 == 1) # (n = 20/413 females)

lbc_female %>% 
  filter(lbc_female$scc_female_ipip28_w3 == 1 & lbc_female$slow_female_w3 == 1) # (n = 18/332 females)

lbc_female %>% 
  filter(lbc_female$scc_female_ipip28_w4 == 1 & lbc_female$slow_female_w4 == 1) # (n = 18/270 females) 

lbc_female %>% 
  filter(lbc_female$scc_female_ipip28_w5 == 1 & lbc_female$slow_female_w5 == 1) # (n = 9/219 females)

# wemwbs7_w2/3/4/5 variable
lbc_female %>% 
  filter(lbc_female$scc_female_wemwbs7_w2 == 1 & lbc_female$slow_female_w2 == 1) # (n = 51/413 females)

lbc_female %>% 
  filter(lbc_female$scc_female_wemwbs7_w3 == 1 & lbc_female$slow_female_w3 == 1) # (n = 46/332 females)

lbc_female %>% 
  filter(lbc_female$scc_female_wemwbs7_w4 == 1 & lbc_female$slow_female_w4 == 1) # (n = 30/270 females)

lbc_female %>% 
  filter(lbc_female$scc_female_wemwbs7_w5 == 1 & lbc_female$slow_female_w5 == 1) # (n = 27/219 females)

# memprob1_w3/4/5 variable
lbc_female %>% 
  filter(lbc_female$scc_female_memprob1_w3 == 1 & lbc_female$slow_female_w3 == 1) # (n = 33/332 females)

lbc_female %>% 
  filter(lbc_female$scc_female_memprob1_w4 == 1 & lbc_female$slow_female_w4 == 1) # (n = 24/270 females)

lbc_female %>% 
  filter(lbc_female$scc_female_memprob1_w5 == 1 & lbc_female$slow_female_w5 == 1) # (n = 22/219 females)

# create scc_any for each wave
# w1 just has the ipip28_w1 variable so take that combination with slow gait if using wave 1. 
scc_any_female_w1 <- lbc_female$scc_female_ipip28_w1
table(scc_any_female_w1)

# w2 has ipip28_w2 and wemwbs7_w2
scc_any_female_w2 <- lbc_female$scc_female_ipip28_w2 + lbc_female$scc_female_wemwbs7_w2
table(scc_any_female_w2)

# w3/4/5 have all three
scc_any_female_w3 <- lbc_female$scc_female_ipip28_w3 + lbc_female$scc_female_wemwbs7_w3 + lbc_female$scc_female_memprob1_w3
table(scc_any_female_w3)

scc_any_female_w4 <- lbc_female$scc_female_ipip28_w4 + lbc_female$scc_female_wemwbs7_w4 + lbc_female$scc_female_memprob1_w4
table(scc_any_female_w4)

scc_any_female_w5 <- lbc_female$scc_female_ipip28_w5 + lbc_female$scc_female_wemwbs7_w5 + lbc_female$scc_female_memprob1_w5
table(scc_any_female_w5)

# Mutate a positive or negative for any SCC column
lbc_female <- lbc_female %>% 
  mutate(scc_any_female_w1 = if_else(scc_any_female_w1 >= 1, 1, 0)) %>% 
  mutate(scc_any_female_w2 = if_else(scc_any_female_w2 >= 1, 1, 0)) %>% 
  mutate(scc_any_female_w3 = if_else(scc_any_female_w3 >= 1, 1, 0)) %>% 
  mutate(scc_any_female_w4 = if_else(scc_any_female_w4 >= 1, 1, 0)) %>% 
  mutate(scc_any_female_w5 = if_else(scc_any_female_w5 >= 1, 1, 0))

# Tabulate scc_any variable
table(lbc_female$scc_any_female_w1)
table(lbc_female$scc_any_female_w2)
table(lbc_female$scc_any_female_w3)
table(lbc_female$scc_any_female_w4)
table(lbc_female$scc_any_female_w5)

# Combine scc_any variables with slow gait
lbc_female %>% 
  filter(lbc_female$scc_any_female_w1 == 1 & lbc_female$slow_female_w1 == 1) # (n = 23/538 females) 

lbc_female %>% 
  filter(lbc_female$scc_any_female_w2 == 1 & lbc_female$slow_female_w2 == 1) # (n = 53/413 females) 

lbc_female %>% 
  filter(lbc_female$scc_any_female_w3 == 1 & lbc_female$slow_female_w3 == 1)  # (n = 48/332 females)

lbc_female %>% 
  filter(lbc_female$scc_any_female_w4 == 1 & lbc_female$slow_female_w4 == 1)  # (n = 35/270 females)

lbc_female %>% 
  filter(lbc_female$scc_any_female_w5 == 1 & lbc_female$slow_female_w5 == 1)  # (n = 28/219 females)





# STEP 4 Create the 'no dementia' variable #####
# Definition of no dementia = dement_w1 == 0 AND mmse_w1 >= 24
## Males####
# First explore these variables 
summary(lbc_male$dement_w1) # NAs = 0 and 0 with dementia
summary(lbc_male$mmse_w1) # NAs = 0 
summary(lbc_male$mmse_w1 < 24) # n=17 males with MMSE < 24

summary(lbc_male$dement_w2)
sum(lbc_male$dement_w2, na.rm = TRUE)# NAs = 100 and 2 with dementia
summary(lbc_male$mmse_w2) # NAs = 100 
summary(lbc_male$mmse_w2 < 24) # n=11 males with MMSE < 24

summary(lbc_male$dement_w3) # NAs = 188 and 5 with dementia
sum(lbc_male$dement_w3, na.rm = TRUE)
summary(lbc_male$mmse_w3) # NAs = 188 
summary(lbc_male$mmse_w3 < 24) # n=12 males with MMSE < 24

summary(lbc_male$dement_w4) # NAs = 278 and 3 with dementia
sum(lbc_male$dement_w4, na.rm = TRUE)
summary(lbc_male$mmse_w4) # NAs = 273
summary(lbc_male$mmse_w4 < 24) # n=13 males with MMSE < 24

# NOTE - dementia variable in w5 has a name change
summary(lbc_male$dementia_w5) # NAs = 339 and 6 with dementia
sum(lbc_male$dementia_w5, na.rm = TRUE)
summary(lbc_male$mmse_w5) # NAs = 337 
summary(lbc_male$mmse_w5 < 24) # n=15 males with MMSE < 24


# Create the males with "No Dementia" variable

lbc_male <- lbc_male %>% 
  mutate(no_dementia_w1 = if_else(dement_w1 == 0 & mmse_w1 >= 24, 1, 0))
table(lbc_male$no_dementia_w1) # (n = 9/545 with dementia, i.e exclude those with 0 for this variable)

lbc_male <- lbc_male %>% 
  mutate(no_dementia_w2 = if_else(dement_w2 == 0 & mmse_w2 >= 24, 1, 0))
table(lbc_male$no_dementia_w2) # n = 7/445

lbc_male <- lbc_male %>% 
  mutate(no_dementia_w3 = if_else(dement_w3 == 0 & mmse_w3 >= 24, 1, 0))
table(lbc_male$no_dementia_w3) # n = 13/357

lbc_male <- lbc_male %>% 
  mutate(no_dementia_w4 = if_else(dement_w4 == 0 & mmse_w4 >= 24, 1, 0))
table(lbc_male$no_dementia_w4) # n = 9/272

lbc_male <- lbc_male %>% 
  mutate(no_dementia_w5 = if_else(dementia_w5 == 0 & mmse_w5 >= 24, 1, 0))
table(lbc_male$no_dementia_w5) # n = 11/208

## Female----
# First explore these variables
summary(lbc_female$dement_w1) # NAs = 0 and 0 with dementia
summary(lbc_female$mmse_w1) # NAs = 1 
summary(lbc_female$mmse_w1 < 24) # n=2 females with MMSE <= 24

summary(lbc_female$dement_w2)
sum(lbc_female$dement_w2, na.rm = TRUE)# NAs = 125 and 0 with dementia
summary(lbc_female$mmse_w2) # NAs = 126
summary(lbc_female$mmse_w2 < 24) # n=1 females with MMSE <= 24

summary(lbc_female$dement_w3) # NAs = 206 and 4 with dementia
sum(lbc_female$dement_w3, na.rm = TRUE)
summary(lbc_female$mmse_w3) # NAs = 206
summary(lbc_female$mmse_w3 < 24) # n=3 females with MMSE <= 24

summary(lbc_female$dement_w4) # NAs = 270 and 3 with dementia
sum(lbc_female$dement_w4, na.rm = TRUE)
summary(lbc_female$mmse_w4) # NAs = 269
summary(lbc_female$mmse_w4 < 24) # n=7 females with MMSE <= 24

# NOTE - dementia variable in w5 has a name change
summary(lbc_female$dementia_w5) # NAs = 321 and 6 with dementia
sum(lbc_female$dementia_w5, na.rm = TRUE)
summary(lbc_female$mmse_w5) # NAs = 322 
summary(lbc_female$mmse_w5 < 24) # n=3 females with MMSE <= 24



# Females with No Dementia
lbc_female <- lbc_female %>% 
  mutate(no_dementia_w1 = if_else(dement_w1 == 0 & mmse_w1 >= 24, 1, 0))
table(lbc_female$no_dementia_w1) # (n = 2/538 with dementia, i.e exclude those with 0 for this variable)

lbc_female <- lbc_female %>% 
  mutate(no_dementia_w2 = if_else(dement_w2 == 0 & mmse_w2 >= 24, 1, 0))
table(lbc_female$no_dementia_w2) # n = 1/413

lbc_female <- lbc_female %>% 
  mutate(no_dementia_w3 = if_else(dement_w3 == 0 & mmse_w3 >= 24, 1, 0))
table(lbc_female$no_dementia_w3) # n = 5/332

lbc_female <- lbc_female %>% 
  mutate(no_dementia_w4 = if_else(dement_w4 == 0 & mmse_w4 >= 24, 1, 0))
table(lbc_female$no_dementia_w4) # n = 9/270

lbc_female <- lbc_female %>% 
  mutate(no_dementia_w5 = if_else(dementia_w5 == 0 & mmse_w5 >= 24, 1, 0))
table(lbc_female$no_dementia_w5) # n = 6/219

# explore how many males have slow gait AND any_scc AND no_dementia 
lbc_male %>% 
  filter(lbc_male$scc_any_male_w1 == 1 & lbc_male$slow_male_w1 == 1 & lbc_male$no_dementia_w1 == 1) # n = 18
lbc_male %>% 
  filter(lbc_male$scc_any_male_w2 == 1 & lbc_male$slow_male_w2 == 1 & lbc_male$no_dementia_w2 == 1) # n = 41
lbc_male %>% 
  filter(lbc_male$scc_any_male_w3 == 1 & lbc_male$slow_male_w3 == 1 & lbc_male$no_dementia_w3 == 1) # n = 46
lbc_male %>% 
  filter(lbc_male$scc_any_male_w4 == 1 & lbc_male$slow_male_w4 == 1 & lbc_male$no_dementia_w4 == 1) # n = 28
lbc_male %>% 
  filter(lbc_male$scc_any_male_w5 == 1 & lbc_male$slow_male_w5 == 1 & lbc_male$no_dementia_w5 == 1) # n = 22

# explore how many Females have slow gait AND any_scc AND no_dementia 
lbc_female %>% 
  filter(lbc_female$scc_any_female_w1 == 1 & lbc_female$slow_female_w1 == 1 & lbc_female$no_dementia_w1 == 1) # n = 22
lbc_female %>% 
  filter(lbc_female$scc_any_female_w2 == 1 & lbc_female$slow_female_w2 == 1 & lbc_female$no_dementia_w2 == 1) # n = 52
lbc_female %>% 
  filter(lbc_female$scc_any_female_w3 == 1 & lbc_female$slow_female_w3 == 1 & lbc_female$no_dementia_w3 == 1) # n = 47
lbc_female %>% 
  filter(lbc_female$scc_any_female_w4 == 1 & lbc_female$slow_female_w4 == 1 & lbc_female$no_dementia_w4 == 1) # n = 31
lbc_female %>% 
  filter(lbc_female$scc_any_female_w5 == 1 & lbc_female$slow_female_w5 == 1 & lbc_female$no_dementia_w5 == 1) # n = 25


#STEP 5 Create 'independent w ADL' variable #####
# Definition of 'No significant functional impairment': (adl_w1 <=1.5 SD greater than the age-matched mean (ie add 1.5 SD to the mean score at each wave, then take everyone below this) remember, higher score equals greater disability. 
# First explore these variables 
summary(lbc_male$adl_w1) # NAs = 1
summary(lbc_male$adl_w2) # NAs = 100
summary(lbc_male$adl_w3) # NAs = 188
summary(lbc_male$adl_w4) # NAs = 273
summary(lbc_male$adl_w5) # NAs = 339

summary(lbc_female$adl_w1) # NAs = 1
summary(lbc_female$adl_w2) # NAs = 126
summary(lbc_female$adl_w3) # NAs = 207
summary(lbc_female$adl_w4) # NAs = 270
summary(lbc_female$adl_w5) # NAs = 321

## Male####

# visualise adl_w1 males
hist(lbc_male$adl_w1)

# 5.1.1 Calculate mean adl_w1/2/3/4/5 males
mean_adl_male_w1 <- lbc_male$adl_w1 %>% 
  mean(na.rm = TRUE)
mean_adl_male_w2 <- lbc_male$adl_w2 %>% 
  mean(na.rm = TRUE)
mean_adl_male_w3 <- lbc_male$adl_w3 %>% 
  mean(na.rm = TRUE)
mean_adl_male_w4 <- lbc_male$adl_w4 %>% 
  mean(na.rm = TRUE)
mean_adl_male_w5 <- lbc_male$adl_w5 %>% 
  mean(na.rm = TRUE)
# 5.1.2 Calculate 1.5SD adl_w1 males
sd1.5_adl_male_w1 <- lbc_male$adl_w1 %>% 
  sd(na.rm = TRUE) *1.5
sd1.5_adl_male_w2 <- lbc_male$adl_w2 %>% 
  sd(na.rm = TRUE) *1.5
sd1.5_adl_male_w3 <- lbc_male$adl_w3 %>% 
  sd(na.rm = TRUE) *1.5
sd1.5_adl_male_w4 <- lbc_male$adl_w4 %>% 
  sd(na.rm = TRUE) *1.5
sd1.5_adl_male_w5 <- lbc_male$adl_w5 %>% 
  sd(na.rm = TRUE) *1.5

# 5.1.3 Calculate >=1.5SD worse (higher) than average for adl_w1 males (ADD 1.5SD to mean)
adl_w1_cutoff_male <- mean_adl_male_w1 + sd1.5_adl_male_w1
adl_w2_cutoff_male <- mean_adl_male_w2 + sd1.5_adl_male_w2
adl_w3_cutoff_male <- mean_adl_male_w3 + sd1.5_adl_male_w3
adl_w4_cutoff_male <- mean_adl_male_w4 + sd1.5_adl_male_w4
adl_w5_cutoff_male <- mean_adl_male_w5 + sd1.5_adl_male_w5

# 5.1.4 Create new Independent ADL variable using if_else statement and mutate it to tibble
lbc_male <- lbc_male %>% 
  mutate(independ_male_w1 = if_else(adl_w1 <= adl_w1_cutoff_male, 1,0))
table(lbc_male$independ_male_w1) # 32/544 score 0 ie are NOT independent / score higher than 1.5SD above mean) 

lbc_male <- lbc_male %>% 
  mutate(independ_male_w2 = if_else(adl_w2 <= adl_w2_cutoff_male, 1,0))
table(lbc_male$independ_male_w2) # n = 25, who are NOT independent

lbc_male <- lbc_male %>% 
  mutate(independ_male_w3 = if_else(adl_w3 <= adl_w3_cutoff_male, 1,0))
table(lbc_male$independ_male_w3) # n = 36, who are NOT independent

lbc_male <- lbc_male %>% 
  mutate(independ_male_w4 = if_else(adl_w4 <= adl_w4_cutoff_male, 1,0))
table(lbc_male$independ_male_w4) # n = 21, who are NOT independent

lbc_male <- lbc_male %>% 
  mutate(independ_male_w5 = if_else(adl_w5 <= adl_w5_cutoff_male, 1,0))
table(lbc_male$independ_male_w5) # n = 17, who are NOT independent


## Females ####

# 5.2.1 Calculate mean adl_w1/2/3/4/5 females
mean_adl_female_w1 <- lbc_female$adl_w1 %>% 
  mean(na.rm = TRUE)
mean_adl_female_w2 <- lbc_female$adl_w2 %>% 
  mean(na.rm = TRUE)
mean_adl_female_w3 <- lbc_female$adl_w3 %>% 
  mean(na.rm = TRUE)
mean_adl_female_w4 <- lbc_female$adl_w4 %>% 
  mean(na.rm = TRUE)
mean_adl_female_w5 <- lbc_female$adl_w5 %>% 
  mean(na.rm = TRUE)

# 5.2.2 Calculate 1.5SD adl_w1 females
sd1.5_adl_female_w1 <- lbc_female$adl_w1 %>% 
  sd(na.rm = TRUE) *1.5
sd1.5_adl_female_w2 <- lbc_female$adl_w2 %>% 
  sd(na.rm = TRUE) *1.5
sd1.5_adl_female_w3 <- lbc_female$adl_w3 %>% 
  sd(na.rm = TRUE) *1.5
sd1.5_adl_female_w4 <- lbc_female$adl_w4 %>% 
  sd(na.rm = TRUE) *1.5
sd1.5_adl_female_w5 <- lbc_female$adl_w5 %>% 
  sd(na.rm = TRUE) *1.5

# 5.2.3 Calculate >=1.5SD worse (higher) than average for adl_w1 females (ADD 1.5SD to mean)
adl_w1_cutoff_female <- mean_adl_female_w1 + sd1.5_adl_female_w1
adl_w2_cutoff_female <- mean_adl_female_w2 + sd1.5_adl_female_w2
adl_w3_cutoff_female <- mean_adl_female_w3 + sd1.5_adl_female_w3
adl_w4_cutoff_female <- mean_adl_female_w4 + sd1.5_adl_female_w4
adl_w5_cutoff_female <- mean_adl_female_w5 + sd1.5_adl_female_w5

# 5.1.4 Create new Independent ADL variable using if_else statement and mutate it to tibble
lbc_female <- lbc_female %>% 
  mutate(independ_female_w1 = if_else(adl_w1 <= adl_w1_cutoff_female, 1,0))
table(lbc_female$independ_female_w1) # 48 score 0 ie are NOT independent / score higher than 1.5SD above mean) 

lbc_female <- lbc_female %>% 
  mutate(independ_female_w2 = if_else(adl_w2 <= adl_w2_cutoff_female, 1,0))
table(lbc_female$independ_female_w2) # n = 30, who are NOT independent

lbc_female <- lbc_female %>% 
  mutate(independ_female_w3 = if_else(adl_w3 <= adl_w3_cutoff_female, 1,0))
table(lbc_female$independ_female_w3) # n = 31, who are NOT independent

lbc_female <- lbc_female %>% 
  mutate(independ_female_w4 = if_else(adl_w4 <= adl_w4_cutoff_female, 1,0))
table(lbc_female$independ_female_w4) # n = 21, who are NOT independent

lbc_female <- lbc_female %>% 
  mutate(independ_female_w5 = if_else(adl_w5 <= adl_w5_cutoff_female, 1,0))
table(lbc_female$independ_female_w5) # n = 19, who are NOT independent

# STEP 6 - create MCR---- 
#If an individual meets each of these four criteria then they are classified as having MCR.
#using a simple categorisation system: MCR = 1, Non-MCR=0 
## Male ----
# explore how many males at each wave have slow gait & any SCC & No Dementia & are Independent (ie have MCR) 
lbc_male %>% 
  filter(lbc_male$scc_any_male_w1 == 1 & lbc_male$slow_male_w1 == 1 & lbc_male$no_dementia_w1 == 1 & lbc_male$independ_male_w1 == 1) # (n = 14)

lbc_male %>% 
  filter(lbc_male$scc_any_male_w2 == 1 & lbc_male$slow_male_w2 == 1 & lbc_male$no_dementia_w2 == 1 & lbc_male$independ_male_w2 == 1) # (n = 28)

lbc_male %>% 
  filter(lbc_male$scc_any_male_w3 == 1 & lbc_male$slow_male_w3 == 1 & lbc_male$no_dementia_w3 == 1 & lbc_male$independ_male_w3 == 1) # (n = 28)

lbc_male %>% 
  filter(lbc_male$scc_any_male_w4 == 1 & lbc_male$slow_male_w4 == 1 & lbc_male$no_dementia_w4 == 1 & lbc_male$independ_male_w4 == 1) # (n = 17)

lbc_male %>% 
  filter(lbc_male$scc_any_male_w5 == 1 & lbc_male$slow_male_w5 == 1 & lbc_male$no_dementia_w5 == 1 & lbc_male$independ_male_w5 == 1) # (n = 14)

# Mutate MCR_male_anyscc column, count number of cases and calculate prevalence
lbc_male <- lbc_male %>% 
  mutate(mcr_male_anyscc_w1 = if_else(lbc_male$scc_any_male_w1 == 1 & lbc_male$slow_male_w1 == 1 & lbc_male$no_dementia_w1 == 1 & lbc_male$independ_male_w1 == 1, 1, 0))
sum(lbc_male$mcr_male_anyscc_w1, na.rm = TRUE)
(sum(lbc_male$mcr_male_anyscc_w1, na.rm = TRUE)/sum(wave1attenders_male)) * 100 

lbc_male <- lbc_male %>% 
  mutate(mcr_male_anyscc_w2 = if_else(lbc_male$scc_any_male_w2 == 1 & lbc_male$slow_male_w2 == 1 & lbc_male$no_dementia_w2 == 1 & lbc_male$independ_male_w2 == 1, 1, 0))
sum(lbc_male$mcr_male_anyscc_w2, na.rm = TRUE)
(sum(lbc_male$mcr_male_anyscc_w2, na.rm = TRUE)/sum(wave2attenders_male)) * 100 

lbc_male <- lbc_male %>% 
  mutate(mcr_male_anyscc_w3 = if_else(lbc_male$scc_any_male_w3 == 1 & lbc_male$slow_male_w3 == 1 & lbc_male$no_dementia_w3 == 1 & lbc_male$independ_male_w3 == 1, 1, 0))
sum(lbc_male$mcr_male_anyscc_w3, na.rm = TRUE)
(sum(lbc_male$mcr_male_anyscc_w3, na.rm = TRUE)/sum(wave3attenders_male)) * 100

lbc_male <- lbc_male %>% 
  mutate(mcr_male_anyscc_w4 = if_else(lbc_male$scc_any_male_w4 == 1 & lbc_male$slow_male_w4 == 1 & lbc_male$no_dementia_w4 == 1 & lbc_male$independ_male_w4 == 1, 1, 0))
sum(lbc_male$mcr_male_anyscc_w4, na.rm = TRUE)
(sum(lbc_male$mcr_male_anyscc_w4, na.rm = TRUE)/sum(wave4attenders_male)) * 100

lbc_male <- lbc_male %>% 
  mutate(mcr_male_anyscc_w5 = if_else(lbc_male$scc_any_male_w5 == 1 & lbc_male$slow_male_w5 == 1 & lbc_male$no_dementia_w5 == 1 & lbc_male$independ_male_w5 == 1, 1, 0))
sum(lbc_male$mcr_male_anyscc_w5, na.rm = TRUE)
(sum(lbc_male$mcr_male_anyscc_w5, na.rm = TRUE)/sum(wave5attenders_male)) * 100

# Mutate MCR_male_memprob1 column - only starts from wave 3. Count number of cases and calculate prevalence

lbc_male <- lbc_male %>% 
  mutate(mcr_male_memprob1_w3 = if_else(lbc_male$scc_male_memprob1_w3 == 1 & lbc_male$slow_male_w3 == 1 & lbc_male$no_dementia_w3 == 1 & lbc_male$independ_male_w3 == 1, 1, 0))
sum(lbc_male$mcr_male_memprob1_w3, na.rm = TRUE)
(sum(lbc_male$mcr_male_memprob1_w3, na.rm = TRUE)/sum(wave3attenders_male)) * 100

lbc_male <- lbc_male %>% 
  mutate(mcr_male_memprob1_w4 = if_else(lbc_male$scc_male_memprob1_w4 == 1 & lbc_male$slow_male_w4 == 1 & lbc_male$no_dementia_w4 == 1 & lbc_male$independ_male_w4 == 1, 1, 0))
sum(lbc_male$mcr_male_memprob1_w4, na.rm = TRUE)
(sum(lbc_male$mcr_male_memprob1_w4, na.rm = TRUE)/sum(wave4attenders_male)) * 100

lbc_male <- lbc_male %>% 
  mutate(mcr_male_memprob1_w5 = if_else(lbc_male$scc_male_memprob1_w5 == 1 & lbc_male$slow_male_w5 == 1 & lbc_male$no_dementia_w5 == 1 & lbc_male$independ_male_w5 == 1, 1, 0))
sum(lbc_male$mcr_male_memprob1_w5, na.rm = TRUE)
(sum(lbc_male$mcr_male_memprob1_w5, na.rm = TRUE)/sum(wave5attenders_male)) * 100

# Mutate MCR_male_ipip28 column, count number of cases and calculate prevalence
lbc_male <- lbc_male %>% 
  mutate(mcr_male_ipip28_w1 = if_else(lbc_male$scc_male_ipip28_w1 == 1 & lbc_male$slow_male_w1 == 1 & lbc_male$no_dementia_w1 == 1 & lbc_male$independ_male_w1 == 1, 1, 0))
sum(lbc_male$mcr_male_ipip28_w1, na.rm = TRUE)
(sum(lbc_male$mcr_male_ipip28_w1, na.rm = TRUE)/sum(wave1attenders_male)) * 100

lbc_male <- lbc_male %>% 
  mutate(mcr_male_ipip28_w2 = if_else(lbc_male$scc_male_ipip28_w2 == 1 & lbc_male$slow_male_w2 == 1 & lbc_male$no_dementia_w2 == 1 & lbc_male$independ_male_w2 == 1, 1, 0))
sum(lbc_male$mcr_male_ipip28_w2, na.rm = TRUE)
(sum(lbc_male$mcr_male_ipip28_w2, na.rm = TRUE)/sum(wave2attenders_male)) * 100

lbc_male <- lbc_male %>% 
  mutate(mcr_male_ipip28_w3 = if_else(lbc_male$scc_male_ipip28_w3 == 1 & lbc_male$slow_male_w3 == 1 & lbc_male$no_dementia_w3 == 1 & lbc_male$independ_male_w3 == 1, 1, 0))
sum(lbc_male$mcr_male_ipip28_w3, na.rm = TRUE)
(sum(lbc_male$mcr_male_ipip28_w3, na.rm = TRUE)/sum(wave3attenders_male)) * 100

lbc_male <- lbc_male %>% 
  mutate(mcr_male_ipip28_w4 = if_else(lbc_male$scc_male_ipip28_w4 == 1 & lbc_male$slow_male_w4 == 1 & lbc_male$no_dementia_w4 == 1 & lbc_male$independ_male_w4 == 1, 1, 0))
sum(lbc_male$mcr_male_ipip28_w4, na.rm = TRUE)
(sum(lbc_male$mcr_male_ipip28_w4, na.rm = TRUE)/sum(wave4attenders_male)) * 100

lbc_male <- lbc_male %>% 
  mutate(mcr_male_ipip28_w5 = if_else(lbc_male$scc_male_ipip28_w5 == 1 & lbc_male$slow_male_w5 == 1 & lbc_male$no_dementia_w5 == 1 & lbc_male$independ_male_w5 == 1, 1, 0))
sum(lbc_male$mcr_male_ipip28_w5, na.rm = TRUE)
(sum(lbc_male$mcr_male_ipip28_w5, na.rm = TRUE)/sum(wave5attenders_male)) * 100

# Mutate MCR_male_wembs7 column - only starts from wave 2. Count number of cases and calculate prevalence
lbc_male <- lbc_male %>% 
  mutate(mcr_male_wemwbs7_w2 = if_else(lbc_male$scc_male_wemwbs7_w2 == 1 & lbc_male$slow_male_w2 == 1 & lbc_male$no_dementia_w2 == 1 & lbc_male$independ_male_w2 == 1, 1, 0))
sum(lbc_male$mcr_male_wemwbs7_w2, na.rm = TRUE)
(sum(lbc_male$mcr_male_wemwbs7_w2, na.rm = TRUE)/sum(wave2attenders_male)) * 100

lbc_male <- lbc_male %>% 
  mutate(mcr_male_wemwbs7_w3 = if_else(lbc_male$scc_male_wemwbs7_w3 == 1 & lbc_male$slow_male_w3 == 1 & lbc_male$no_dementia_w3 == 1 & lbc_male$independ_male_w3 == 1, 1, 0))
sum(lbc_male$mcr_male_wemwbs7_w3, na.rm = TRUE)
(sum(lbc_male$mcr_male_wemwbs7_w3, na.rm = TRUE)/sum(wave3attenders_male)) * 100

lbc_male <- lbc_male %>% 
  mutate(mcr_male_wemwbs7_w4 = if_else(lbc_male$scc_male_wemwbs7_w4 == 1 & lbc_male$slow_male_w4 == 1 & lbc_male$no_dementia_w4 == 1 & lbc_male$independ_male_w4 == 1, 1, 0))
sum(lbc_male$mcr_male_wemwbs7_w4, na.rm = TRUE)
(sum(lbc_male$mcr_male_wemwbs7_w4, na.rm = TRUE)/sum(wave4attenders_male)) * 100

lbc_male <- lbc_male %>% 
  mutate(mcr_male_wemwbs7_w5 = if_else(lbc_male$scc_male_wemwbs7_w5 == 1 & lbc_male$slow_male_w5 == 1 & lbc_male$no_dementia_w5 == 1 & lbc_male$independ_male_w5 == 1, 1, 0))
sum(lbc_male$mcr_male_wemwbs7_w5, na.rm = TRUE)
(sum(lbc_male$mcr_male_wemwbs7_w5, na.rm = TRUE)/sum(wave5attenders_male)) * 100

# Save lbc_male as it's own tibble
write_csv(lbc_male, file = "lbc_male.csv")

## Female ----
# Mutate MCR_female_anyscc column, Count number of cases and calculate prevalence
lbc_female <- lbc_female %>% 
  mutate(mcr_female_anyscc_w1 = if_else(lbc_female$scc_any_female_w1 == 1 & lbc_female$slow_female_w1 == 1 & lbc_female$no_dementia_w1 == 1 & lbc_female$independ_female_w1 == 1, 1, 0))
sum(lbc_female$mcr_female_anyscc_w1, na.rm = TRUE)
(sum(lbc_female$mcr_female_anyscc_w1, na.rm = TRUE)/sum(wave1attenders_female)) * 100

lbc_female <- lbc_female %>% 
  mutate(mcr_female_anyscc_w2 = if_else(lbc_female$scc_any_female_w2 == 1 & lbc_female$slow_female_w2 == 1 & lbc_female$no_dementia_w2 == 1 & lbc_female$independ_female_w2 == 1, 1, 0))
sum(lbc_female$mcr_female_anyscc_w2, na.rm = TRUE)
(sum(lbc_female$mcr_female_anyscc_w2, na.rm = TRUE)/sum(wave2attenders_female)) * 100

lbc_female <- lbc_female %>% 
  mutate(mcr_female_anyscc_w3 = if_else(lbc_female$scc_any_female_w3 == 1 & lbc_female$slow_female_w3 == 1 & lbc_female$no_dementia_w3 == 1 & lbc_female$independ_female_w3 == 1, 1, 0))
sum(lbc_female$mcr_female_anyscc_w3, na.rm = TRUE)
(sum(lbc_female$mcr_female_anyscc_w3, na.rm = TRUE)/sum(wave3attenders_female)) * 100

lbc_female <- lbc_female %>% 
  mutate(mcr_female_anyscc_w4 = if_else(lbc_female$scc_any_female_w4 == 1 & lbc_female$slow_female_w4 == 1 & lbc_female$no_dementia_w4 == 1 & lbc_female$independ_female_w4 == 1, 1, 0))
sum(lbc_female$mcr_female_anyscc_w4, na.rm = TRUE)
(sum(lbc_female$mcr_female_anyscc_w4, na.rm = TRUE)/sum(wave4attenders_female)) * 100

lbc_female <- lbc_female %>% 
  mutate(mcr_female_anyscc_w5 = if_else(lbc_female$scc_any_female_w5 == 1 & lbc_female$slow_female_w5 == 1 & lbc_female$no_dementia_w5 == 1 & lbc_female$independ_female_w5 == 1, 1, 0))
sum(lbc_female$mcr_female_anyscc_w5, na.rm = TRUE)
(sum(lbc_female$mcr_female_anyscc_w5, na.rm = TRUE)/sum(wave5attenders_female)) * 100

# Mutate MCR_female_memprob1 column - only starts from wave 3. Count number of cases and calculate prevalence

lbc_female <- lbc_female %>% 
  mutate(mcr_female_memprob1_w3 = if_else(lbc_female$scc_female_memprob1_w3 == 1 & lbc_female$slow_female_w3 == 1 & lbc_female$no_dementia_w3 == 1 & lbc_female$independ_female_w3 == 1, 1, 0))
sum(lbc_female$mcr_female_memprob1_w3, na.rm = TRUE)
(sum(lbc_female$mcr_female_memprob1_w3, na.rm = TRUE)/sum(wave3attenders_female)) * 100

lbc_female <- lbc_female %>% 
  mutate(mcr_female_memprob1_w4 = if_else(lbc_female$scc_female_memprob1_w4 == 1 & lbc_female$slow_female_w4 == 1 & lbc_female$no_dementia_w4 == 1 & lbc_female$independ_female_w4 == 1, 1, 0))
sum(lbc_female$mcr_female_memprob1_w4, na.rm = TRUE)
(sum(lbc_female$mcr_female_memprob1_w4, na.rm = TRUE)/sum(wave4attenders_female)) * 100

lbc_female <- lbc_female %>% 
  mutate(mcr_female_memprob1_w5 = if_else(lbc_female$scc_female_memprob1_w5 == 1 & lbc_female$slow_female_w5 == 1 & lbc_female$no_dementia_w5 == 1 & lbc_female$independ_female_w5 == 1, 1, 0))
sum(lbc_female$mcr_female_memprob1_w5, na.rm = TRUE)
(sum(lbc_female$mcr_female_memprob1_w5, na.rm = TRUE)/sum(wave5attenders_female)) * 100

# Mutate MCR_female_ipip28 column, count number of cases and calculate prevalence
lbc_female <- lbc_female %>% 
  mutate(mcr_female_ipip28_w1 = if_else(lbc_female$scc_female_ipip28_w1 == 1 & lbc_female$slow_female_w1 == 1 & lbc_female$no_dementia_w1 == 1 & lbc_female$independ_female_w1 == 1, 1, 0))
sum(lbc_female$mcr_female_ipip28_w1, na.rm = TRUE)
(sum(lbc_female$mcr_female_ipip28_w1, na.rm = TRUE)/sum(wave1attenders_female)) * 100

lbc_female <- lbc_female %>% 
  mutate(mcr_female_ipip28_w2 = if_else(lbc_female$scc_female_ipip28_w2 == 1 & lbc_female$slow_female_w2 == 1 & lbc_female$no_dementia_w2 == 1 & lbc_female$independ_female_w2 == 1, 1, 0))
sum(lbc_female$mcr_female_ipip28_w2, na.rm = TRUE)
(sum(lbc_female$mcr_female_ipip28_w2, na.rm = TRUE)/sum(wave2attenders_female)) * 100

lbc_female <- lbc_female %>% 
  mutate(mcr_female_ipip28_w3 = if_else(lbc_female$scc_female_ipip28_w3 == 1 & lbc_female$slow_female_w3 == 1 & lbc_female$no_dementia_w3 == 1 & lbc_female$independ_female_w3 == 1, 1, 0))
sum(lbc_female$mcr_female_ipip28_w3, na.rm = TRUE)
(sum(lbc_female$mcr_female_ipip28_w3, na.rm = TRUE)/sum(wave3attenders_female)) * 100

lbc_female <- lbc_female %>% 
  mutate(mcr_female_ipip28_w4 = if_else(lbc_female$scc_female_ipip28_w4 == 1 & lbc_female$slow_female_w4 == 1 & lbc_female$no_dementia_w4 == 1 & lbc_female$independ_female_w4 == 1, 1, 0))
sum(lbc_female$mcr_female_ipip28_w4, na.rm = TRUE)
(sum(lbc_female$mcr_female_ipip28_w4, na.rm = TRUE)/sum(wave4attenders_female)) * 100

lbc_female <- lbc_female %>% 
  mutate(mcr_female_ipip28_w5 = if_else(lbc_female$scc_female_ipip28_w5 == 1 & lbc_female$slow_female_w5 == 1 & lbc_female$no_dementia_w5 == 1 & lbc_female$independ_female_w5 == 1, 1, 0))
sum(lbc_female$mcr_female_ipip28_w5, na.rm = TRUE)
(sum(lbc_female$mcr_female_ipip28_w5, na.rm = TRUE)/sum(wave5attenders_female)) * 100

# Mutate MCR_female_wembs7 column - only starts from wave 2, count number of cases and calculate prevalence
lbc_female <- lbc_female %>% 
  mutate(mcr_female_wemwbs7_w2 = if_else(lbc_female$scc_female_wemwbs7_w2 == 1 & lbc_female$slow_female_w2 == 1 & lbc_female$no_dementia_w2 == 1 & lbc_female$independ_female_w2 == 1, 1, 0))
sum(lbc_female$mcr_female_wemwbs7_w2, na.rm = TRUE)
(sum(lbc_female$mcr_female_wemwbs7_w2, na.rm = TRUE)/sum(wave2attenders_female)) * 100

lbc_female <- lbc_female %>% 
  mutate(mcr_female_wemwbs7_w3 = if_else(lbc_female$scc_female_wemwbs7_w3 == 1 & lbc_female$slow_female_w3 == 1 & lbc_female$no_dementia_w3 == 1 & lbc_female$independ_female_w3 == 1, 1, 0))
sum(lbc_female$mcr_female_wemwbs7_w3, na.rm = TRUE)
(sum(lbc_female$mcr_female_wemwbs7_w3, na.rm = TRUE)/sum(wave3attenders_female)) * 100

lbc_female <- lbc_female %>% 
  mutate(mcr_female_wemwbs7_w4 = if_else(lbc_female$scc_female_wemwbs7_w4 == 1 & lbc_female$slow_female_w4 == 1 & lbc_female$no_dementia_w4 == 1 & lbc_female$independ_female_w4 == 1, 1, 0))
sum(lbc_female$mcr_female_wemwbs7_w4, na.rm = TRUE)
(sum(lbc_female$mcr_female_wemwbs7_w4, na.rm = TRUE)/sum(wave4attenders_female)) * 100

lbc_female <- lbc_female %>% 
  mutate(mcr_female_wemwbs7_w5 = if_else(lbc_female$scc_female_wemwbs7_w5 == 1 & lbc_female$slow_female_w5 == 1 & lbc_female$no_dementia_w5 == 1 & lbc_female$independ_female_w5 == 1, 1, 0))
sum(lbc_female$mcr_female_wemwbs7_w5, na.rm = TRUE)
(sum(lbc_female$mcr_female_wemwbs7_w5, na.rm = TRUE)/sum(wave5attenders_female)) * 100

# Save lbc_female as it's own tibble
write_csv(lbc_female, file = "lbc_female.csv")


# STEP 7 Rejoin male and female tibbles----
MCRcombined <- full_join(lbc_male, lbc_female)

# Create MCR regardless of sex----
# Issue with using if_else() is it only keeps the positive cases, recording all else as NA (loses the 0s from each gender)
#MCRcombined <- MCRcombined %>% 
# mutate(mcr_mem_w3 = if_else
# (MCRcombined$mcr_male_memprob1_w3 == 1 | 
# MCRcombined$mcr_female_memprob1_w3 ==1, 1, 0))
# table(MCRcombined$mcr_mem_w3)

# Instead, requires using case_when() to allow me to vectorise multiple if and else statements... 

#MCRmemprob
MCRcombined <- 
  MCRcombined %>% 
  mutate(mcr_mem_w3 = case_when(
    mcr_male_memprob1_w3 == 1 |
      mcr_female_memprob1_w3 == 1 ~ 1,
    mcr_male_memprob1_w3 == 0 | mcr_female_memprob1_w3 == 0 ~ 0,
    mcr_male_memprob1_w3 == NA | mcr_female_memprob1_w3 == NA ~ NA_real_)) # # All RHS values need to be of the same type. Inconsistent types will throw an error.
# This applies also to NA values used in RHS: NA is logical (whereas my LHS values are numeric), use
# typed values like NA_real_, NA_complex, NA_character_, NA_integer_ as appropriate

table(MCRcombined$mcr_mem_w3)
summary(MCRcombined$mcr_mem_w3)



MCRcombined <- 
  MCRcombined %>% 
  mutate(mcr_mem_w4 = case_when(
    mcr_male_memprob1_w4 == 1 |
      mcr_female_memprob1_w4 == 1 ~ 1,
    mcr_male_memprob1_w4 == 0 | mcr_female_memprob1_w4 == 0 ~ 0,
    mcr_male_memprob1_w4 == NA | mcr_female_memprob1_w4 == NA ~ NA_real_))

MCRcombined <- 
  MCRcombined %>% 
  mutate(mcr_mem_w5 = case_when(
    mcr_male_memprob1_w5 == 1 |
      mcr_female_memprob1_w5 == 1 ~ 1,
    mcr_male_memprob1_w5 == 0 | mcr_female_memprob1_w5 == 0 ~ 0,
    mcr_male_memprob1_w5 == NA | mcr_female_memprob1_w5 == NA ~ NA_real_))

# MCRipip
MCRcombined <- 
  MCRcombined %>% 
  mutate(mcr_ipip_w1 = case_when(
    mcr_male_ipip28_w1 == 1 |
      mcr_female_ipip28_w1 == 1 ~ 1,
    mcr_male_ipip28_w1 == 0 | mcr_female_ipip28_w1 == 0 ~ 0,
    mcr_male_ipip28_w1 == NA | mcr_female_ipip28_w1 == NA ~ NA_real_))

MCRcombined <- 
  MCRcombined %>% 
  mutate(mcr_ipip_w2 = case_when(
    mcr_male_ipip28_w2 == 1 |
      mcr_female_ipip28_w2 == 1 ~ 1,
    mcr_male_ipip28_w2 == 0 | mcr_female_ipip28_w2 == 0 ~ 0,
    mcr_male_ipip28_w2 == NA | mcr_female_ipip28_w2 == NA ~ NA_real_))

MCRcombined <- 
  MCRcombined %>% 
  mutate(mcr_ipip_w3 = case_when(
    mcr_male_ipip28_w3 == 1 |
      mcr_female_ipip28_w3 == 1 ~ 1,
    mcr_male_ipip28_w3 == 0 | mcr_female_ipip28_w3 == 0 ~ 0,
    mcr_male_ipip28_w3 == NA | mcr_female_ipip28_w3 == NA ~ NA_real_))

MCRcombined <- 
  MCRcombined %>% 
  mutate(mcr_ipip_w4 = case_when(
    mcr_male_ipip28_w4 == 1 |
      mcr_female_ipip28_w4 == 1 ~ 1,
    mcr_male_ipip28_w4 == 0 | mcr_female_ipip28_w4 == 0 ~ 0,
    mcr_male_ipip28_w4 == NA | mcr_female_ipip28_w4 == NA ~ NA_real_))

MCRcombined <- 
  MCRcombined %>% 
  mutate(mcr_ipip_w5 = case_when(
    mcr_male_ipip28_w5 == 1 |
      mcr_female_ipip28_w5 == 1 ~ 1,
    mcr_male_ipip28_w5 == 0 | mcr_female_ipip28_w5 == 0 ~ 0,
    mcr_male_ipip28_w5 == NA | mcr_female_ipip28_w5 == NA ~ NA_real_))

# MCRwem

MCRcombined <- MCRcombined %>% 
  mutate(mcr_wem_w2 = case_when(
    mcr_male_wemwbs7_w2 == 1 |
      mcr_female_wemwbs7_w2 == 1 ~ 1,
    mcr_male_wemwbs7_w2 == 0 | mcr_female_wemwbs7_w2 == 0 ~ 0,
    mcr_male_wemwbs7_w2 == NA | mcr_female_wemwbs7_w2 == NA ~ NA_real_))

MCRcombined <- MCRcombined %>% 
  mutate(mcr_wem_w3 = case_when(
    mcr_male_wemwbs7_w3 == 1 |
      mcr_female_wemwbs7_w3 == 1 ~ 1,
    mcr_male_wemwbs7_w3 == 0 | mcr_female_wemwbs7_w3 == 0 ~ 0,
    mcr_male_wemwbs7_w3 == NA | mcr_female_wemwbs7_w3 == NA ~ NA_real_))

MCRcombined <- MCRcombined %>% 
  mutate(mcr_wem_w4 = case_when(
    mcr_male_wemwbs7_w4 == 1 |
      mcr_female_wemwbs7_w4 == 1 ~ 1,
    mcr_male_wemwbs7_w4 == 0 | mcr_female_wemwbs7_w4 == 0 ~ 0,
    mcr_male_wemwbs7_w4 == NA | mcr_female_wemwbs7_w4 == NA ~ NA_real_))

MCRcombined <- MCRcombined %>% 
  mutate(mcr_wem_w5 = case_when(
    mcr_male_wemwbs7_w5 == 1 |
      mcr_female_wemwbs7_w5 == 1 ~ 1,
    mcr_male_wemwbs7_w5 == 0 | mcr_female_wemwbs7_w5 == 0 ~ 0,
    mcr_male_wemwbs7_w5 == NA | mcr_female_wemwbs7_w5 == NA ~ NA_real_))

#MCRany
MCRcombined <- MCRcombined %>% 
  mutate(mcr_any_w1 = case_when(
    mcr_male_anyscc_w1 == 1 |
      mcr_female_anyscc_w1 == 1 ~ 1,
    mcr_male_anyscc_w1 == 0 | mcr_female_anyscc_w1 == 0 ~ 0,
    mcr_male_anyscc_w1 == NA | mcr_female_anyscc_w1 == NA ~ NA_real_))

MCRcombined <- MCRcombined %>% 
  mutate(mcr_any_w2 = case_when(
    mcr_male_anyscc_w2 == 1 |
      mcr_female_anyscc_w2 == 1 ~ 1,
    mcr_male_anyscc_w2 == 0 | mcr_female_anyscc_w2 == 0 ~ 0,
    mcr_male_anyscc_w2 == NA | mcr_female_anyscc_w2 == NA ~ NA_real_))

MCRcombined <- MCRcombined %>% 
  mutate(mcr_any_w3 = case_when(
    mcr_male_anyscc_w3 == 1 |
      mcr_female_anyscc_w3 == 1 ~ 1,
    mcr_male_anyscc_w3 == 0 | mcr_female_anyscc_w3 == 0 ~ 0,
    mcr_male_anyscc_w3 == NA | mcr_female_anyscc_w3 == NA ~ NA_real_))

MCRcombined <- MCRcombined %>% 
  mutate(mcr_any_w4 = case_when(
    mcr_male_anyscc_w4 == 1 |
      mcr_female_anyscc_w4 == 1 ~ 1,
    mcr_male_anyscc_w4 == 0 | mcr_female_anyscc_w4 == 0 ~ 0,
    mcr_male_anyscc_w4 == NA | mcr_female_anyscc_w4 == NA ~ NA_real_))

MCRcombined <- MCRcombined %>% 
  mutate(mcr_any_w5 = case_when(
    mcr_male_anyscc_w5 == 1 |
      mcr_female_anyscc_w5 == 1 ~ 1,
    mcr_male_anyscc_w5 == 0 | mcr_female_anyscc_w5 == 0 ~ 0,
    mcr_male_anyscc_w5 == NA | mcr_female_anyscc_w5 == NA ~ NA_real_))

# Save MCRcombined as it's own processed dataset ----
write_csv(MCRcombined, file = "MCRcombined.csv")



# CIs for MCR proportions----
# Initially I am only doing the rest for the MCRmem variable, not the other MCR subtypes
# first check MCR positive Vs negatives
table(MCRcombined$mcr_mem_w3)
table(MCRcombined$mcr_mem_w4)
table(MCRcombined$mcr_mem_w5)


# MCRmem_w3
# note, the second number should equal total no. of 'trials' (i.e. participants) rather than no of zeros. 
binom.test(39, 654+39,
           0.5,
           alternative = "two.sided",
           conf.level = 0.95)

# MCRmem_w4
binom.test(29, 517+29,
           0.5,
           alternative = "two.sided",
           conf.level = 0.95)

# MCRmem_w5
binom.test(24, 404+24,
           0.5,
           alternative = "two.sided",
           conf.level = 0.95)


# explore MCR numbers for COMPLETERS of all waves

MCRcombined$attrition <- complete.cases(MCRcombined$agedays_w5)
MCRcombined$attrition <- as.numeric(MCRcombined$attrition)
MCRcombined$attrition <- factor(MCRcombined$attrition, levels = c(0,1), labels = c("Withdrawer", "Completer"))

table(MCRcombined$mcr_mem_w3, data = MCRcombined$attrition)
table(MCRcombined$mcr_mem_w4, data = MCRcombined$attrition)
table(MCRcombined$mcr_mem_w5, data = MCRcombined$attrition)

## Difference between COMPLETERS W3 and W5---- 
# For how to create the matrix see https://www.statology.org/mcnemars-test-r/ (I wrote out 2x2 tables for w3 and w5 and input the COMPLETERS data to this test - see excel doc in LBC folder)
Mcnemar.matrix<-matrix(c(408, 16, 404, 24), nrow = 2, ncol = 2,                   dimnames = list("W3" = c("No MCR", "MCR"),
      "W5" = c("No MCR", "MCR"))) #seems this dimnames list bit is optional
mcnemar.test(Mcnemar.matrix)

##Transition rates ----
healthy_to_MCR <- ifelse(MCRcombined$mcr_mem_w3==0 & MCRcombined$mcr_mem_w5==1,1,0)
table(healthy_to_MCR)

MCR_to_healthy <- ifelse(MCRcombined$mcr_mem_w3==1 & MCRcombined$mcr_mem_w5==0,1,0)
table(MCR_to_healthy)

#all 3 waves patterns
#000
table(ifelse(MCRcombined$mcr_mem_w3==0 & MCRcombined$mcr_mem_w4==0 & MCRcombined$mcr_mem_w5==0, 1,0))
#001
table(ifelse(MCRcombined$mcr_mem_w3==0 & MCRcombined$mcr_mem_w4==0 & MCRcombined$mcr_mem_w5==1, 1,0))
#011
table(ifelse(MCRcombined$mcr_mem_w3==0 & MCRcombined$mcr_mem_w4==1 & MCRcombined$mcr_mem_w5==1, 1,0))
#111
table(ifelse(MCRcombined$mcr_mem_w3==1 & MCRcombined$mcr_mem_w4==1 & MCRcombined$mcr_mem_w5==1, 1,0))
#010
table(ifelse(MCRcombined$mcr_mem_w3==0 & MCRcombined$mcr_mem_w4==1 & MCRcombined$mcr_mem_w5==0, 1,0))
#110
table(ifelse(MCRcombined$mcr_mem_w3==1 & MCRcombined$mcr_mem_w4==1 & MCRcombined$mcr_mem_w5==0, 1,0))
#100
table(ifelse(MCRcombined$mcr_mem_w3==1 & MCRcombined$mcr_mem_w4==0 & MCRcombined$mcr_mem_w5==0, 1,0))
#0--
table(ifelse(MCRcombined$mcr_mem_w3==0 & is.na(MCRcombined$mcr_mem_w4)==TRUE & is.na(MCRcombined$mcr_mem_w5)==TRUE, 1,0))
#1--
table(ifelse(MCRcombined$mcr_mem_w3==1 & is.na(MCRcombined$mcr_mem_w4)==TRUE & is.na(MCRcombined$mcr_mem_w5)==TRUE, 1,0))
#01-
table(ifelse(MCRcombined$mcr_mem_w3==0 & MCRcombined$mcr_mem_w4==1 & is.na(MCRcombined$mcr_mem_w5)==TRUE, 1,0))
#11-
table(ifelse(MCRcombined$mcr_mem_w3==1 & MCRcombined$mcr_mem_w4==1 & is.na(MCRcombined$mcr_mem_w5)==TRUE, 1,0))

#MCRatanywave = MCR diagnosed at 73, 76 or 79, whether or not people show up at all three waves*.
#nonMCR =  attended at age 79 and not diagnosed with MCR at any wave.

MCRatanywave<-ifelse(MCRcombined$mcr_mem_w3==1 | MCRcombined$mcr_mem_w4==1 | MCRcombined$mcr_mem_w5==1, 1,0)
table(MCRatanywave)

nonMCR <- ifelse(MCRcombined$attrition == 'Completer' & MCRatanywave == 0,1,0)
table(nonMCR)

#looking at the MCR numbers for the completers of all waves
completers_vs_withdrawers_w3 <- data.frame(MCRcombined$mcr_mem_w3, MCRcombined$attrition)
completers_vs_withdrawers_w5 <- data.frame(MCRcombined$mcr_mem_w5, MCRcombined$attrition)

completers_w3 = completers_vs_withdrawers_w3[completers_vs_withdrawers_w3['MCRcombined.attrition'] == 'Completer']
completers_w5 = completers_vs_withdrawers_w5[completers_vs_withdrawers_w5['MCRcombined.attrition'] == 'Completer']


#looking at only completers of all waves
completed_w3<- complete.cases(MCRcombined$agedays_w3)
completed_w4<- complete.cases(MCRcombined$agedays_w4)
completed_w5<- complete.cases(MCRcombined$agedays_w5)
completed_MCR_w3<- complete.cases(MCRcombined$mcr_mem_w3)
completed_MCR_w4<- complete.cases(MCRcombined$mcr_mem_w4)
completed_MCR_w5<- complete.cases(MCRcombined$mcr_mem_w5)

MCRcombined$attrition <- ifelse(completed_w3 =='TRUE'& completed_w4 =='TRUE'& completed_w5 =='TRUE'& completed_MCR_w3 == 'TRUE'& completed_MCR_w4 =='TRUE'& completed_MCR_w5 =='TRUE',1,0)

MCRcombined$attrition<-as.numeric(MCRcombined$attrition)

MCRcombined$attrition <- factor(MCRcombined$attrition, levels = c(0,1),labels = c("Withdrawer", "Completer"))
table(MCRcombined$attrition)

table(MCRcombined$mcr_mem_w3, data=MCRcombined$attrition)
table(MCRcombined$mcr_mem_w4, data=MCRcombined$attrition)
table(MCRcombined$mcr_mem_w5, data=MCRcombined$attrition)


# RECODE the data - define factors----

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
         
         MCR.factor.any.w1 = 
           factor(mcr_any_w1) %>% 
           fct_recode("MCR" = "1", 
                      "No MCR" = "0") %>% 
           ff_label("MCR_any_w1"),
         
         MCR.factor.any.w2 = 
           factor(mcr_any_w2) %>% 
           fct_recode("MCR" = "1", 
                      "No MCR" = "0") %>% 
           ff_label("MCR_any_w2"),
         
         MCR.factor.any.w3 = 
           factor(mcr_any_w3) %>% 
           fct_recode("MCR" = "1", 
                      "No MCR" = "0") %>% 
           ff_label("MCR_any_w3"), 
         
         MCR.factor.any.w4 = 
           factor(mcr_any_w4) %>% 
           fct_recode("MCR" = "1", 
                      "No MCR" = "0") %>% 
           ff_label("MCR_any_w4"), 
         
         MCR.factor.any.w5 = 
           factor(mcr_any_w5) %>% 
           fct_recode("MCR" = "1", 
                      "No MCR" = "0") %>% 
           ff_label("MCR_any_w5"),
         
         Social.factor = 
           factor(hmsonum_w1) %>% 
           fct_recode("Professional" = "1",
                      "Managerial" = "2",
                      "Skilled non-manual" = "3", 
                      "Skilled manual" = "3.5",
                      "Semi-skilled" = "4",
                      "Unskilled" = "5"),
         
         ApoE.factor = 
           factor(APOEe4) %>% 
           fct_recode("No ApoE4" = "0", 
                      "ApoE4" = "1") %>% 
           ff_label("ApoE4"), 
         
         Smoking.factor.w1 = 
           factor(smokcat_w1) %>% 
           fct_recode("Never" = "0",
                      "Ex-smoker" = "1",
                      "Current" = "2") %>% 
           ff_label("Smoke_w1"),
         
         Smoking.factor.w2 = 
           factor(smokcurr_w2) %>% 
           fct_recode("Never" = "0",
                      "Ex-smoker" = "1",
                      "Current" = "2") %>% 
           ff_label("Smoke_w2"),
         
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
         
         Alcohol.factor.w1 = 
           factor(alcohol_w1) %>% 
           fct_recode("No alcohol" = "0",
                      "Drinks alcohol" = "1",) %>% 
           ff_label("Alcohol_w1"),
         
         Alcohol.factor.w2 = 
           factor(alcohol_w2) %>% 
           fct_recode("No alcohol" = "0",
                      "Drinks alcohol" = "1",) %>% 
           ff_label("Alcohol_w2"),
         
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
         
         CVD.factor.w1 = 
           factor(cvdhist_w1) %>% 
           fct_recode("No CVD" = "0",
                      "CVD" = "1",) %>% 
           ff_label("CVD_w1"),
         
         CVD.factor.w2 = 
           factor(cvdhist_w2) %>% 
           fct_recode("No CVD" = "0",
                      "CVD" = "1",) %>% 
           ff_label("CVD_w2"),
         
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
         
         Stroke.factor.w1 = 
           factor(stroke_w1) %>% 
           fct_recode("No stroke" = "0",
                      "Stroke" = "1",) %>% 
           ff_label("Stroke_w1"),
         
         Stroke.factor.w2 = 
           factor(stroke_w2) %>% 
           fct_recode("No stroke" = "0",
                      "Stroke" = "1",) %>% 
           ff_label("Stroke_w2"),
         
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
         
         PD.factor.w1 = 
           factor(parkin_w1) %>% 
           fct_recode("No PD" = "0",
                      "PD" = "1",) %>% 
           ff_label("PD_w1"),
         
         PD.factor.w2 = 
           factor(parkin_w2) %>% 
           fct_recode("No PD" = "0",
                      "PD" = "1",) %>% 
           ff_label("PD_w2"),
         
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
         
         Diabetes.factor.w1 = 
           factor(diab_w1) %>% 
           fct_recode("No Diabetes" = "0",
                      "Diabetes" = "1",) %>% 
           ff_label("Diabetes_w1"),
         
         Diabetes.factor.w2 = 
           factor(diab_w2) %>% 
           fct_recode("No Diabetes" = "0",
                      "Diabetes" = "1",) %>% 
           ff_label("Diabetes_w2"),
         
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
           
         Dementia.factor.w1 = 
           factor(dement_w1) %>% 
           fct_recode("No Dementia" = "0") %>% 
           ff_label("Dementia_w1"),
         
         Dementia.factor.w2 = 
           factor(dement_w2) %>% 
           fct_recode("No Dementia" = "0",
                      "Dementia" = "1",) %>% 
           ff_label("Dementia_w2"),
         
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
         
         Arthritis.factor.w1 = 
           factor(arthrit_w1) %>% 
           fct_recode("No Arthritis" = "0",
                      "Arthritis" = "1",) %>% 
           ff_label("Arthritis_w1"),
         
         Arthritis.factor.w2 = 
           factor(arthrit_w2) %>% 
           fct_recode("No Arthritis" = "0",
                      "Arthritis" = "1",) %>% 
           ff_label("Arthritis_w2"),
         
         Arthritis.factor.w3 = 
           factor(arthrit_w3) %>% 
           fct_recode("No Arthritis" = "0",
                      "Arthritis" = "1",) %>% 
           ff_label("Arthritis_w3"),
         
         Arthritis.factor.w4 = 
           factor(arthrit_w4) %>% 
           fct_recode("No Arthritis" = "0",
                      "Arthritis" = "1",) %>% 
           ff_label("Arthritis_w4"),
         
         Arthritis.factor.w5 = 
           factor(arthrit_w5) %>% 
           fct_recode("No Arthritis" = "0",
                      "Arthritis" = "1",) %>% 
           ff_label("Arthritis_w5"),
         
         On.meds.factor.w1 = 
           factor(onmeds_w1) %>% 
           fct_recode("No Meds" = "0",
                      "Meds" = "1",) %>% 
           ff_label("On.Meds_w1"),
         
         On.meds.factor.w2 = 
           factor(onmeds_w2) %>% 
           fct_recode("No Meds" = "0",
                      "Meds" = "1",) %>% 
           ff_label("On.Meds_w2"),
         
         On.meds.factor.w3 = 
           factor(onmeds_w3) %>% 
           fct_recode("No Meds" = "0",
                      "Meds" = "1",) %>% 
           ff_label("On.Meds_w3"),
         
         On.meds.factor.w4 = 
           factor(onmeds_w4) %>% 
           fct_recode("No Meds" = "0",
                      "Meds" = "1",) %>% 
           ff_label("On.Meds_w4"),
         
        # No onmeds_w5 available
         
         Legpain.factor.w1 = 
           factor(legpain_w1) %>% 
           fct_recode("No Legpain" = "0",
                      "Legpain" = "1",) %>% 
           ff_label("Legpain_w1"),
         
         Legpain.factor.w2 = 
           factor(legpain_w2) %>% 
           fct_recode("No Legpain" = "0",
                      "Legpain" = "1",) %>% 
           ff_label("Legpain_w2"),
         
         Legpain.factor.w3 = 
           factor(legpain_w3) %>% 
           fct_recode("No Legpain" = "0",
                      "Legpain" = "1",) %>% 
           ff_label("Legpain_w3"),
         
         Legpain.factor.w4 = 
           factor(legpain_w4) %>% 
           fct_recode("No Legpain" = "0",
                      "Legpain" = "1",) %>% 
           ff_label("Legpain_w4"),
         
         Legpain.factor.w5 = 
           factor(legpain_w5) %>% 
           fct_recode("No Legpain" = "0",
                      "Legpain" = "1",) %>% 
           ff_label("Legpain_w5"),
         
         HiBP.factor.w1 = 
           factor(hibp_w1) %>% 
           fct_recode("No HiBP" = "0",
                      "HiBP" = "1",) %>% 
           ff_label("HiBP_w1"),
         
         HiBP.factor.w2 = 
           factor(hibp_w2) %>% 
           fct_recode("No HiBP" = "0",
                      "HiBP" = "1",) %>% 
           ff_label("HiBP_w2"),
         
         HiBP.factor.w3 = 
           factor(hibp_w3) %>% 
           fct_recode("No HiBP" = "0",
                      "HiBP" = "1",) %>% 
           ff_label("HiBP_w3"),
         
         HiBP.factor.w4 = 
           factor(hibp_w4) %>% 
           fct_recode("No HiBP" = "0",
                      "HiBP" = "1",) %>% 
           ff_label("HiBP_w4"),
         
         HiBP.factor.w5 = 
           factor(hibp_w5) %>% 
           fct_recode("No HiBP" = "0",
                      "HiBP" = "1",) %>% 
           ff_label("HiBP_w5"),
         
         Marital.factor.w1 = 
           factor(marital_w1) %>% 
           fct_recode("Married" = "1",
                      "Single" = "2",
                      "Divorced" = "3", 
                      "Cohabiting" = "4",
                      "Widowed" = "5",
                      "Other" = "6") %>% 
           ff_label("Marital_w1"),
         
         # seems there is no marital_w2
         
         Marital.factor.w3 = 
           factor(marital_w3) %>% 
           fct_recode("Married" = "1",
                      "Single" = "2",
                      "Divorced" = "3", 
                      "Cohabiting" = "4",
                      "Widowed" = "5",
                      "Other" = "6") %>% 
           ff_label("Marital_w3"),
         
         # note change in levels at w4 from other marital_waves
         Marital.factor.w4 = 
           factor(marital_w4) %>% 
           fct_recode("Married" = "2",
                      "Single" = "1",
                      "Divorced" = "4", 
                      "Cohabiting" = "3",
                      "Separated" = "5",
                      "Widowed" = "6") %>% 
           ff_label("Marital_w4"),
         
         Marital.factor.w5 = 
           factor(marital_w5) %>% 
           fct_recode("Married" = "2",
                      "Single" = "1",
                      "Divorced" = "4", 
                      "Cohabiting" = "3",
                      "Separated" = "5",
                      "Widowed" = "6",
                      "Other" = "7") %>% 
           ff_label("Marital_w5")
  )

# 
write_csv(MCRcombinedRecoded, file = "MCRcombinedRecoded.csv")
                    
# Correlation Matrix (of continuous variables) ----
# https://www.displayr.com/how-to-create-a-correlation-matrix-in-r/
# really should check if the cont vars are para aka normal (Pearson) or nonpara (Spearman) first to decide on which test to run
# in this instance, I'm just getting a rough idea of which cog tests correlate 
# Step 1 - subset data to only include cont variables

# Most of the CONTINUOUS/QUANTITATIVE variables in the dataset
cor_matrix_data_MOST <- MCRcombinedRecoded %>% 
  select( "agedays_w1", "agedays_w2", "agedays_w3", "agedays_w4","yrsedu_w1", "alcunitwk_w1", "alcunitwk_w2", "alcunitwk_w3", "alcunitwk_w4", "hadsa_w1", "hadsa_w2", "hadsa_w3", "HADS_A_w4","hadsd_w1" , "hadsd_w2", "hadsd_w3", "HADS_D_w4","mmse_w1", "mmse_w2", "mmse_w3","mmse_w4" , "age11IQ" , "age70IQ_w1", "mht1947", "mht_w1", "lm1_re_w1", "lm1_re_w2" , "lm1_re_w3", "lm1_re_w4", "lm2_re_w1", "lm2_re_w2"  , "lm2_re_w3", "lm2_re_w4", "lm2_tu_w1", "lm2_tu_w2", "lm2_tu_w3", "lm2_tu_w4", "lmtotal_w1", "lmtotal_w2", "lmtotal_w3", "lmtotal_w4", "vpatotal_w1", "vpatotal_w2" ,  "vpatotal_w3",  "vpatotal_w4", "spantot_w1" , "spantot_w2"       , "spantot_w3", "spantot_w4", "matreas_w1", "matreas_w2", "matreas_w3", "matreas_w4", "vftot_w1", "vftot_w2", "vftot_w3",  "vftot_w4" , "digback_w1", "digback_w2", "digback_w3", "digback_w4", "nart_w1", "nart_w2", "nart_total_w3", "nart_total_w4", "wtar_w1", "wtar_w2", "wtar_total_w3", "wtar_total_w4", "blkdes_w1", "blkdes_w2", "blkdes_w3", "blkdes_w4", "ittotal_w1", "ittotal_w2", "ittotal_w3", "ittotal_w4" , "digsym_w1", "digsym_w2","digsym_w3", "digsym_w4" ,"srtmean_w1" ,"srtmean_w2", "srtmean_w3", "srtmean_w4", "crtmean_w1","crtmean_w4" ,"trailmakingtime_w3", "trailmakingtime_w4", "height_w1", "height_w2", "height_w3", "height_w4", "weight_w1", "weight_w2", "weight_w3","weight_w4", "bmi_w1", "bmi_w2","bmi_w3", "bmi_w4" , "sixmwk_w1", "sixmwk_w2", "sixmwk_w3", "sixmwk_w4","sitstnd_w1", "dbp1sit_w1" , "dbp1sit_w2", "dbp1sit_w3", "dbp1sit_w4", "sbp1sit_w1", "sbp1sit_w2", "sbp1sit_w3" ,"sbp1sit_w4" ,"dbp1std_w1", "dbp1std_w2" , "dbp1std_w3", "dbp1std_w4", "sbp1std_w1", "sbp1std_w2" , "sbp1std_w3", "sbp1std_w4", "fev_w1", "fev_w2", "fev_w3", "fev_w4", "griprh_w1" , "griprh_w2", "griprh_w3", "griprh_w4" , "griplh_w1", "bld_crprot_w2" , "bld_crprot_w3", "bld_crprot_w4", "APOEe4"      , "alcunitwk_w5" ,"HADS_A_w5", "HADS_D_w5", "HADS_total_w5"    , "mmse_w5" , "lm1_re_w5" , "lm2_re_w5" , "lm2_tu_w5"           , "lmtotal_w5", "vpa_total_w5" ,  "spantot_w5" , "matreas_w5" , "vftot_w5" , "digback_w5" , "nart_total_w5"     ,"wtar_total_w5" , "blkdes_w5","ittotal_w5", "digsym_w5" , "srtmean_w5" , "crtmean_w5" , "trailmakingtime_w5" , "height_w5" , "weight_w5", "bmi_w5","sixmwk_w5", "dbp1sit_w5" ,"sbp1sit_w5" ,"dbp1std_w5", "sbp1std_w5","fev_w5", "griprh_w5", "chairst_w5" , "chairst_sec_w5", "bld_choles_w5", "bld_hba1c_IFFC_w5"        ,"bld_crprot_w5")

cor_matrix_data <- MCRcombinedRecoded %>% 
  select("agedays_w1", "agedays_w2", "agedays_w3", "agedays_w4","yrsedu_w1", "mmse_w1", "mmse_w2", "mmse_w3","mmse_w4" , "age11IQ" , "age70IQ_w1", "mht1947", "mht_w1", "lm1_re_w1", "lm1_re_w2" , "lm1_re_w3", "lm1_re_w4", "lm2_re_w1", "lm2_re_w2"  , "lm2_re_w3", "lm2_re_w4", "lm2_tu_w1", "lm2_tu_w2", "lm2_tu_w3", "lm2_tu_w4", "lmtotal_w1", "lmtotal_w2", "lmtotal_w3", "lmtotal_w4", "vpatotal_w1", "vpatotal_w2" ,  "vpatotal_w3",  "vpatotal_w4", "spantot_w1" , "spantot_w2"       , "spantot_w3", "spantot_w4", "matreas_w1", "matreas_w2", "matreas_w3", "matreas_w4", "vftot_w1", "vftot_w2", "vftot_w3",  "vftot_w4" , "digback_w1", "digback_w2", "digback_w3", "digback_w4", "nart_w1", "nart_w2", "nart_total_w3", "nart_total_w4", "wtar_w1", "wtar_w2", "wtar_total_w3", "wtar_total_w4", "blkdes_w1", "blkdes_w2", "blkdes_w3", "blkdes_w4", "ittotal_w1", "ittotal_w2", "ittotal_w3", "ittotal_w4" , "digsym_w1", "digsym_w2","digsym_w3", "digsym_w4" ,"srtmean_w1" ,"srtmean_w2", "srtmean_w3", "srtmean_w4", "crtmean_w1","crtmean_w4" ,"trailmakingtime_w3", "trailmakingtime_w4")

(cor_matrix_data_w1 <- MCRcombinedRecoded %>% 
  select("agedays_w1","yrsedu_w1", "age11IQ" , "lm1_re_w1",  "lm2_re_w1",  "lm2_tu_w1",  "lmtotal_w1",  "vpatotal_w1",  "spantot_w1", "matreas_w1", "vftot_w1",  "digback_w1",  "nart_w1",  "wtar_w1",  "blkdes_w1",  "ittotal_w1", "digsym_w1", "srtmean_w1" , "crtmean_w1", "mcr_mem_w3")) # note this includes mcr_mem_w3 which is 1/0, this is probably not appropriate. Should be correlated to numeric variable using point-biserial correlation but I don't know how to incorporate this into the matrix. 

str(cor_matrix_data_w1)
head(cor_matrix_data_w1, 3)
(cor_matrix_data.cor_w1  = cor(cor_matrix_data_w1 , method = c("pearson"), use = "complete.obs"))

# Significance levels (p-values) can also be generated using the rcorr function which is found in the Hmisc package. 


cor_matrix_data.rcorr_w1  = rcorr(as.matrix(cor_matrix_data_w1)) #This generates one table of correlation coefficients (the correlation matrix) and another table of the p-values. 
cor_matrix_data.rcorr_w1  

# extract the values from this object into a useable data structure
cor_matrix_data.coeff_w1  = cor_matrix_data.rcorr_w1$r
cor_matrix_data.p_w1  = cor_matrix_data.rcorr_w1$P

# Visualizing the correlation matrix


cogtestcorrplot_w1  <- corrplot(cor_matrix_data.cor_w1)

# (this next step didn't work for me) Efficiently filter out uncorrelated variables to see more relevant results. https://towardsdatascience.com/how-to-create-a-correlation-matrix-with-too-many-variables-309cc0c0a57
corr_simple <- function(data=cor_matrix_data, sig= 0.5)
  #convert data to numeric in order to run correlations
  #convert to factor first to keep the integrity of the data - each value will become a number rather than turn into NA
  df_cor <- corr_simple %>% mutate_if(is.character, as.factor)
df_cor <- df_cor %>% mutate_if(is.factor, as.numeric)

#run a correlation and drop the insignificant ones
corr <- cor(df_cor)
#prepare to drop duplicates and correlations of 1     
corr[lower.tri(corr,diag=TRUE)] <- NA 
#drop perfect correlations
corr[corr == 1] <- NA 

#turn into a 3-column table
corr <- as.data.frame(as.table(corr))
#remove the NA values from above 
# corr <- na.omit(corr) # this removes all but 3 variables!

#select significant values  
corr <- subset(corr, abs(Freq) > sig) 
#sort by highest correlation
corr <- corr[order(-abs(corr$Freq)),] 

# Explore associations between explanatory variables - equiv of a correlation matrix ----
# from the log reg chapter of HealthyR, needs more work before valid- this is just an example/trial
explanatory_w1_ggally = c("ageyears_w1", "digsym_w1", "ittotal_w1", "vftot_w1", "fev_w1", "bld_crprot_w1")
library(GGally)
MCRcombinedRecoded %>% 
  remove_labels() %>% 
  ggpairs(columns = explanatory_w1_ggally)
# EXPLORATORY DATA ANALYSIS----

# Plot the data----
# To start, we simply count the number of patients with MCRw3 with different exposures at wave 1 - or maybe look at MCRw3 as explanatory and cognitive tests in w5 as dependent variables. if this is an association study, MCR is my OUTCOME variable. It is useful to plot this as counts but also as proportions. It is proportions you are comparing, but you really want to know the absolute numbers as well.  

## Wave 1 Explanatory variables EDA ----

p1 <- MCRcombinedRecoded %>% 
  ggplot(aes(x = MCR.factor.w3, fill = Alcohol.factor.w1)) +
  geom_bar() +
  theme(legend.position = "none")

p2 <- MCRcombinedRecoded %>% 
  ggplot(aes(x = MCR.factor.w3, fill = Alcohol.factor.w1)) +
  geom_bar(position = "fill") +
  ylab("proportion")

MCRw3_Alcoholw1 <- p1 + p2
MCRw3_Alcoholw1
ggsave(MCRw3_Alcoholw1, filename = "MCRw3_Alcoholw1.png", height = 5, width = 8)

plot_marital1 <- MCRcombinedRecoded %>% 
  ggplot(aes(x = MCR.factor.w3, fill = Marital.factor.w1)) +
  geom_bar() +
  theme(legend.position = "none")

plot_marital2 <- MCRcombinedRecoded %>% 
  ggplot(aes(x = MCR.factor.w3, fill = Marital.factor.w1)) +
  geom_bar(position = "fill") +
  ylab("proportion")

MCRw3_Maritalw1 <- plot_marital1 + plot_marital2
MCRw3_Maritalw1

ggsave(MCRw3_Maritalw1, filename = "MCRw3_Maritalw1.png", height = 5, width = 8)

# try a another variable
p3 <- MCRcombinedRecoded %>% 
  ggplot(aes(x = MCR.factor.w3, fill = Dementia.factor.w1)) +
  geom_bar() +
  theme(legend.position = "none")


p4 <- MCRcombinedRecoded %>% 
  ggplot(aes(x = MCR.factor.w3, fill = Dementia.factor.w1)) +
  geom_bar(position = "fill") +
  ylab("proportion")

MCRw3_Demw1 <- p3 + p4
MCRw3_Demw1 # nothing to see here ? as no dementia in wave 1 (excluded at outset!)

ggsave(MCRw3_Demw1, filename = "MCRw3_Demw1.png", height = 5, width = 8)

names(MCRcombinedRecoded)

# other variables to explore in relation to MCR+/- groups: (these have been added to explanatory_w1 - except falls_w4!) "bld_crprot_w1", "bld_hba1c_w1", "bld_choles_w1", "fev_w1",  "bmi_w1" "falls_w4" "arthrit_w1", "onmeds_w1", "cvdhist_w1", "hibp_w1",  "yrsedu_w1", "marital_w1", "evmarr_w1", "alcunitwk_w1", "legpain_w1" 

# 
plot_social1 <- MCRcombinedRecoded %>% 
  ggplot(aes(x = MCR.factor.w3, fill = hmsonum_w1)) +
  geom_bar() +
  theme(legend.position = "none")


plot_social2 <- MCRcombinedRecoded %>% 
  ggplot(aes(x = MCR.factor.w3, fill = hmsonum_w1)) +
  geom_bar(position = "fill") +
  ylab("proportion")

MCRw3_social <- plot_social1 + plot_social2
MCRw3_social



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

## Wave 5 variables ----

p1 <- MCRcombinedRecoded %>% 
  ggplot(aes(x = MCR.factor.w3, fill = Alcohol.factor.w3)) +
           geom_bar() +
           theme(legend.position = "none")

p2 <- MCRcombinedRecoded %>% 
  ggplot(aes(x = MCR.factor.w3, fill = Alcohol.factor.w3)) +
           geom_bar(position = "fill") +
           ylab("proportion")

MCRw3_Alcoholw3 <- p1 + p2
MCRw3_Alcoholw3
ggsave(MCRw3_Alcoholw3, filename = "MCRw3_Alcoholw3.png", height = 5, width = 8)


# try a another variable
p3 <- MCRcombinedRecoded %>% 
  ggplot(aes(x = MCR.factor.w3, fill = Dementia.factor.w3)) +
  geom_bar() +
  theme(legend.position = "none")


p4 <- MCRcombinedRecoded %>% 
  ggplot(aes(x = MCR.factor.w3, fill = Dementia.factor.w3)) +
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

## Histogram - Plot Outcome variable(s) ----
# Plot other variables that will be important as either outcome or explanatory variables (vftot, tmt, hmda, hmdd etc) to determine their spread ('distribution') and thus decide if I can use parametric tests such as t-test or non-parametric such as chi sq.

### Verbal fluency variable - Note all parametric ----
# all vftot waves in one graph (quite big)
(vftot_distr <- MCRcombinedRecoded %>% 
   pivot_longer(contains("vftot")) %>% 
   ggplot(aes(x = value)) +
   geom_histogram(bins = 15) +
   facet_wrap(~name, scales = "free"))

# individual graphs for each year for record
(vftot_w1_distribution <- MCRcombinedRecoded %>% 
  ggplot(aes(x = vftot_w1)) +
  geom_histogram(bins = 40))

(vftot_w2_distribution <- MCRcombinedRecoded %>% 
    ggplot(aes(x = vftot_w2)) +
    geom_histogram(bins = 40))
  
(vftot_w3_distribution <- MCRcombinedRecoded %>% 
    ggplot(aes(x = vftot_w3)) +
    geom_histogram(bins = 40))

(vftot_w4_distribution <- MCRcombinedRecoded %>% 
    ggplot(aes(x = vftot_w4)) +
    geom_histogram(bins = 40))

(vftot_w5_distribution <- MCRcombinedRecoded %>% 
    ggplot(aes(x = vftot_w5)) +
    geom_histogram(bins = 40))

### ittotal variable ----
# all ittotal waves in one graph (quite big)
(ittotal_distr <- MCRcombinedRecoded %>% 
   pivot_longer(contains("ittotal")) %>% 
   ggplot(aes(x = value)) +
   geom_histogram(bins = 15) +
   facet_wrap(~name, scales = "free"))  # parametric but has two outliers (mean and median very similar still though)

# replace outliers in ittotal_w1 with NAs (this didn't work for some reason)
MCRcombinedRecoded %>% 
  ff_glimpse("ittotal_w1")

MCRcombinedRecoded <- MCRcombinedRecoded %>%
  replace_with_na(replace = list("ittotal_w1" <70))

### TMT variable - Note all non-parametric, right-skewed ----
# these are log-transformed later (with good effect)
(TMTb_distr <- MCRcombinedRecoded %>% 
   pivot_longer(contains("trailmakingtime")) %>% 
   ggplot(aes(x = value)) +
   geom_histogram(bins = 15) +
   facet_wrap(~name, scales = "free")) 


### Depression variable - Note all non-parametric, right-skewed ----
# these are log-transformed later (without much effect)
(hadsd_distr <- MCRcombinedRecoded %>% 
   pivot_longer(contains(c("hadsd", "HADS_D"))) %>% # variable called hadsd w1-3 then HADS_D w4,5
   ggplot(aes(x = value)) +
   geom_histogram(bins = 15) +
   facet_wrap(~name, scales = "free")) 


### Anxiety variable - Note, all non-parametric, right-skewed ----
(hadsa_distr <- MCRcombinedRecoded %>% 
   pivot_longer(contains(c("hadsa", "HADS_A"))) %>% # variable called hadsd w1-3 then HADS_D w4,5
   ggplot(aes(x = value)) +
   geom_histogram(bins = 40) +
   facet_wrap(~name, scales = "free")) 

(hadstot_distr <- MCRcombinedRecoded %>% 
    pivot_longer(contains("hadstot")) %>% 
    ggplot(aes(x = value)) +
    geom_histogram(bins = 15) +
    facet_wrap(~name, scales = "free"))  # non-par, r tail/skewed/positive

### digsym & others ----
(digsym_distr <- MCRcombinedRecoded %>% 
  pivot_longer(contains("digsym")) %>% 
  ggplot(aes(x = value)) +
  geom_histogram(bins = 15) +
  facet_wrap(~name, scales = "free")) # parametric

(age11IQ_distribution <- MCRcombinedRecoded %>% 
    ggplot(aes(x = age11IQ)) +
    geom_histogram(bins = 40)) # non-par, LEFT tail/negative skew

(hadstot_distr <- MCRcombinedRecoded %>% 
    pivot_longer(contains("hadstot")) %>% 
    ggplot(aes(x = value)) +
    geom_histogram(bins = 15) +
    facet_wrap(~name, scales = "free"))  # non-par, r tail/skewed/positive

(lm1_re_distr <- MCRcombinedRecoded %>% 
    pivot_longer(contains("lm1_re")) %>% 
    ggplot(aes(x = value)) +
    geom_histogram(bins = 15) +
    facet_wrap(~name, scales = "free"))  # Parametric

(lm2_re_distr <- MCRcombinedRecoded %>% 
    pivot_longer(contains("lm2_re")) %>% 
    ggplot(aes(x = value)) +
    geom_histogram(bins = 15) +
    facet_wrap(~name, scales = "free"))  # Parametric

(lmtotal_distr <- MCRcombinedRecoded %>% 
    pivot_longer(contains("lmtotal")) %>% 
    ggplot(aes(x = value)) +
    geom_histogram(bins = 15) +
    facet_wrap(~name, scales = "free"))  # Parametric

(vpatotal_distr <- MCRcombinedRecoded %>% 
    pivot_longer(contains("vpatotal")) %>% 
    ggplot(aes(x = value)) +
    geom_histogram(bins = 15) +
    facet_wrap(~name, scales = "free")) # non-par, LEFT tail/negative skew

(spantot_distr <- MCRcombinedRecoded %>% 
    pivot_longer(contains("spantot")) %>% 
    ggplot(aes(x = value)) +
    geom_histogram(bins = 15) +
    facet_wrap(~name, scales = "free")) # Parametric

(matreas_distr <- MCRcombinedRecoded %>% 
    pivot_longer(contains("matreas")) %>% 
    ggplot(aes(x = value)) +
    geom_histogram(bins = 15) +
    facet_wrap(~name, scales = "free")) # Parametric-ish!

(matreas_w1_distribution <- MCRcombinedRecoded %>% 
    ggplot(aes(x = matreas_w1)) +
    geom_histogram(bins = 40)) # Parametric-ish!

(hadstot_distr <- MCRcombinedRecoded %>% 
    pivot_longer(contains("hadstot")) %>% 
    ggplot(aes(x = value)) +
    geom_histogram(bins = 15) +
    facet_wrap(~name, scales = "free")) # non-par, r tail/skewed/positive

(digback_distr <- MCRcombinedRecoded %>% 
    pivot_longer(contains("digback")) %>% 
    ggplot(aes(x = value)) +
    geom_histogram(bins = 15) +
    facet_wrap(~name, scales = "free")) # Parametric

(nart_distr <- MCRcombinedRecoded %>% 
    pivot_longer(contains("nart")) %>% 
    ggplot(aes(x = value)) +
    geom_histogram(bins = 15) +
    facet_wrap(~name, scales = "free")) # Left tail/skewed/negative

(blkdes_distr <- MCRcombinedRecoded %>% 
    pivot_longer(contains("blkdes")) %>% 
    ggplot(aes(x = value)) +
    geom_histogram(bins = 15) +
    facet_wrap(~name, scales = "free")) # Parametric

(srtmean_distr <- MCRcombinedRecoded %>% 
    pivot_longer(contains("srtmean")) %>% 
    ggplot(aes(x = value)) +
    geom_histogram(bins = 15) +
    facet_wrap(~name, scales = "free")) # non-par, r tail/skewed/positive

(griprh_distr <- MCRcombinedRecoded %>% 
    pivot_longer(contains("griprh")) %>% 
    ggplot(aes(x = value)) +
    geom_histogram(bins = 15) +
    facet_wrap(~name, scales = "free")) # Parametric

(griplh_distr <- MCRcombinedRecoded %>% 
    pivot_longer(contains("griplh")) %>% 
    ggplot(aes(x = value)) +
    geom_histogram(bins = 15) +
    facet_wrap(~name, scales = "free")) # Parametric

(days_sig_exe_distr <- MCRcombinedRecoded %>% 
    pivot_longer(contains("Days_significant")) %>% 
    ggplot(aes(x = value)) +
    geom_histogram(bins = 15) +
    facet_wrap(~name, scales = "free")) # MASSIVE non-par, r tail/skewed/positive - vast majority scored 0


explanatory = c("ageyears_w3", "Sex.factor", "Social.factor", "yrsedu_w1", "Smoking.factor.w3", "ApoE.factor", "Alcohol.factor.w3", "CVD.factor.w3", "Stroke.factor.w3", "Diabetes.factor.w3", "trailmakingtime_w3", "bld_crprot_w3", "Days_significant_exercise_w3", "vftot_w3", "age70IQ_w1", "hadsd_w3", "hadsa_w3")

## QQ-plot ----
# comparing the distribution (think shape) of our own data to a theoretical distribution, such as the normal distribution.

### Verbal fluency ----
(vftot_w1_qq <- MCRcombinedRecoded %>% 
  ggplot(aes(sample = vftot_w1)) +
  geom_qq() +
  geom_qq_line(colour = "blue")) +
  ggtitle("Verbal Fluency w1 vftot_w1")

(vftot_w2_qq <- MCRcombinedRecoded %>% 
  ggplot(aes(sample = vftot_w2)) +
  geom_qq() +
  geom_qq_line(colour = "blue")) +
  ggtitle("Verbal Fluency w2 vftot_w2")

(vftot_w3_qq <- MCRcombinedRecoded %>% 
  ggplot(aes(sample = vftot_w3)) +
  geom_qq() +
  geom_qq_line(colour = "blue")) +
  ggtitle("Verbal Fluency w3 vftot_w3")

(vftot_w4_qq <- MCRcombinedRecoded %>% 
  ggplot(aes(sample = vftot_w4)) +
  geom_qq() +
  geom_qq_line(colour = "blue")) +
  ggtitle("Verbal Fluency w4 vftot_w4")

(vftot_w5_qq <- MCRcombinedRecoded %>% 
  ggplot(aes(sample = vftot_w5)) +
  geom_qq() +
  geom_qq_line(colour = "blue")) +
  ggtitle("Verbal Fluency w5 vftot_w5")

## Boxplot ----
# the preferred method for comparing a continuous outcome variable across a categorical explanatory variable
# The box represents the median (bold horizontal line in the middle) and interquartile range (where 50% of the data sits). The lines (whiskers) extend to the lowest and highest values that are still within 1.5 times the interquartile range. Outliers (anything outwith the whiskers) are represented as points.

### Verbal fluency with MCR----
# Wave 3 (starting at w3 as this is when MCR starts)
MCRcombinedRecoded %>% 
  ggplot(aes(x = , y = vftot_w3)) +
  geom_boxplot(aes())  +     
  facet_wrap( "MCR.factor.w3") +
  xlab("No MCR                  MCR               Missing") +
  ggtitle(
    "MCR w3 and verbal fluency w3"
  )

# Wave 4
(MCRcombinedRecoded %>% 
  ggplot(aes(x = , y = vftot_w4)) +
  geom_boxplot(aes()) +
  facet_wrap( "MCR.factor.w4") +
  xlab("No MCR                  MCR               Missing") +
  ggtitle(
    "MCR w4 and verbal fluency w4"
  ))

# Wave 5
(MCRcombinedRecoded %>% 
    ggplot(aes(x = , y = vftot_w5)) +
    geom_boxplot(aes()) +
    facet_wrap( "MCR.factor.w5") +
    xlab("No MCR                  MCR               Missing") +
    ggtitle(
      "MCR w5 and verbal fluency w5"
    ))

### MCRw3, vftotw5 ----
(MCRcombinedRecoded %>% 
    ggplot(aes(x = , y = vftot_w5)) +
    geom_boxplot(aes()) +
    facet_wrap( "MCR.factor.w3") +
    xlab("No MCR                  MCR               Missing") +
    ggtitle(
      "MCR w3 and verbal fluency w5"
    ))

# T-test comparing means of continuous cognitive outcome variables of two independent (categorical) groups (MCR+ and MCR-) ----
# from https://argoshare.is.ed.ac.uk/healthyr_book/compare-the-means-of-two-groups.html
ttest_result_vftot_w1 <- MCRcombinedRecoded %>% 
  t.test(vftot_w1 ~ MCR.factor.w3, data = .) %>%
  tidy()
ttest_result_vftot_w1 # significant
# above the 'data=.' gets the data in the order the t test wants it, and 'tidy()' from broom gets key info into a nice wee tibble. 
# the data = . bit is necessary because the pipe usually sends data to the beginning of function brackets. 
# However, this is not an order that t.test() will accept. t.test() wants us to specify the formula first, and then wants the data these variables are present in.

# T-test assumes that variable has normal distribution

ttest_result_vftot_w3 <- MCRcombinedRecoded %>% 
  t.test(vftot_w3 ~ MCR.factor.w3, data = .) %>%
  tidy()
ttest_result_vftot_w3 # significant

ttest_result_vftot_w4 <- MCRcombinedRecoded %>% 
  t.test(vftot_w4 ~ MCR.factor.w3, data = .) %>%
  tidy()
ttest_result_vftot_w4 # significant

ttest_result_vftot_w5 <- MCRcombinedRecoded %>% 
  t.test(vftot_w5 ~ MCR.factor.w3, data = .) %>%
  tidy()
ttest_result_vftot_w5

# Non-parametric test ----
# https://argoshare.is.ed.ac.uk/healthyr_book/chap06-non-param-tests.html

  
# As the TMT data is, reasonably and understandably, skewed to the right, it might be better to log-transform it to make it normal then perform ttest, rather than jump to non-par test.If at all possible, you should us parametric tests, as they tend to be more accurate. Parametric tests have greater statistical power, which means they are likely to find a true significant effect (https://www.statisticshowto.com/probability-and-statistics/statistics-definitions/parametric-and-non-parametric-data/). 
# Remember, the Welch t-test is reasonably robust to divergence from the normality assumption, so small deviations can be safely ignored.

## Log-transforming data ----
# natural log, log(), is common and suited if there is a substantial right skew (https://argoshare.is.ed.ac.uk/healthyr_book/chap06-non-param-tests.html). 

# Natural log() transform TMT
MCRcombinedRecoded <- MCRcombinedRecoded %>% 
  mutate(logTMT_w3 = log(trailmakingtime_w3),
        logTMT_w4 = log(trailmakingtime_w4),
        logTMT_w5 = log(trailmakingtime_w5))

# Natural log() transform Depression (hadsd)
MCRcombinedRecoded <- MCRcombinedRecoded %>% 
  mutate(log_hadsd_w1 = log(hadsd_w1),
         log_hadsd_w2 = log(hadsd_w2),
         log_hadsd_w3 = log(hadsd_w3),
         log_hadsd_w4 = log(HADS_D_w4),
         log_hadsd_w5 = log(HADS_D_w5))

# Natural log() transform Anxiety (hadsa)
MCRcombinedRecoded <- MCRcombinedRecoded %>% 
  mutate(log_hadsa_w1 = log(hadsa_w1),
         log_hadsa_w2 = log(hadsa_w2),
         log_hadsa_w3 = log(hadsa_w3),
         log_hadsa_w4 = log(HADS_A_w4),
         log_hadsa_w5 = log(HADS_A_w5))

names(MCRcombinedRecoded) # just checking new log variables mutated on

## Histogram of log-transformations ----
# This worked for TMT
MCRcombinedRecoded %>% 
  pivot_longer(contains("tmt")) %>% 
  ggplot(aes(x = value)) +
  geom_histogram(bins = 15) +
  facet_wrap(~name, scales = "free") 

# I'm not convinced this worked for hadsa/d judging by the following histograms (? too many 0 values - i.e. no depression/anxiety)
# hadsa
MCRcombinedRecoded %>% 
  pivot_longer(contains("hadsa")) %>% 
  ggplot(aes(x = value)) +
    geom_histogram(bins = 15) +
    facet_wrap(~name, scales = "free")

# hadsd
MCRcombinedRecoded %>% 
  pivot_longer(contains("hadsd")) %>% 
  ggplot(aes(x = value)) +
  geom_histogram(bins = 15) +
  facet_wrap(~name, scales = "free")

## Base-10 log - log10() - transform for more substantial right skew ----
# if this doesn't work, must mean I have to use non-parametric tests on these variables

# log10() transform TMT
MCRcombinedRecoded <- MCRcombinedRecoded %>% 
  mutate(log10TMT_w3 = log10(trailmakingtime_w3),
         log10TMT_w4 = log10(trailmakingtime_w4),
         log10TMT_w5 = log10(trailmakingtime_w5))

# log10() transform Depression (hadsd)
MCRcombinedRecoded <- MCRcombinedRecoded %>% 
  mutate(log10_hadsd_w1 = log10(hadsd_w1),
         log10_hadsd_w2 = log10(hadsd_w2),
         log10_hadsd_w3 = log10(hadsd_w3),
         log10_hadsd_w4 = log10(HADS_D_w4),
         log10_hadsd_w5 = log10(HADS_D_w5))

# log10() transform Anxiety (hadsa)
MCRcombinedRecoded <- MCRcombinedRecoded %>% 
  mutate(log10_hadsa_w1 = log10(hadsa_w1),
         log10_hadsa_w2 = log10(hadsa_w2),
         log10_hadsa_w3 = log10(hadsa_w3),
         log10_hadsa_w4 = log10(HADS_A_w4),
         log10_hadsa_w5 = log10(HADS_A_w5))

## Inverse Rank Norm transformation (ML suggested) ----
library(RNOmni) #Missingness is not permitted in either the outcome vector y or the model matrix X, therfore I am firstly omitting NAs from hadsd_w1 to see if this actually works. 

# omit NAs as a trial (note this is not okay to do across the board but just drops 5 NAs in hadsd_w1)
MCRcombinedRecoded <- MCRcombinedRecoded %>% 
  mutate(na_omit_hadsd_w1 = na.omit(MCRcombinedRecoded$hadsd_w1))


MCRcombinedRecoded <- MCRcombinedRecoded %>% 
  mutate(rank_hadsd_w1 = RankNorm(hadsd_w1),
         rank_hadsd_w2 = RankNorm(hadsd_w2),
         rank_hadsd_w3 = RankNorm(hadsd_w3),
         rank_hadsd_w4 = RankNorm(HADS_D_w4),
         rank_hadsd_w5 = RankNorm(HADS_D_w5), na.rm=True)

na_omit_hadsd_w1 <- na.omit(MCRcombinedRecoded$hadsd_w1)
rnomni_hadsd_rnomni_w1 <- RankNorm(MCRcombinedRecoded$na_omit_hadsd_w1)

## Histogram of Rank normal log-transformations ----
# I'm not convinced this worked judging by the following histograms (? too many 0 values - i.e. no depression/anxiety)
# hadsa
MCRcombinedRecoded %>% 
  pivot_longer(contains("hadsa")) %>% 
  ggplot(aes(x = value)) +
  geom_histogram(bins = 15) +
  facet_wrap(~name, scales = "free")

# hadsd
MCRcombinedRecoded %>% 
  pivot_longer(contains("hadsd")) %>% 
  ggplot(aes(x = value)) +
  geom_histogram(bins = 15) +
  facet_wrap(~name, scales = "free")

# TMT
MCRcombinedRecoded %>% 
  pivot_longer(contains("tmt")) %>% 
  ggplot(aes(x = value)) +
  geom_histogram(bins = 15) +
  facet_wrap(~name, scales = "free") # no difference between log() or log10() TMT transformations, thus stick with log() as more common

# T-Test of log-transformed TMT results ----
ttest_result_logtmt_w3 <- MCRcombinedRecoded %>% 
  t.test(logTMT_w3 ~ MCR.factor.w3, data = .) %>%
  tidy()
ttest_result_logtmt_w3

ttest_result_logtmt_w4 <- MCRcombinedRecoded %>% 
  t.test(logTMT_w4 ~ MCR.factor.w3, data = .) %>%
  tidy()
ttest_result_logtmt_w4 # significant

ttest_result_logtmt_w5 <- MCRcombinedRecoded %>% 
  t.test(logTMT_w5 ~ MCR.factor.w3, data = .) %>%
  tidy()
ttest_result_logtmt_w5 # significant

# NB for comparison only - this is the ttest of the non-log-transformed TMT result at wave4, still significant but p-value 0.279 rather than 0.008
ttest_result_tmt_w4 <- MCRcombinedRecoded %>% 
  t.test(trailmakingtime_w4 ~ MCR.factor.w3, data = .) %>%
  tidy()
ttest_result_tmt_w4 # significant

# Non-parametric for values not well transformed by log ----
# The Mann-Whitney U test is also called the # Wilcoxon rank-sum test and uses a rank-based # method to compare two groups (note the Wilcoxon # signed-rank test is for paired data). Rank-based # just means ordering your grouped continuous # data from smallest to largest value and assigning # a rank (1, 2, 3 …) to each measurement.

## Histogram, Q-Q plot and boxplot first, for hadsd_w1 to start with.----

(hist_hadsd_w1 <- MCRcombinedRecoded %>% 
  ggplot(aes(x = hadsd_w1)) +
  geom_histogram(bins = 20) +
   facet_wrap("MCR.factor.w3")) 
(qq_hadsd_w1 <- MCRcombinedRecoded %>% 
  ggplot(aes(sample = hadsd_w1)) +
    geom_qq() +
    geom_qq_line(colour = "blue") +
    facet_wrap("MCR.factor.w3") +
    ggtitle("hadsd_w1"))
(box_hadsd_w1 <- MCRcombinedRecoded %>% 
    ggplot(aes(x = , y = hadsd_w1)) +
    geom_boxplot(aes()) +
    facet_wrap( "MCR.factor.w3") +
    xlab("No MCR                  MCR               Missing") +
    ggtitle(
      "MCR w3 and hadsd_w1"
    ))

# patch together using library(patchwork)
hist_hadsd_w1 / qq_hadsd_w1 | box_hadsd_w1

## Wilcoxon rank sum test----
# hadsd_w1
MCRcombinedRecoded %>% 
  wilcox.test(hadsd_w1 ~ MCR.factor.w3, data = .) %>% 
  tidy()

# hadsd_w2
MCRcombinedRecoded %>% 
  wilcox.test(hadsd_w2 ~ MCR.factor.w3, data = .) %>% 
  tidy() # significant

# hadsd_w3
MCRcombinedRecoded %>% 
  wilcox.test(hadsd_w3 ~ MCR.factor.w3, data = .)%>% 
  tidy()

# hadsd_w4
MCRcombinedRecoded %>% 
  wilcox.test(HADS_D_w4 ~ MCR.factor.w3, data = .)%>% 
  tidy()

# hadsd_w5
MCRcombinedRecoded %>% 
  wilcox.test(HADS_D_w5 ~ MCR.factor.w3, data = .)%>% 
  tidy() # significant

## Kruskal-wallis test, non-par test for comparing more than two groups (MCR+/-/missing) ----
# The non-parametric equivalent to ANOVA, is the Kruskal-Wallis test.
# not sure if I should be doing this or what to make of the results, but here is how it's done if so
MCRcombinedRecoded %>% 
  kruskal.test(hadsd_w1 ~ MCR.factor.w3, data = .) %>% 
  tidy()

# FINALFIT approach ----
# https://argoshare.is.ed.ac.uk/healthyr_book/finalfit-approach.html
# firstly define dependent and explanatory variables
# Define dependent and explanatory variables ----
# These can then be used in the Demographics tables (see later)
# To remind myself of the names of the variables and factors (I created the factors) run...
names(MCRcombinedRecoded)

# Define dependent variable for each wave
dependent_w3 = "MCR.factor.w3"
dependent_w4 = "MCR.factor.w4"
dependent_w5 = "MCR.factor.w5"

# define explanatory variables for each wave
explanatory_w1 = c("ageyears_w1", "Sex.factor", "yrsedu_w1", "Social.factor", "bmi_w1", "bld_choles_w1", "fev_w1", "Smoking.factor.w1", "ApoE.factor", "Alcohol.factor.w1", "CVD.factor.w1", "Stroke.factor.w1", "bld_crprot_w1", "bld_hba1c_w1", "vftot_w1", "age70IQ_w1", "hadsd_w1", "hadsa_w1", "Arthritis.factor.w1", "On.meds.factor.w1", "Legpain.factor.w1", "HiBP.factor.w1", "Marital.factor.w1", "alcunitwk_w1", "hadsd_w1", "hadsa_w1", "hadstot_w1", "age11IQ", "lm1_re_w1", "lm2_re_w1", "lmtotal_w1", "vpatotal_w1", "spantot_w1", "matreas_w1", "digback_w1", "nart_w1", "blkdes_w1", "ittotal_w1", "digsym_w1", "srtmean_w1", "griprh_w1", "griplh_w1") # "trailmakingtime_w3" and "Days_significant_exercise_w3" only start in w3; removed "PD.factor.w1" & "Diabetes.factor.w1" as there were no cases of MCR+ in either of these, thus giving a massively large CI and insightly table. 
# including "log_hadsd_w1", "log_hadsa_w1" here leads to an error as there is -Inf NaN -Inf scores here. 

# could use ff_glimpse function to see key info on each
MCRcombinedRecoded %>% 
  ff_glimpse(explanatory_w1)

explanatory_w3 = c("ageyears_w3", "Sex.factor", "yrsedu_w1", "Social.factor", "bmi_w3", "Smoking.factor.w3", "ApoE.factor", "Alcohol.factor.w3", "CVD.factor.w3", "Stroke.factor.w3", "PD.factor.w3", "Diabetes.factor.w3", "trailmakingtime_w3", "bld_crprot_w3", "Days_significant_exercise_w3", "vftot_w3", "age11IQ", "hadsd_w3", "hadsa_w3", "Arthritis.factor.w3", "On.meds.factor.w3", "Legpain.factor.w3", "HiBP.factor.w3", "Marital.factor.w3", "alcunitwk_w3", "hadsd_w3", "hadsa_w3", "hadstot_w3", "age11IQ", "lm1_re_w3", "lm2_re_w3", "lmtotal_w3", "vpatotal_w3", "spantot_w3", "matreas_w3", "digback_w3", "nart_w3", "blkdes_w3", "ittotal_w3", "digsym_w3", "srtmean_w3", "griprh_w3", "griplh_w3") 


# Summarising factors with finalfit ----
# if this is an association study, MCR is my OUTCOME/dependent variable. This is the first step in building a Table1. 

MCRcombinedRecoded %>% 
  summary_factorlist(dependent = "MCR.factor.w3",
                     explanatory = c("ageyears_w3", "Sex.factor", "yrsedu_w1", "Social.factor", "bmi_w3", "Smoking.factor.w3", "ApoE.factor", "Alcohol.factor.w3", "CVD.factor.w3", "Stroke.factor.w3", "PD.factor.w3", "Diabetes.factor.w3", "trailmakingtime_w3", "bld_crprot_w3", "Days_significant_exercise_w3", "vftot_w3", "age70IQ_w1", "hadsd_w3", "hadsa_w3"),                   p = TRUE)
                                  

# MCRw3 assoc w Dementia w5 ----
# Chi-squared / Fisher’s exact test using finalfit. Including p = TRUE in summary_factorlist() adds a hypothesis test to each included comparison. This defaults to chi-squared tests with a continuity correction for categorical variables. Rem: The Chi-Square Test of Independence can only compare categorical variables. It cannot make comparisons between continuous variables or between categorical and continuous variables
# using fisher's instead of Chi2 as numbers are small (<1000 in sample, or <5 in over 20% of the groups) https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5426219/
MCRcombinedRecoded %>% 
  summary_factorlist(dependent = "Dementia.factor.w5",
                     explanatory = "MCR.factor.w3",
                     p = TRUE,
                     p_cat = "fisher") # significant



# Adding further variables
MCRcombinedRecoded %>% 
  summary_factorlist(dependent = "Dementia.factor.w5",
                     explanatory = c("MCR.factor.w3", "Sex.factor", "Smoking.factor.w3", "Alcohol.factor.w3", "Stroke.factor.w3", "CVD.factor.w3"),
                     p = TRUE,
                     p_cat = "fisher")

# Define dependent and explanatory variables ----
# These can then be used in the Demographics tables (see later)
# To remind myself of the names of the variables and factors (I created the factors) run...
names(MCRcombinedRecoded)

# Define dependent variable for each wave
dependent_w3 = "MCR.factor.w3"
dependent_w4 = "MCR.factor.w4"
dependent_w5 = "MCR.factor.w5"

# define explanatory variables for each wave
explanatory_w1 = c("ageyears_w1", "Sex.factor", "yrsedu_w1", "Social.factor", "bmi_w1", "bld_choles_w1", "fev_w1", "Smoking.factor.w1", "ApoE.factor", "Alcohol.factor.w1", "CVD.factor.w1", "Stroke.factor.w1", "bld_crprot_w1", "bld_hba1c_w1", "vftot_w1", "age70IQ_w1", "hadsd_w1", "hadsa_w1", "Arthritis.factor.w1", "On.meds.factor.w1", "Legpain.factor.w1", "HiBP.factor.w1", "Marital.factor.w1", "alcunitwk_w1", "hadsd_w1", "hadsa_w1", "hadstot_w1", "age11IQ", "lm1_re_w1", "lm2_re_w1", "lmtotal_w1", "vpatotal_w1", "spantot_w1", "matreas_w1", "digback_w1", "nart_w1", "blkdes_w1", "ittotal_w1", "digsym_w1", "srtmean_w1", "griprh_w1", "griplh_w1") # "trailmakingtime_w3" and "Days_significant_exercise_w3" only start in w3; removed "PD.factor.w1" & "Diabetes.factor.w1" as there were no cases of MCR+ in either of these, thus giving a massively large CI and insightly table. 
# including "log_hadsd_w1", "log_hadsa_w1" here leads to an error as there is -Inf NaN -Inf scores here. 

# could use ff_glimpse function to see key info on each
MCRcombinedRecoded %>% 
  ff_glimpse(explanatory_w1)

explanatory_w3 = c("ageyears_w3", "Sex.factor", "yrsedu_w1", "Social.factor", "bmi_w3", "Smoking.factor.w3", "ApoE.factor", "Alcohol.factor.w3", "CVD.factor.w3", "Stroke.factor.w3", "PD.factor.w3", "Diabetes.factor.w3", "trailmakingtime_w3", "bld_crprot_w3", "Days_significant_exercise_w3", "vftot_w3", "age11IQ", "hadsd_w3", "hadsa_w3", "Arthritis.factor.w3", "On.meds.factor.w3", "Legpain.factor.w3", "HiBP.factor.w3", "Marital.factor.w3", "alcunitwk_w3", "hadsd_w3", "hadsa_w3", "hadstot_w3", "age11IQ", "lm1_re_w3", "lm2_re_w3", "lmtotal_w3", "vpatotal_w3", "spantot_w3", "matreas_w3", "digback_w3", "nart_w3", "blkdes_w3", "ittotal_w3", "digsym_w3", "srtmean_w3", "griprh_w3", "griplh_w3") 



# Demographics Tables aka Table 1 aka Crosstable----
# https://argoshare.is.ed.ac.uk/healthyr_book/including-missing-data-in-demographics-tables.html This is probably better than the crosstable version below (more detail yet looks cleaner, second way is from finalfit.org)
# firstly, remind myself of names of variables to put in the table!
names(MCRcombinedRecoded) # and/or
MCRcombinedRecoded %>% 
  ff_glimpse(dependent_w3, explanatory_w1)

demog_tab_MCRw3_explanw1 <- MCRcombinedRecoded %>% 
  summary_factorlist(dependent = dependent_w3,
                     explanatory = explanatory_w1,
                     cont_nonpara = 24, # variable 24,ie, "alcunitwk_w1" is to be treated as non-parametric (report median not mean)
                     cont_range = TRUE,               # lower and upper quartile
                     p = TRUE,
                     p_cont_para = "t.test",          # use t.test/aov for parametric
                     p_cat = "fisher",
                     digits = c(1,1,4,2), #1: mean/median, 2: SD/IQR 
                     # 3: p-value, 4: count percentage 
                     na_include = TRUE, # include missing data from the explanatory variables (but not dependent) in final table
                     na_include_dependent = TRUE, # include missing data from the dependent variable
                     total_col = TRUE, #Including a total column 
                     add_col_totals = TRUE, # including column totals
                     add_dependent_label = TRUE, 
  )
demog_tab_MCRw3_explanw1 

# save using here::here - on a Mac you would otherwise do read_csv("data/melanoma.csv") and on Windows you would have to do read_csv("data\melanoma.csv"). Having to include either / (GNU/Linux, macOS) or \ (Windows) in your script means it will have to be changed by hand when running on a different system. What here::here("data_raw", "melanoma.csv"), however, works on any system, as it will use an appropriate one ‘behind the scenes’ without you having to change anything.

save(demog_tab_MCRw3_explanw1, dependent_w3, explanatory_w1,
     file = here::here("mcr_project", "demog_tab_MCRw3_explanw1.rda"))

# Now do the same but with w3 explan variables
demog_tab_MCRw3_explanw3 <- MCRcombinedRecoded %>% 
  summary_factorlist(dependent = dependent_w3,
                     explanatory = explanatory_w3,
                     p = TRUE,
                     p_cat = "fisher",
                     digits = c(1,1,4,2), # no of digits/decimals (defaults 2,2,3) 
                     #1: mean/median, 2: SD/IQR 
                     # 3: p-value, 4: count percentage 
                     na_include = TRUE, # include missing data from the explanatory variables (but not dependent) in final table
                     na_include_dependent = TRUE, # include missing data from the dependent variable
                     total_col = TRUE, #Including a total column 
                     add_col_totals = TRUE, # including column totals
                     add_dependent_label = TRUE)
demog_tab_MCRw3_explanw3

# Save 
save(demog_tab_MCRw3_explanw3, dependent_w3, explanatory_w3,
     file = here::here("mcr_project", "demog_tab_MCRw3_explanw3.rda"))


# This next version defines the dep and exp variables on the go - good for tweaking but involves a lot of copy and pasting - maybe better to use the dep/exp variables as defined a few steps earlier
demog_tab_MCRw3_explanw1 <- MCRcombinedRecoded %>% 
  summary_factorlist(dependent = "MCR.factor.w3",
                     explanatory = c("ageyears_w1", "Sex.factor", "yrsedu_w1", "Social.factor", "bmi_w1", "Smoking.factor.w1", "ApoE.factor", "Alcohol.factor.w1", "CVD.factor.w1", "Stroke.factor.w1", "PD.factor.w1", "Diabetes.factor.w1", "bld_crprot_w1", "vftot_w1", "age70IQ_w1", "log_hadsd_w1", "log_hadsa_w1", "hadstot_w1", "age11IQ", "lm1_re_w1", "lm2_re_w1", "lmtotal_w1", "vpatotal_w1", "spantot_w1", "matreas_w1", "digback_w1", "nart_w1", "blkdes_w1", "ittotal_w1", "digsym_w1", "srtmean_w1", "griprh_w1", "griplh_w1"),
                     p = TRUE,
                     p_cat = "fisher",
                     digits = c(1,1,4,2), #1: mean/median, 2: SD/IQR 
                     # 3: p-value, 4: count percentage 
                     na_include = TRUE, # include missing data from the explanatory variables (but not dependent) in final table
                     na_include_dependent = TRUE, # include missing data from the dependent variable
                     total_col = TRUE, #Including a total column 
                     add_col_totals = TRUE, # including column totals
                     add_dependent_label = TRUE)
demog_tab_MCRw3_explanw1 # NB: `trailmakingtime_w1` and `Days_significant_exercise_w1` don't exist - only started at wave3






# Demographics/Crosstable/Table 1 slightly different way (without as much info as above) ----
# See https://finalfit.org/ for the detail of this next section

# Crosstable with MCRw3 as dependent/outcome variable and w1 explanatory variables ("trailmakingtime_w1", "Days_significant_exercise_w1" don't exist "Diabetes.factor.w1","PD.factor.w1",removed as no people with these in some boxes therefore get wild CIs making it unreadable)
explanatory = c("ageyears_w1", "Sex.factor", "yrsedu_w1", "Social.factor", "bmi_w1", "Smoking.factor.w1", "ApoE.factor", "Alcohol.factor.w1", "CVD.factor.w1", "Stroke.factor.w1",    "bld_crprot_w1", "vftot_w1", "age70IQ_w1", "hadsd_w1", "hadsa_w1")
dependent = 'MCR.factor.w5'
# have a peak at the variables, including missingness
MCRcombinedRecoded %>% 
  ff_glimpse(dependent, explanatory)

xtableMCRw3explanw1 <- MCRcombinedRecoded %>%
  summary_factorlist(dependent, explanatory, 
                     p=TRUE, add_dependent_label=TRUE)
knitr::kable(xtableMCRw3explanw1, align=c("l", "l", "r", "r", "r"))

save(xtableMCRw3explanw1, dependent, explanatory,
     file = here::here("mcr_project", "xtableMCRw3explanw1.rda"))

# Regression table with MCR as categ outcome variable, explan variables at various waves ("multivariable regression as more than one explanatory variable") ----
# https://argoshare.is.ed.ac.uk/healthyr_book/fitting-logistic-regression-models-with-finalfit.html If the outcome variable is correctly specified as a factor, the finalfit() function will run a logistic regression model directly.

# w1 explan variables; MCRw3 (no "Days_significant_exercise_w1" or "trailmakingtime_w1")
# Define dependent variable for each wave
dependent_w3 = "MCR.factor.w3"
dependent_w4 = "MCR.factor.w4"
dependent_w5 = "MCR.factor.w5"

# define explanatory variables for each wave
explanatory_w1reg = c("ageyears_w1", "Sex.factor", "yrsedu_w1", "Social.factor", "bmi_w1", "bld_choles_w1", "fev_w1", "bld_crprot_w1", "bld_hba1c_w1", "vftot_w1", "hadsd_w1", "hadsa_w1", "hadstot_w1", "age11IQ", "lm1_re_w1", "lm2_re_w1", "lmtotal_w1", "vpatotal_w1", "spantot_w1", "matreas_w1", "digback_w1", "nart_w1", "blkdes_w1", "ittotal_w1", "digsym_w1", "srtmean_w1", "griprh_w1", "griplh_w1")
# including "log_hadsd_w1", "log_hadsa_w1" here leads to an error as there is -Inf NaN -Inf scores here. 

explanatory_w1_cont = c("ageyears_w1", "yrsedu_w1", "Social.factor", "bmi_w1", "bld_choles_w1", "fev_w1", "bld_crprot_w1", "bld_hba1c_w1", "vftot_w1", "hadsd_w1", "hadsa_w1", "hadstot_w1", "age11IQ", "lm1_re_w1", "lm2_re_w1", "lmtotal_w1", "vpatotal_w1", "spantot_w1", "matreas_w1", "digback_w1", "nart_w1", "blkdes_w1", "ittotal_w1", "digsym_w1", "srtmean_w1", "griprh_w1", "griplh_w1")


explanatory_w3 = c("ageyears_w3", "Sex.factor", "yrsedu_w1", "Social.factor", "bmi_w3", "Smoking.factor.w3", "ApoE.factor", "Alcohol.factor.w3", "CVD.factor.w3", "Stroke.factor.w3", "PD.factor.w3", "Diabetes.factor.w3", "trailmakingtime_w3", "bld_crprot_w3", "Days_significant_exercise_w3", "vftot_w3", "age11IQ", "hadsd_w3", "hadsa_w3", "Arthritis.factor.w3", "On.meds.factor.w3", "Legpain.factor.w3", "HiBP.factor.w3", "Marital.factor.w3", "alcunitwk_w3", "hadsd_w3", "hadsa_w3", "hadstot_w3", "age11IQ", "lm1_re_w3", "lm2_re_w3", "lmtotal_w3", "vpatotal_w3", "spantot_w3", "matreas_w3", "digback_w3", "nart_w3", "blkdes_w3", "ittotal_w3", "digsym_w3", "srtmean_w3", "griprh_w3", "griplh_w3")

dependent <- "MCR.factor.w5"
explanatory <- c("ageyears_w1", "yrsedu_w1", "Social.factor", "ittotal_w1", "digsym_w1", "bld_hba1c_w1", "vftot_w1", "hadsd_w1")


explanatory_sex <- c("ageyears_w1", "Sex.factor", "yrsedu_w1", "ittotal_w1", "digsym_w1", "bld_hba1c_w1", "vftot_w1")
MCRcombinedRecoded %>% 
  finalfit(dependent_w5, explanatory, metrics = TRUE) # metrics = TRUE gives us AIC (HealthyR 7.3.3, aim for min), c-statistic (9.7.1, aim for max) and Hosmer-Lemeshow test (aim for non-sig)



MCRcombinedRecoded %>% 
  select(all_of(explanatory_w1_cont)) %>% 
  pivot_longer(-ageyears_w1) %>% 
  ggplot(aes(value, age)) +
  geom_boxplot() +
  facet_wrap(~name, scales = 'free', ncol = 3) +
  coord_flip()

# could use ff_glimpse function to see key info on each
MCRcombinedRecoded %>% 
  ff_glimpse(explanatory_w1)

regtableMCRw3_explanw1_v2 <- MCRcombinedRecoded %>%
  finalfit(dependent_w3, explanatory_w1reg, metrics = TRUE) #finalfit is a logistic regression!
knitr::kable(regtableMCRw3_explanw1_v2 [[1]], row.names = FALSE, align=c("l", "l", "r", "r", "r", "r"))
knitr::kable(regtableMCRw3_explanw1_v2 [[2]], row.names = FALSE, col.names = "")

regtableMCRw3_explanw1_v2 

save(regtableMCRw3_explanw1,
     file = here::here("mcr_project", "regtableMCRw3_explanw1.rda"))

# w1 explan variables; MCRw5 (no "Days_significant_exercise_w1" or "trailmakingtime_w1")
explanatory = c("ageyears_w1", "Sex.factor", "Social.factor", "yrsedu_w1", "Smoking.factor.w1", "ApoE.factor", "Alcohol.factor.w1", "CVD.factor.w1", "Stroke.factor.w1", "Diabetes.factor.w1", "bld_crprot_w1", "vftot_w1", "age70IQ_w1", "hadsd_w1", "hadsa_w1")
dependent = 'MCR.factor.w5'
regtableMCRw5_explanw1 <- MCRcombinedRecoded %>%
  finalfit(dependent, explanatory, metrics = TRUE)
knitr::kable(regtableMCRw5_explanw1[[1]], row.names = FALSE, align=c("l", "l", "r", "r", "r"))
knitr::kable(regtableMCRw5_explanw1[[2]], row.names = FALSE, col.names = "")

save(regtableMCRw5_explanw1, dependent, explanatory,
     file = here::here("mcr_project", "regtableMCRw5_explanw1.rda"))


# w3 explan variables; MCRw3 - requires editing to change ORs to max of 3 or 4 digits for readability purposes
regtableMCRw5_explanw3 <- MCRcombinedRecoded %>%
  finalfit(dependent_w3, explanatory_w3, metrics = TRUE)
knitr::kable(regtableMCRw5_explanw3[[1]], row.names = FALSE, align=c("l", "l", "r", "r", "r"))
knitr::kable(regtableMCRw5_explanw3[[2]], row.names = FALSE, col.names = "")

regtableMCRw5_explanw3

save(regtableMCRw5_explanw3, dependent, explanatory,
     file = here::here("mcr_project", "regtableMCRw5_explanw3.rda"))

# OR plots ----
# MCRw3 and explan variables w1
ORplotMCRw3_explanw1 <- MCRcombinedRecoded %>% 
  or_plot(dependent, explanatory, 
          breaks = c(0.5, 1, 5, 10, 15))
save(ORplotMCRw3_explanw1, dependent, explanatory,
     file = here::here("mcr_project", "dependent_w3", "explanatory_w1", "ORplotMCRw3_explanw1.rda"))

# MCRw3 and explan variables w3

# MCRw3 and explan variables w1
ORplotMCRw5_explanw3 <- MCRcombinedRecoded %>% 
  or_plot(dependent, explanatory, 
          breaks = c(0.5, 1, 5, 10, 15))

# save all these tables as output for knitting to word in RMarkdown https://finalfit.org/articles/export.html - probably need to do one of these for each combo of dependent and explanatory variables
save(xtableMCRw5, regtableMCRw5, ORplotMCRw5_explanw3, dependent, explanatory, file = "MCRw5explanw3out.rda")

# Crosstable with MCRw5 as dependent/outcome variable and w3 explanatory variables
explanatory = c("ageyears_w3", "Sex.factor", "yrsedu_w1", "Social.factor", "bmi_w3", "Smoking.factor.w3", "ApoE.factor", "Alcohol.factor.w3", "CVD.factor.w3", "Stroke.factor.w3", "PD.factor.w3", "Diabetes.factor.w3", "trailmakingtime_w3", "bld_crprot_w3", "Days_significant_exercise_w3", "vftot_w3", "age70IQ_w1", "hadsd_w3", "hadsa_w3")
dependent = 'MCR.factor.w5'
# have a peak at the variables, including missingness
MCRcombinedRecoded %>% 
  ff_glimpse(dependent, explanatory)

xtableMCRw5 <- MCRcombinedRecoded %>%
  summary_factorlist(dependent, explanatory, 
                     p=TRUE, add_dependent_label=TRUE)
knitr::kable(t1, align=c("l", "l", "r", "r", "r"))


#MODELLING----
## Continuous outcome variable ----
glimpse(MCRcombinedRecoded)
missing_glimpse(MCRcombinedRecoded)
ff_glimpse(MCRcombinedRecoded)

# 1st plot the vftot_w1 variable to check for normality etc

MCRcombinedRecoded %>% 
  ggplot(aes(x = vftot_w1)) +
  geom_histogram() +
  facet_grid("MCR.factor.w3") # looks normal

# checking some other cont cog variables out of curiosity
MCRcombinedRecoded %>% 
  ggplot(aes(x = lm1_re_w1)) +
  geom_histogram()

MCRcombinedRecoded %>% 
  ggplot(aes(x = lm2_re_w1)) +
  geom_histogram()

# Quantile-quantile (Q-Q) plot
MCRcombinedRecoded %>% 
  ggplot(aes(sample = vftot_w1)) + # Q-Q plot requires 'sample'
  geom_qq() + # defaults to normal distribution
  geom_qq_line(colour = "blue") + # add the theoretical line
  facet_grid("MCR.factor.w3")









# library(finalfit) # glimpse, missing_glimpse, ff_glimpse
getOption("max.print") # determines what max print number is (default 1000 rows/observations)
options(max.print=3000)

glimpse(MCRcombinedRecoded)

missing_glimpse(MCRcombined)
ff_glimpse(MCRcombined)

logistic <- glm(mcr_mem_w3 ~ mcr_ipip_w1, data = MCRcombined, family = "binomial")
summary(logistic)

log_all <- glm(dement_w3 ~ ., data = MCRcombined, family = "binomial")
summary(log_all)
# Create function theme.clean to make nice-looking graphs with consistent formatting
theme.clean <- function(){
  theme_bw()+
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14, face = "plain"),             
          axis.title.y = element_text(size = 14, face = "plain"),             
          panel.grid.major.x = element_blank(),                                          
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
          legend.text = element_text(size = 12, face = "italic"),          
          legend.position = "right")
}

# plot the covariation 
# ipip28_w3 to memprob1_w3
ggplot(MCRcombined, aes(x = mcr_mem_w3, y =  mcr_ipip_w3)) +
  geom_point(aes()) +
  labs(x = "mcr_mem_w3", y = "mcr_ipip_w3") +
  stat_smooth(method = "lm", aes() +    # adding regression lines 
  scale_colour_manual(values = c("#FFC125", "#36648B")) +
  scale_fill_manual(values = c("#FFC125", "#36648B")) +
    theme.clean())

# ipip28_w1 to mmse_w1
ggplot(lbc_male, aes(x = scc_male_ipip28_w1, y = mmse_w1)) +
  geom_point(aes()) +
  labs(x = "ipip28_w1", y = "mmse_w1") +
  stat_smooth(method = "lm", aes() +    # adding regression lines 
                scale_colour_manual(values = c("#FFC125", "#36648B")) +
                scale_fill_manual(values = c("#FFC125", "#36648B")) +
                theme.clean())


# memprob1_w3 to wemwb7_w2
ggplot(lbc_male, aes(x = scc_male_memprob1_w3, y = scc_male_wemwbs7_w3)) +
  geom_point(aes()) +
  labs(x = "memprob1_w3", y = "wemwbs7_w3") +
  stat_smooth(method = "lm", aes() +    # adding regression lines 
                scale_colour_manual(values = c("#FFC125", "#36648B")) +
                scale_fill_manual(values = c("#FFC125", "#36648B")) +
                theme.clean()) # this graph shows a linear relationship, with wemwbs7 more likely to be closer to 1

# as it values can only be four separate combos, use geom_count to show numbers of each:
ggplot(lbc_male, aes(x = scc_male_memprob1_w3, y = scc_male_wemwbs7_w3)) +
  geom_count(aes(scc_male_memprob1_w3, scc_male_wemwbs7_w3)) +
  labs(x = "memprob1_w3", y = "wemwbs7_w3")



# attempt to run linear model of ipip and memprob but they're both binomial so this isn't the right choice
# ipipmemprob <- lm(lbc_male$scc_male_ipip28_w3 ~ lbc_male$scc_male_memprob1_w3, data = lbc_male)


# count the number of observations for each combination using geom_count
ggplot(lbc_male) +
  geom_count(mapping = aes(x = scc_male_ipip28_w3, y = scc_male_memprob1_w3) +
               geom_point())

ggplot(lbc_male) +
  geom_count(mapping = aes(x = scc_male_wemwbs7_w3, y = scc_male_memprob1_w3))

lbc_male %>% 
  count(scc_male_ipip28_w3, scc_male_memprob1_w3)

lbc_male %>% 
  count(scc_male_ipip28_w3, scc_male_memprob1_w3) %>% 
  ggplot(aes(scc_male_ipip28_w3, scc_male_memprob1_w3)) +
  geom_tile(aes(fill = n))

# Modelling using baseR https://sphweb.bumc.bu.edu/otlt/MPH-Modules/BS/R/R-Manual/R-Manual18.html
cor.test(lbc_male$scc_male_ipip28_w2, lbc_male$scc_male_wemwbs7_w2, method = "pearson")

cor.test(lbc_male$scc_male_ipip28_w2, lbc_male$scc_male_wemwbs7_w2, method = "spearman")
cor.test

# Modelling using RSTATIX package https://rpkgs.datanovia.com/rstatix/ - pipe friendly tests with results as tibbles

# library(rstatix)

# Subset smaller dataset of relevant SCC measures----
model_data_m <- lbc_male %>% 
  select(scc_male_ipip28_w2, scc_male_ipip28_w3, scc_male_ipip28_w4, scc_male_ipip28_w5, scc_male_wemwbs7_w2, scc_male_wemwbs7_w3, scc_male_wemwbs7_w4, scc_male_wemwbs7_w5, scc_male_memprob1_w3, scc_male_memprob1_w4, scc_male_memprob1_w5, scc_any_male_w1, scc_any_male_w2, scc_any_male_w3, scc_any_male_w4, scc_any_male_w5)
head(model_data_m, 3)


# this is for demonstration as Pearson and Spearman are for Quantitative data (Pearson = Parametric/Normal, Spearman = non-normal) whereas my SCC variables are Categorical (specifically dichotomous, aka binary, aka yes/no) so it may be better to do Fisher's exact test although ML suggests to use Pearson for correlation tests, regardless. log transform/inverse log transform non-para continuous variables if necessary  
# https://help.xlstat.com/s/article/which-statistical-test-should-you-use?language=en_US 



model_data_m %>% cor_test(scc_male_ipip28_w2, scc_male_wemwbs7_w2, method = "pearson")
model_data_m %>% cor_test(scc_male_ipip28_w2, scc_male_wemwbs7_w2, method = "spearman")

model_data_m %>% cor_test(scc_male_ipip28_w3, scc_male_wemwbs7_w3, method = "pearson")
model_data_m %>% cor_test(scc_male_memprob1_w3, scc_male_wemwbs7_w3, method = "pearson")
model_data_m %>% cor_test(scc_male_memprob1_w3, scc_male_wemwbs7_w3, method = "spearman")

model_data_m %>% cor_test(scc_male_memprob1_w4, scc_male_wemwbs7_w4, method = "pearson")
model_data_m %>% cor_test(scc_male_memprob1_w4, scc_male_wemwbs7_w4, method = "spearman")

model_data_m %>% cor_test(scc_male_memprob1_w5, scc_male_wemwbs7_w5, method = "pearson")
model_data_m %>% cor_test(scc_male_memprob1_w5, scc_male_wemwbs7_w5, method = "spearman")

model_data_m %>% cor_test(scc_male_memprob1_w3, scc_male_wemwbs7_w5, method = "pearson")
model_data_m %>% cor_test(scc_male_memprob1_w3, scc_male_memprob1_w4, method = "pearson")

# Pairwise correlation test between all subj cog variables variables (remove lbc36no variable from model_data_m before running as it is not a numeric vector (like all the others)) ----
# ML says Pearsons is better for correlation, full-stop!
# Check correlations of each SCC measure to each other - probably with memprob as gold standard
pairwise_male <- model_data_m %>% cor_test(method = "pearson")
pairwise_male

# Correlate scc measures to an obj cog measure mmse_w3/4/5
glimpse(lbc_male)

lbc_male %>% cor_test(scc_male_memprob1_w5, mmse_w5, method = "pearson")
lbc_male %>% cor_test(scc_male_memprob1_w5, mmse_w5, method = "spearman")
lbc_male %>% cor_test(scc_male_wemwbs7_w5, mmse_w5, method = "spearman")
lbc_male %>% cor_test(scc_male_wemwbs7_w5, mmse_w5, method = "spearman")
lbc_male %>% cor_test(scc_male_ipip28_w5, mmse_w5, method = "spearman")
lbc_male %>% cor_test(scc_male_wemwbs7_w5, mmse_w5, method = "spearman")



# attempt at fisher's exact test for count data ("is the presence of scc_male_ipip28 linked to the presence of scc_male_wemwbs7?") https://argoshare.is.ed.ac.uk/healthyr_book/fishers-exact-test.html
lbc_male %$%
  table(scc_male_ipip28_w2, scc_male_wemwbs7_w2) %>%
  fisher.test()



# ANOVA of MCR with mmse as response variable - no point in doing this as MMSE is incorporated in MCR (you won't find any MCR+ ppl with MMSE <24, by definition!)
# Data visualisation

theme.mmse <- function(){  # Creating a function
  theme_classic() +  # Using pre-defined theme as base
    theme(axis.text.x = element_text(size = 12, face = "bold"),  # Customizing axes text      
          axis.text.y = element_text(size = 12, face = "bold"),
          axis.title = element_text(size = 14, face = "bold"),  # Customizing axis title
          panel.grid = element_blank(),  # Taking off the default grid
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          legend.text = element_text(size = 12, face = "italic"),  # Customizing legend text
          legend.title = element_text(size = 12, face = "bold"),  # Customizing legend title
          legend.position = "right",  # Customizing legend position
          plot.caption = element_text(size = 12))  # Customizing plot caption
}                                                                     
# Creating a histogram with ggplot
(mmse_histogram <- ggplot(lbc_male, aes(x = mmse_w1)) +  
    # Plotting from the tidy data frame          
    geom_histogram(stat = "count") +
    # Makes height of bars proportional to number of cases in each group
    geom_vline(aes(xintercept = mean(mmse_w1)),     
               colour = "red", linetype = "dashed", size = 1) +
    # Adding a line for mean abundance
    scale_fill_manual(values = c("#97F7C5", "#4ED973", "#08873D")) +          
    # Adding custom colours    
    labs(x = "\n MMSE score", y = "Frequency \n",                   
         # Adding x and y axis labels.
         # "\n" adds space before x and after y axis text
         caption = "\n Fig.1 Response variable (MMSE w1 males) is not normally distributed. Red dashed
                         line shows mean MMSE score.") +   
    # Adding informative figure caption
    # caption = "\n Fig.1") +  # Adding caption for figure in panel
    theme.clean() +  # Adding our personalised theme
    guides(fill = guide_legend(title = "MMSE score)")))
# Adding an informative legend title

# Fit Linear model to all our data ignoring confounding variables
basic.anyscc.lm <-lm(mmse_w1 ~ mcr_male_anyscc_w1, data = lbc_male)
summary(basic.anyscc.lm)

basic.wemwbs.lm <-lm(mmse_w1 ~ mcr_male_wemwbs7_w2, data = lbc_male)
summary(basic.wemwbs.lm)

lbc_male$mcr_male_wemwbs7_w2

## Mixed effects models----
# Install Mixed effect models package

library(lme4) # good one to use - https://www.r-bloggers.com/2017/04/choosing-r-packages-for-mixed-effects-modelling-based-on-the-car-you-drive/



# ARCHIVE ----

# move specific column(s) to the front of the tibble for ease of viewing
# MCRdata %>% 
#  select(lbc36no, agedays_w1, agedays_w2, agedays_w3, agedays_w4, agedays_w5, sex, everything())


# 5.1 males calculate adl_w1 score >= 1.5 SD from the mean (i.e. exclude anyone scoring higher than 1.5SD from the mean, higher scores = greater disability)


# Initially I planned to choose define those with a score <= 1.5D of mean as independent (lower score = better function) - for recap of how Townsend works - see problems with this variable https://www-karger-com.ezproxy.is.ed.ac.uk/Article/Abstract/271603) - as per Miles's paper take scores 1.5 SD less than the mean as indpendent (https://journals.lww.com/alzheimerjournal/Fulltext/2021/07000/Prevalence_of_Mild_Cognitive_Impairment_in_the.6.aspx). However, data of adl_w1 was not normally distributed. Doing SD and 1.5SD didn’t make much sense. Mean for male cohort was 3.1666 and one SD was 2.918 therefore anyone scoring even a 1 (which is not functionally impaired) would have been ruled out as functionally impaired. As such, better to select anyone with adl_w1 <= 1. 

# males calculate for adl_w1 score <= 1.5 SD from the mean
# calculate mean and store as a value
# meanM_ADL <- lbcMslowSCCnoDem$adl_w1 %>% 
#  mean() 
# calculate SD and store
#sdM_ADL <-lbcMslowSCCnoDem$adl_w1 %>% 
#  sd()

# calculate 1.5 SD and store
#sd1.5M_ADL <- (sdM_ADL/2) + sdM_ADL

# calculate mean minus 1.5 SD
#meanM_ADL - sd1.5M_ADL # answer makes no sense - it's a negative number as data is non-parametric!





