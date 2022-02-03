# Title: LBC waves 1 - 5, Deriving the MCR variable
# Donncha Mullin (d.mullin@ed.ac.uk)
# Following coding etiquette https://ourcodingclub.github.io/tutorials/etiquette/index.html
Sys.time()
Sys.Date()

# Set wd----
setwd("/Users/dmullin/Dropbox/Academic/PhD/LBC")

# Packages----
install.packages("haven")
install.packages("DescTools")
install.packages("naniar")
library(haven) # loading of .sav file
library(tidyverse) # formatting data for analysis (includes ggplot)
library(DescTools) # descriptive statistics and exploratory data analysis
library(naniar) # replace_with_na function amongst others 

# Load data----
RawLBCdata <- read_sav("LBC1936_GaitSpeedw1to5.sav")
RawLBCdata


# Pre-processing of LBC data----

# First, list the various NA values used in LBC: 
na_values <- c(-777, -777.00, -888, -888.00, -999, -999.00, 
                -999.000, 88, 98, 99, 999, 999.00, 999.000) 
# Use naniar package to convert this vector of values all to NA - This didn't work despite following steps here https://cran.r-project.org/web/packages/naniar/vignettes/replace-with-na.html
# RawLBCdata %>% 
# replace_with_na_all(condition = ~.x %in% na_values)
# View(RawLBCdata)

# Base R approach, did not work either, even when uninstalled all packages bar haven.
# RawLBCdata[RawLBCdata == 999.00] <- NA
# rlang::last_error()
# rlang::last_trace()

# more targeted replace na's of one type of NA variable didn't work either
# RawLBCdata %>% 
#  replace_with_na_all(condition = ~.x == "-777")
# Error: Can't combine `..1` <character> and `..2` <double>.

# attempt using dplyr and naniar combined - did not work!
# MCRdata <- RawLBCdata %>% 
#  mutate_if(is.numeric, replace_with_na(RawLBCdata, replace = list(na_values)))

# Recode all the NAs as the same thing across the data frame (https://dplyr.tidyverse.org/reference/na_if.html) and name this new tibble MCRdata - this does not work - I wondered if it was to do with is.character but it does not run if this is changed to is.numeric. 

# Mark Adam's tip
# modify any column that is a double. when an element in the column is equal to any of the column's label values, replace it with NA, otherwise convert the remaining elements to numeric (stripping out the label information) - from Mark Adams. Note this turns all the variables in sex, blood pressure, stroke and many more to NA as they must have been seen as str numeric + label by R. This problem seemed to arise from importing the data as sav using haven. 

#MCRdata <-
#  RawLBCdata %>%
#  mutate(across(where(is.double), ~if_else(. %in% attr(., "labels"), true=NA_real_, false=as.numeric(.))))


RawLBCdata %>% 
  mutate(across(where(is.double), ~if_else(. %in% attr(., "labels"), true=NA_real, false=as.numeric(.))))

MCRdata <- RawLBCdata %>%           
  mutate(across(where(is.character), ~na_if(., "-777"))) %>% 
  mutate(across(where(is.character), ~na_if(., "-777.00"))) %>% 
  mutate(across(where(is.character), ~na_if(., "-888"))) %>% 
  mutate(across(where(is.character), ~na_if(., "-888.00"))) %>% 
  mutate(across(where(is.character), ~na_if(., "-999"))) %>% 
  mutate(across(where(is.character), ~na_if(., "-999.00"))) %>% 
  mutate(across(where(is.character), ~na_if(., "-999.000"))) %>% 
  mutate(across(where(is.character), ~na_if(., "88"))) %>%
  mutate(across(where(is.character), ~na_if(., "98"))) %>% 
  mutate(across(where(is.character), ~na_if(., "99"))) %>% 
  mutate(across(where(is.character), ~na_if(., "999"))) %>% 
  mutate(across(where(is.character), ~na_if(., "999.00"))) %>% 
  mutate(across(where(is.character), ~na_if(., "999.000"))) %>% 
  mutate(across(where(is.character), ~na_if(., "100"))) %>% 
  mutate(across(where(is.character), ~na_if(., ".")))    


