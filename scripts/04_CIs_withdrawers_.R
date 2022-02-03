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
