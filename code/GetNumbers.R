# credits  ---------------------------------------------------------------------
'
Author:     I. Fetzer
Purpose:    This R script generates all numbers found in the text of the paper. 
Note:       Some numbers were already been extracted during the merging process 
            of the three data set (SP Capital IQ, Compustat and data from 
            https://amirsufi.net/data-and-appendices/CSTATVIOLATIONS_NSS_20090701.dta).
            The specific code lines producing the numbers are marked with the header:
            ## !fact check! ---------------- 
            in the GeDataset.R file. 
            The file "my_dataset.csv" -obtained by the R skript "GetDataset.R"- 
            is required to produce all tables.
            Please make sure to readjust the working directory path in line 20
            before running the code. 
' 
# 1 initialization -------------------------------------------------------------
rm(list=ls(all=TRUE))         # clear environment
graphics.off()                # clear console
# set working directory 
setwd("~/Library/Mobile\ Documents/com~apple~CloudDocs/Uni/BA - icloud")
path <-"data" # set path
# install & load packages
libraries = c( "dplyr", "tidyverse") 
lapply(libraries, function(x) if (!(x %in% installed.packages())) 
{ install.packages(x) })
lapply(libraries, library, quietly = TRUE, character.only = TRUE)



# 2 load in datatset -----------------------------------------------------------
data <- read.csv("data/my_dataset.csv") 


# 3 numbers  -------------------------------------------------------------------
# 3.1 about time period
# What is the calendar time period of the dataset? 
sort(unique(data$calendar)) # 1997Q2 - 2009Q2
# What is the fiscal time period of the dataset? 
sort(unique(data$quarter)) # 1997Q1 - 2009Q4

# 3.2 about firms --------------------------------------------------------------
# How many firms does the sample consist of?
length(unique(data$gvkey)) # 7075
# How many firms does the sample consist of, when dropping missing values for 
# all violation dummies (i.e. dropping observations for 2008Q3-2009Q2 (calendar))?
data %>% filter(calendar <= 20082) %>% drop_na(viol) %>% select(gvkey) %>% distinct() %>% nrow() # 7075
# How many firms have violated a debt covenant at some point over the sample period?
data %>% filter(viol_once == 1 | viol_multi ==1) %>% select(gvkey) %>% distinct() %>% nrow() # 2846
# percentage of all firms:
2846/7075*100 # 40.22615
# Control: How many firms have not violated a debt covenant at some point over the sample period? (No-violators)
data %>% filter(viol_once == 0 & viol_multi == 0) %>% select(gvkey) %>% distinct() %>% nrow() # 4229
# percentage of all firms:
4229/7075*100 # 59.77385
# How many firms have violated a a debt covenant ONCE at some point over the sample period? (One-time violators)
data %>% filter(viol_once == 1) %>% select(gvkey) %>% distinct() %>% nrow() # 708
# percentage of all firms:
708/7075*100 # 10.00707
# How many firms have violated a a debt covenant more than once at some point over the sample period? (Frequent violators)
data %>% filter(viol_multi == 1) %>% select(gvkey) %>% distinct() %>% nrow() # 2138
# percentage of all firms:
2138/7075*100 # 30.21908
# How many firms have violated a NEW debt covenant at some point over the sample period?
data %>% filter(viol_new ==1 & (viol_multi ==1 | viol_once ==1)) %>% select(gvkey) %>% distinct() %>% nrow() # 2425
# percentage of all firms:
2425/7075*100 #  34.27562
# percentage of all violator firms: 
2425/2846*100 # 85.20731

# 3.3 about quarterly observations ---------------------------------------------
# How many quarterly observation does the sample consist of in total?
nrow(data) # 174984
# How many quarterly observation does the sample consist of, when dropping missing values for 
# all violation dummies(i.e. dropping observations for 2008Q3-2009Q2 (calendar))?
data %>% filter(calendar <= 20082) %>% drop_na(viol) %>% nrow() # 162100
# How many firm-quarter observations with debt covenant violation does the sample count? 
data %>% filter(viol == 1) %>% nrow() # 11236
# percentage of all observations: 
11236/174984*100 # 6.421159 
# percentage of all observations excluding those for with no debt covenant data is available: 
11236/162100*100 # 6.931524
# How many firm-quarter observations with NEW debt covenant violation does the sample count? 
data %>% filter(viol_new == 1) %>% nrow() # 3227
# percentage of all observations: 
3227/174984*100 # 1.844169
# percentage of all observations excluding those for with no debt covenant data is available: 
3227/162100*100 # 1.990746
# How many firm-quarter observations does a firms have on average in full sample (without excluding 2008Q3-2009Q2)?
data %>% group_by(gvkey) %>% summarise(n = n()) %>% select(n) %>% summarise(mean = mean(n)) # 24.7
# How many firm-quarter observations does a one-time violator firm have on average in the sample?
data %>% filter(viol_once == 1) %>% nrow()/710 # 23.19577
# How many firm-quarter observations does a frequent violator firm have on average in the sample?
data %>% filter(viol_multi == 1) %>% nrow()/2142 # 27.12979

# 3.4 other --------------------------------------------------------------------
# How many firms and firm-quarter observations are included for Figure 1 (sample period 1997Q2-2007Q4 (calendar)) ?
# firm- quarters: 
data %>% filter(quarter <= 20074 & quarter >= 19971) %>% nrow() # 161857
# Control: How many quarterly observations are excluded from Figure 1 (2008Q1, 2008Q2)
data %>% filter(quarter <= 20082) %>% drop_na(viol) %>% filter(quarter > 20074 | quarter < 19971) %>% nrow() # 243
243 + 161857 # 162100
# no of firms: 
data %>% filter(quarter <= 20074 & quarter >= 19971) %>% select(gvkey) %>% distinct() %>% nrow() # 7074






