# credits  ---------------------------------------------------------------------
'
Author:     I. Fetzer
Purpose:    This R skript retrieves and constructs the main data for my thesis. 
            It first extracts accounting data for U.S. non-financial firms from 
            S&P Capital IQ/Compustat database, which are formated, filtered and 
            finally merged onto a firm-quarter data set of financial covenant 
            violation indicators constructed from SEC filings and provided as 
            supplementary material to Greg Nini, David Smith, and Amir Sufi 2012 
            paper "Creditor Control Rights, Corporate Governance, and Firm Value" 
            (The Review of Financial Studies, 25(6)) at https://amirsufi.net/chronology.html. 
            
Note:       Please make sure to readjust the working directory path in line 20
            before running the code. 
' 
# 1 initialization -------------------------------------------------------------
rm(list=ls(all=TRUE))         # clear environment
graphics.off()                # clear console
# set working directory 
setwd("~/Library/Mobile\ Documents/com~apple~CloudDocs/Uni/BA - icloud")
path <-"data" # set path
# install & load packages
libraries = c("haven", "dplyr", "tidyverse", "readxl", "labelled", "Hmisc", 
              "here", "devtools", "stringr", "gridExtra", "stargazer", "lubridate", 
              "cowplot", "sjPlot","DescTools", 
              "plm", "lmtest", "fixest", "texreg") # for regression)
lapply(libraries, function(x) if (!(x %in% installed.packages())) 
  { install.packages(x) })
lapply(libraries, library, quietly = TRUE, character.only = TRUE)
devtools::devtoolsinstall_github("weiyangtham/twydyverse")
library(twydyverse)



# 2 load data ------------------------------------------------------------------
## 2.1 by research paper ----
# Nini,G., Smith,D.C., Sufi,A. (2012): Creditor Control Rights, Corporate 
# Governance, and Firm Value, in: The Review of Financial Studies, 25(6), 
# p. 1713–1761, https://amirsufi.net/chronology.html'
viol <- read_dta(file = file.path(path, "by Amir Sufi", 
                                  "cstatviolations_nss_20090701.dta"))
## 2.2 by my professor ---------------------------------------------------------
# supplementary data set from Compustat
suppl <- read.csv(file = file.path(path, "by Dr. Mariano", "data.csv"))
## 2.3 by S&P Capital IQ -------------------------------------------------------
# without having made any modifications in Excel!
folder_capitaliq <- list.files(
  path = file.path(here(), path, "by S&P Capital IQ"), 
  pattern = '.xls')



# 3 load sub-functions ---------------------------------------------------------
# The formatting() sub-function takes a data frame as its only argument. 
# It cleans the data in three steps: 
# (1) deleting the suffixes of our variable names, e.g. 
# "Revenue - Compustat [FQ1 1996] ($USDmm, Historical rate)" -> "Revenue11996"
# or
# "Total Asset Turnover - Compustat [FQ2 1996]" ->  ""TotalAssetTurnover21996"
# (2) discards the variable entries for "Exchange Ticker" and 
# (3) corrects variable classes. The function returns a modified data frame.  
formatting <- function(df){
  # adjust column names
  if(sum((grepl("[[:punct:]]", names(df)) == FALSE)) != length(colnames(df))) {
    # delete all non-alphanumeric characters
    colnames(df) <- gsub("[[:punct:]]|(USDmm.*)|\\s|(Compustat)|(CapitalIQ)|([m]{2})\\>|FQ","", 
                         colnames(df)) 
  }
  
  # drop irrelevant columns
  df <- df[,!(names(df) %in% c("ExchangeTicker"))]
  
  # adjust variable classes 
  strings <- c("CompanyName", "StateofIncorporation", "CountryRegionofIncorporation", 
               "CompanyType")
  numerics <- names(df)[!names(df) %in% strings]
  df <- df %>% mutate_at(numerics, ~as.numeric(.)) 
  
  return(df)
}

# The filtering() sub-function takes a data frame as its only argument. 
# It runs first filter mechanisms that ensure that our data set only contains ...
# (1) public & private firms that 
# (2) ... have a unique key available (gvkey)
# (3) ... are not operating in finance, insurance, real estate, construction, mining industry
# (4) ... country of incorporation is the US.
# The function returns a modified data frame. 
filtering <- function(df){
  # check validity of df
  if (sum((grepl("[[:punct:]]", names(df)) == FALSE)) != length(colnames(df))) {       
    stop("Please call function <<formatting>> first.")
  } 
  # erase all non-firms
  df <- df  %>% filter(CompanyType %in% c("Public Company","Private Company",
                                          "Private Investment Firm", 
                                          "Public Investment Firm"))
  # erase all firms without gvkey 
  df <- df[!is.na(df$GVKEY), ]
  
  # erase all firms in finance, insurance, real estate, construction, mining industry
  df <- df  %>% filter(SICCodesPrimaryCodeOnly < 6000 | SICCodesPrimaryCodeOnly > 6999) 
  # erase all non-US firms 
  us_states <- c("Alabama","Alabama","Alaska","Arizona","Arkansas","California",
                 "Colorado","Connecticut","Delaware","Florida","Georgia","Hawaii",
                 "Idaho", "Illinois", "Indiana" ,"Iowa" ,"Kansas" ,"Kentucky" ,
                 "Louisiana" ,"Maine" ,"Maryland" ,"Massachusetts" ,"Michigan" ,
                 "Minnesota" ,"Mississippi" ,"Missouri" ,"Montana Nebraska" ,"Nevada" ,
                 "New Hampshire" ,"New Jersey" ,"New Mexico" ,"New York" ,
                 "North Carolina" ,"North Dakota" ,"Ohio" ,"Oklahoma" ,"Oregon" ,
                 "Pennsylvania" ,"Rhode Island","South Carolina" ,"South Dakota" ,
                 "Tennessee" ,"Texas" ,"Utah" ,"Vermont" ,"Virginia" ,"Washington" ,
                 "West Virginia" ,"Wisconsin" ,"Wyoming")
  df <- df %>% filter(CountryRegionofIncorporation == "United States" | ((StateofIncorporation %in% us_states) & is.na(CountryRegionofIncorporation)) )
  return(df)
}

# The transform_into_panel() sub-function takes a data frame as its only argument. 
# Its aim is to convert the input data frame from wide format to long format.
# E.g.
# Column 1                | Column 2                | Column 3
# TotalAssetTurnover11996 | TotalAssetTurnover21996 | TotalAssetTurnover31996
# 0.1                     | 0.2                     | 0.3
# becomes: 
# Column 1      | Column 2
# Year          | TotalAssetTurnover
# 11996         | 0.1
# 21996         | 0.2
# 31996         | 0.3
# After the transformation, column 1 is re-written to 1991Q1, 1992Q2, 1996Q3 ...
# The function makes use of the assorted function 'gather_multivalue()' written 
# and made available by Wei Yang Tham on GitHub: 
# https://rdrr.io/github/weiyangtham/twydyverse/src/R/gather_multivalue.R
# make sure to install and load the package via 
# devtools::install_github("weiyangtham/twydyverse")
# library(twydyverse)
# The function returns a data frame in (STATA's) long format. 
transform_into_panel <- function(df){
  # Check validity of df
  if (sum((grepl("[[:punct:]]", names(df)) == FALSE)) != length(colnames(df))) {       
    stop("Please call function <<formatting>> first.")
  } 
  # starting variable 
  var_start <- colnames(df)[grep("\\d", colnames(df))[1]]
  # ending variable 
  var_end <- colnames(df)[length(df)]
  # Stata reshape command 
  df_panel <- gather_multivalue(df, "year",(var_start):(var_end))
  # if: quarterly data
  if ( (nchar(gsub("[^0-9]*", "", colnames(df)[length(df)]))) > 4) {
    # new variables 
    df_panel$fqtr <- as.integer(gsub("(^\\d{1}).*", "\\1", df_panel$year)) # extract quarters
    df_panel$fyearq <- as.integer(sub("(^\\d{1})", "", df_panel$year))
    df_panel$quarter <- as.integer(paste0(df_panel$fyearq,df_panel$fqtr))
    df_panel$datafqtr <- paste0(df_panel$fyearq,"Q",df_panel$fqtr)
    # order by 'gvkey' then by 'quarter'
    df_panel <- df_panel[with(df_panel, order( GVKEY, quarter)),]
    df_panel <- df_panel %>% select(datafqtr,  GVKEY , everything()) %>% relocate(fqtr, .after = fyearq)
    df_panel$year <- NULL
    return(df_panel)
  }
  
  return(df_panel)
}

# The renaming() sub-function takes a data frame as its only argument. 
# It takes care of renaming the variables of the input data frame.
# Besides it conducts two changes on the value level: It renames "Unites States" 
# to "USA" and drops the Exchange Ticker behind a company's name. 
# The function returns a modified data frame. 
renaming <- function(df){
  # renaming variables
  var_names <- c(gvkey = "GVKEY" ,
                 name = "CompanyName",
                       incorp = "StateofIncorporation", 
                       fic = "CountryRegionofIncorporation", 
                       sic = "SICCodesPrimaryCodeOnly", 
                       capex = "CapitalExpenditures", 
                       cshoq = "CommonSharesOutstanding", 
                       crq = "CurrentRatio", 
                       dpq = "DepreciationAmortization", 
                       xintq = "InterestExpense", 
                       dlttq = "LongTermDebt",
                       ppentq = "NetPropertyPlantEquipment", 
                       OperatingIncome = "OperatingIncome",
                       prstkcq = "PurchaseofCommonPrefStock",
                       xrd = "RDExpense",
                       saleq = "Revenue", 
                       atq = "TotalAssets", 
                       ltq = "TotalLiabilities",
                       seqq = "TotalParentEquity")
  df <- df %>% rename(any_of(var_names))
  # Makes sure to conduct modifications on the variable value level, only if 
  # merging process has not been completed yet. 
  if (!"datacqtr" %in% names(df)) {
    # renaming value "United States" to "USA"
    df$fic <- gsub('United States', 'USA', df$fic)
    # delete Exchange Ticker behind the company's name
    df$name <- gsub("\\([a-zA-Z]+:[a-zA-Z]+\\)$", "", df$name)   
    # delete white space at the end of the company's name
    df$name <- str_trim(df$name, side = c("right"))
  }
  return(df)
}

# The labeling() sub-function takes a data frame as its only argument. 
# It takes care of abeling the variables of the input data frame. 
labeling <- function(df){
  # labeling variables
  var_labels <- c( gvkey = "Identifier", 
                   name = "Company name",
                   viol = "1=Firm violated debt covenant; 0=otherwise",
                   viol_new = "1=New debt covenant violation; 0=otherwise", 
                   viol_once = "1=Firm is one-time violator; 0=otherwise", 
                   viol_multi = "1=Firm is frequent violator; 0=otherwise", 
                   ffi = "Fama-French industry classifications", 
                   ocf_atqaver = "Operating cash flow/Average assets", 
                   leverage = "Leverage ratio", 
                   interestex_atqaver = "Interest expenses/Average assets", 
                   networth_atq = "Networth/Total assets", 
                   capex_atqaver = "CAPEX/Average assets",
                   currentratio = "Current ratio", 
                   mb = "Market-to-book ratio",
                   atq = "Total assets", 
                   atq_log = "Ln(Total assets)", 
                   atq_diff = "First difference of Ln(Total assets)", 
                   ppentq = "Net property, plant and equipment (PPENT)" ,
                   ppentq_log = "Ln(PPENT)", 
                   ppentq_atq = "PPENT/Total assets",
                   ppentq_atq_diff = "First difference of PPENT/Total assets", 
                   xrd_atq =  "RD/Total assets (RD:= Research and Development Expenditures)", 
                   atq_change = "Change in Ln(Total assets)", 
                   ppentq_change = "Change in Ln(PPENT)", 
                   capex_atqaver_change = "Change in CAPEX/Average assets", 
                   xrd_atq_change = "Change in RD/Total assets", 
                   ppent_ratio = "PPENT (4leads)/PPENT", 
                   atq_ratio = "Total assets (4leads)/Total assets",
                   fic = "ISO country code of incorporation", 
                   incorp = "State/Province of Incorperation", 
                   cyearq = "Calendar year",
                   cqtr = "Calendar quarter", 
                   fyearq = "Fiscal year",
                   fqtr = "Fiscal quarter",
                   datafqtr = "Fiscal data year and quarter", 
                   datacqtr = "Calendar data year and quarter", 
                   quarter = "Numerical version of datafqtr",
                   calendar = "Numerical version of datacqtr",
                   datadate = "Report date of violation")
  Hmisc::label(df) <-  as.list(var_labels[match(names(df), names(var_labels))])
  return(df)
}

# The defining() sub-function aims to extend universal_filtered1 by all variables 
# of interest. 
# This is done by (1) shrinking universal data set (which still includes all available 
# data for each variable for each firm) to firms, that "survived" the filter1_acc_refpaper()- 
# process and, hence, are still in universal_filtered1. This step serves the purpose 
# of maximizing efficiency. 
# In order to prepare for the calculation of lagged variables, in step (2) the 
# function make_panelframe() adds missing year-quarter entries to the data frame
# so that each firm has exactly 14x4=56 observations.  
# Step (3) ensures that the vari ables are scaled correctly, step (4) calculates 
# all lagged variables, and step (5) defines all remaining variables of interest.
defining <- function(df_in, df_out){
  # select important variables for further analysis
  df <- df_in %>% mutate(datacqtr = replace(datacqtr, datacqtr == "", NA), 
                         calendar = as.numeric(str_replace_all(df_in$datacqtr, "Q", "")), 
                         cqtr = as.numeric(str_sub(calendar, start= -1)), 
                         cyearq = as.numeric(substring(calendar,1, nchar(calendar)-1)))

  firmsids <- df_out$gvkey 
  
  # (1) Reduce universal data set to firms of universal_filtered
  df <- df %>% filter(gvkey %in% unique(firmsids))
  # check: length(unique(df$gvkey)) == length(unique( firmsids ))
  
  # (2) Extend df to 52 observations for each gvkey 
  panel_frame <- make_panelframe(df,  firmsids , calendar = FALSE)
  df$gvkey <- as.numeric(df$gvkey)
  df$quarter <- as.numeric(df$quarter)
  df_panel <- left_join(panel_frame ,df, by =c("gvkey", "quarter")) 

  # (3) Set interval of variables correctly 
  df_panel  <- df_panel  %>% mutate(
      dlttq = replace(dlttq, dlttq <0, NA), # Long-Term Debt >=0 
      dlcq = replace(dlcq, dlcq <0, NA), # Debt in Current Liabilities >=0 
      ppentq = replace(ppentq, ppentq <0, NA), # PPENT >=0 # 12
      #lctq = replace(lctq, lctq< 0 , NA), # Total Current Liabilities >=0 
      ltq = replace(ltq, ltq< 0 , NA),
      xrd = replace(xrd, xrd < 0, NA), # Research and Development Expense >=0 
      txditcq = replace(txditcq,txditcq <0, NA), 
      capex= capex*(-1),
      dpq = replace(dpq, is.na(dpq) , 0),
      dpq = replace(dpq, dpq < 0 , NA),
      #ocf = (saleq- obexq)*(4),  # Operating Cash Flow = Total Revenue – Operating Expense, https://www.wallstreetmojo.com/operating-cash-flow-formula/#1-direct-method-ocf-formula
      ocf = (OperatingIncome+dpq),
      xintq = xintq*(-1),
      atq = replace(atq, atq <=0, NA)) # assets > 0
     
  # (4) Calculate lagged variables and add to df_panel
  # This step takes around 8 minutes!
  lagged_ys <- df_panel %>% 
    select(gvkey, quarter, atq, ppentq, capex, xrd) %>%
    arrange(gvkey, quarter) %>%
    group_by(gvkey) %>% 
    mutate(   # LAGGED VARIABLES (needed to obtain control variables and variables of interest)
              atq_lag = Hmisc::Lag(atq, +1), 
              atq_tplus4 = Hmisc::Lag(atq, -4), 
              atq_aver = (atq + atq_lag)/2,
              ppentq_tplus4 = Hmisc::Lag(ppentq, -4), 
              
              # CONTROL VARIABLES 1/2:
              # PPENT/Assets 
              ppentq_atq = ppentq/atq, 
              ppentq_atq = replace(ppentq_atq, ppentq_atq > 1, NA), 
              ppentq_atq = replace(ppentq_atq, ppentq_atq < -1, NA), 
              ppentq_atq = DescTools::Winsorize(ppentq_atq, probs = c(0.01, 0.99), na.rm = TRUE ), 
              ppentq_atq_lag = Hmisc::Lag(ppentq_atq, +1), 
              
              # VARIABLES OF INTEREST 1/2:
              # CAPEX / Average Assets
              capex_adj =  ifelse(abs(capex) <=atq & abs(capex) <=atq_lag , 4*capex, NA ),
              capex_atqaver = capex_adj/atq_aver,
              capex_atqaver = DescTools::Winsorize(capex_atqaver, probs = c(0.01, 0.99), na.rm = TRUE ), 
              capex_atqaver_tplus4 = Hmisc::Lag(capex_atqaver, -4), 
              
              xrd_atq = xrd/atq, 
              xrd_atq = replace(xrd_atq, xrd_atq > 1, NA), 
              xrd_atq = DescTools::Winsorize(xrd_atq, probs = c(0.01, 0.99), na.rm = TRUE ), 
              xrd_atq_tplus4 = Hmisc::Lag(xrd_atq, -4))
  
  df_panel <- left_join(df_panel, lagged_ys) %>% mutate( 
  # (5) Define other variables of interest
  # CONTROL VARIABLES 2/2:
    # ln(Assets)
    atq_log = log(atq), # firm size 
   
    # First Difference of ln(Assets)
    atq_diff = atq_log - log(atq_lag), 
    
    # First difference of PPENT/Assets
    ppentq_atq_diff = ppentq_atq - ppentq_atq_lag, 
    
    # Operating cash flow / Average assets 
    ocf_adj =  ifelse(abs(ocf) <=atq &abs(ocf) <=atq_lag , 4*ocf, NA ),
    ocf_atqaver = ocf_adj/atq_aver, 
    ocf_atqaver = DescTools::Winsorize(ocf_atqaver, probs = c(0.01, 0.99), na.rm = TRUE ), 
    
    # Leverage
    leverage = (dlttq+ dlcq)/atq, 
    leverage = replace(leverage, leverage >1, NA), 
    leverage = DescTools::Winsorize(leverage, probs = c(0.01, 0.99), na.rm = TRUE ), 
    
    # M-B-ratio
    mb_nowinz = (prccq * cshoq - (atq - ltq + txditcq) + atq)/atq, 
    mb =  DescTools::Winsorize(mb_nowinz, probs = c(0.01, 0.99), na.rm = TRUE ), 

    # Interest Expense / Average Assets
    xintq_adj =  ifelse(abs(xintq) <=atq &abs(xintq) <=atq_lag , 4*xintq, NA ),
    #xintq = case_when(xintq/atq %in% c(-1:1) & xintq/atq_lag %in% c(-1:1) ~ xintq),
    interestex_atqaver = xintq_adj/atq_aver,
    interestex_atqaver =  DescTools::Winsorize(interestex_atqaver, probs = c(0.01, 0.99), na.rm = TRUE ), 
    
    
    # Networth /Assets
    networth_atq = seqq/atq,   # same as (atq-ltq)/atq, 
    networth_atq = replace(networth_atq, networth_atq < -1, NA),
    networth_atq = replace(networth_atq, networth_atq > 1, NA),
    networth_atq =  DescTools::Winsorize(networth_atq, probs = c(0.01, 0.99), na.rm = TRUE ), 
    
    # Current Ratio 
    currentratio_nowinz = crq, 
    currentratio =  DescTools::Winsorize(currentratio_nowinz, probs = c(0.01, 0.99), na.rm = TRUE ), 

  # VARIABLES OF INTEREST 2/2:
    # change in ln(Assets)
    atq_tplus4_log = log(atq_tplus4), 
    atq_change = log(atq_tplus4) - atq_log,
    atq_change = replace(atq_change, atq_change == "Inf", NA),
    atq_ratio = ifelse(!is.na(atq_change) , atq_tplus4/atq , NA),
  
  
    # Change in ln(PPENT)
    ppentq_log = log(ppentq), 
    ppentq_log = replace(ppentq_log, ppentq_log == "Inf", NA),
    ppentq_log = replace(ppentq_log, ppentq_log == "-Inf", NA),
    ppentq_change = log(ppentq_tplus4) - ppentq_log , 
    ppentq_change = replace(ppentq_change, ppentq_change == "Inf", NA),
    ppent_ratio = ifelse(!is.na(ppentq_change) , ppentq_tplus4/ppentq , NA),
    
    # Change in CAPEX / Average Assets
    capex_atqaver_change =  (capex_atqaver_tplus4) - capex_atqaver, 
    
  
    # Change in RD / Assets 
    xrd_atq_change = xrd_atq_tplus4 - xrd_atq, 
    xrd_atq_change = replace(xrd_atq_change, xrd_atq_change == "Inf", NA),
    xrd_atq_change = replace(xrd_atq_change, xrd_atq_change == "-Inf", NA))

  df_out$quarter <- as.numeric(df_out$quarter)
  df_final <- df_out %>% 
    select(gvkey, quarter) %>% 
    left_join(df_panel, by = c("gvkey", "quarter"))
  return(df_final)
} 

# The create_ffi() sub-function aims to categorize 38 Fama and French industries (ffi)
# based on the firm's SIC code. The industries are defined according to 
# https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/det_38_ind_port.html. 
# It takes a data frame as input and extends it by the variable `ffi`.
create_ffi <- function(df) {
  df <- df %>% mutate(
    ffi = case_when(
      sic %in% c(0100:0999) ~ 1, # Agriculture
      sic %in% c(1000:1299) ~ 2, # Mines 
      sic %in% c(1300:1399) ~ 3, # Oil 
      sic %in% c(1400:1499) ~ 4, #Stone 
      sic %in% c(1500:1799) ~ 5,  #Construction
      sic %in% c(2000:2099) ~ 6, # Food   
      sic %in% c(2100:2199) ~ 7, # Smoke
      sic %in% c(2200:2299) ~ 8, # Textile
      sic %in% c(2300:2399) ~ 9, # Apparel
      sic %in% c(2400:2499) ~ 10, # Wood
      sic %in% c(2500:2599) ~ 11, # Chair 
      sic %in% c(2600:2661) ~ 12, # Paper 
      sic %in% c(2700:2799) ~ 13, # Print 
      sic %in% c(2800:2899) ~ 14, # Chems 
      sic %in% c(2900:2999) ~ 15, # Petroleum
      sic %in% c(3000:3099) ~ 16, #Rubber
      sic %in% c(3100:3199) ~ 17, # Leather
      sic %in% c(3200:3299) ~ 18, # Glass
      sic %in% c(3300:3399) ~ 19,  # Metal 
      sic %in% c(3400:3499) ~ 20, # Fabricated Metal Products
      sic %in% c(3500:3599) ~ 21, # Machinery
      sic %in% c(3600:3699) ~ 22, # Electronic 
      sic %in% c(3700:3799) ~ 23, # Cars 
      sic %in% c(3800:3879) ~ 24, # Instruments
      sic %in% c(3900:3999) ~ 25, # Miscellaneous Manufacturing Industries
      sic %in% c(4000:4799) ~ 26, # Transportation
      sic %in% c(4800:4829) ~ 27, # Phone 
      sic %in% c(4830:4899) ~ 28, # TV 
      sic %in% c(4900:4949) ~ 29, # Utils:Electric, Gas, and Water Supply
      sic %in% c(4950:4959) ~ 30, # Sanitary Services
      sic %in% c(4960:4969) ~ 31, # Steam
      sic %in% c(4970:4979) ~ 32, # Water
      sic %in% c(5000:5199) ~ 33, # Wholesale
      sic %in% c(5200:5999) ~ 34, # Retail Stores
      sic %in% c(6000:6999) ~ 35, # Finance, Insurance, and Real Estate
      sic %in% c(7000:8999) ~ 36, # Services
      sic %in% c(9000:9999) ~ 37),  # Public Administration
    ffi = replace( ffi, is.na(ffi), 38)) # Other
}

# The make_panelframe() sub-function adds missing time variables to the input data frame 
# in order ensure consecutive time steps for the computation of lagged and lead variables. 
make_panelframe <- function(df, firmid, calendar = TRUE){

 
  firms_all <- unique(firmid)
  
  if (calendar == TRUE) {
    calendars <- c(19962:19964, 19971:19974 , 19981:19984,  19991:19994,  20001:20004, 
                  20011:20014, 20021:20024, 20031:20034, 20041:20044, 20051:20054, 
                  20061:20064, 20071:20074, 20081:20084, 20091, 20092)
    # calendar variable 
    panel_frame <- data.frame(calendar= rep(calendars , length(unique(firmid))), 
                              gvkey = rep(firms_all, each = length(calendars))) 
  } else {
    # quarter variable 
    fquarters <- c(19961:19964, 19971:19974 , 19981:19984,  19991:19994,  20001:20004, 
                   20011:20014, 20021:20024, 20031:20034, 20041:20044, 20051:20054, 
                   20061:20064, 20071:20074, 20081:20084)
    panel_frame <- data.frame(quarter = rep(fquarters , length(unique(firmid))), 
                              gvkey = rep(firms_all, each = length(fquarters))) 
  }
 
  #nrow(panel_frame)/length(quarters) == length(firms_all) # check 
  return(panel_frame)
}

# The adjust_viol() sub-function takes the data frame viol as its only argument. 
# It derives the new columns "datacqtr", i.e. calendar Data Year and quarter, 
# and "calendar", i.e. numeric version of datacqtr, from the existent variable 
# "datadate", e.g. 06301996 becomes 1996Q2 and 19962 respectively. 
# This modification must be made in order to complete a trailing merge. 
# The function outputs 'viol', extended by two columns, otherwise unchanged. 
adjust_viol <- function(viol) {
  # Check validity of df
  if (sum(colnames(viol) %in% c("datadate", "gvkey", "viol", "datacqtr") ) > 4 |
      sum(colnames(viol) %in% c("datadate", "gvkey", "viol", "datacqtr") ) < 3) {       
    stop("Please use this function only on the data set 'viol'.")
  } 
  viol$year <-  as.numeric(stringr::str_sub(viol$datadate, start= -4))
  viol$cq <-  (stringr::str_sub(viol$datadate, start= 1 , end = 2))
  viol$cq<- gsub("01|02|03","1", viol$cq)
  viol$cq<- gsub("04|05|06","2", viol$cq)
  viol$cq<- gsub("07|08|09","3", viol$cq)
  viol$cq<- gsub("10|11|12","4", viol$cq)
  viol$datacqtr <- paste0(viol$year,"Q",viol$cq)
  viol$calendar <- as.integer(paste0(viol$year,viol$cq))
  viol$cq <- NULL
  viol$year <- NULL
  # make sure only ONE viol-entry PER quarter and year , e.g. 
  # How many observations would we loose if we neglect column "datadate" and check for duplicates? 
  # test <- viol %>% select(!datadate) %>% distinct()  # One obs., which is: 
  # viol[viol$gvkey == 24710 & viol$datacqtr == "2003Q1", 1:4 ]
  # datadate | gvkey  | viol | datacqtr 
  # 01312003 | 24710  |   0  | 2003Q1
  # 03312003 | 24710  |   0  | 2003Q1
  # -> one of these observation must be dropped! 
  # We choose to drop the latter
  viol <- viol %>% filter (! (gvkey == 24710 &  datadate == "03312003"))
  # order by 'gvkey' then by 'quarter'
  viol <- viol[with(viol, order(gvkey)),]
  viol <- viol %>% select(datacqtr, gvkey, viol, everything()) 
  var_label(viol$viol) <- "1=Firm violated Debt Covenant; 0=otherwise"
  var_label(viol$calendar) <- "Numerical Version of datacqtr"
  var_label(viol$datadate) <- "Report Date of Violation"
  return(viol)
}

# The filter1_acc_refpaper() sub-function takes care of the first 4 filter processes
# as described in Nini,G., Smith,D.C., Sufi,A. (2012): Creditor Control Rights, Corporate 
# Governance, and Firm Value, in: The Review of Financial Studies, 25(6), p. 1724. 
# It is applied onto`universal` data set.
filter1_acc_refpaper <- function(df){
  df$gvkey <- as.integer(df$gvkey)
  # (1) non financial US firms 
  # -> already filtered down in data scraping process on S&P Capital IQ, whereas 
  # the sub-function "filtering()" ensured compliance with the filter criteria, too.
  
  # (2) Limit data set to calendar time period: 1996Q2 - 2009Q2 
  df <- df %>% mutate(datacqtr = replace(datacqtr, datacqtr == "", NA), 
                      calendar = as.numeric(str_replace_all(df$datacqtr, "Q", "")))%>% 
    set_variable_labels(calendar  = "Numeric Version of datacqtr") %>%
    filter(calendar <= 20092 & calendar >= 19962) 

  # (3) Make sure the average of atq in year 2000 is > 10
  # This may take some minutes! 
  df <- make_panelframe(df, df$gvkey, calendar = TRUE) %>% left_join(df, by = c("calendar", "gvkey")) %>% 
    select(gvkey, quarter,atq) %>% arrange(gvkey, quarter) %>%
    group_by(gvkey) %>% mutate(
      atq = replace(atq, atq <=0, NA), # Set total assets >0 
      atq_lag = Hmisc::Lag(atq, +1),  # Define lagged total assets 
      atq_aver = (atq + atq_lag)/2) %>%   # Define variable for average atq: atq_aver
      select(!atq) %>%
      right_join(df, by = c("gvkey", "quarter")) # Remerge onto data frame 
  var_label(df$atq_aver) <- "Average Assets"
  out <- (unique(df$gvkey[df$atq_aver <= 10 & df$datacqtr %in% c("2000Q1","2000Q2","2000Q3","2000Q4")]))
  df <- df %>% filter (! gvkey %in% out )
  
  # (4) Drop all quarterly observations with missing values for variables: atq, saleq, cshoq, prccq, datacqtr
  df <- df %>% filter_at(vars(gvkey, atq, saleq, cshoq, prccq, datacqtr), all_vars(!is.na(.)))
  # Remark: datacqtr is needed for the merge with viol_added
}

# The filter2_acc_refpaper() sub-function aims to ensure that all observations in the 
# sample have at least four consecutive previous quarters. This step is done to determine 
# the new debt covenant violation variable. 
# It is applied onto `viol_added` data set.
filter2_acc_refpaper <- function(df){
  # for filters (1) - (4) -> see sub-function "filter1_acc_refpaper()"
  # (5) For determination of a "new-violation", hence a violation where Include observations only if the four consecutive previous observations are available
  # Change classes
  df$gvkey <- as.numeric(df$gvkey)
  df$calendar<- as.numeric(df$calendar)
  
  # Drop all firms that have less than 5 observations
  df <- df %>% drop_na(viol) %>% group_by(gvkey) %>% summarise(nrow= n())  %>% filter(nrow >=5) %>% 
    left_join(df, by = "gvkey") %>% arrange(gvkey, calendar) %>% select(!nrow)
  
  df_in <- df # save results 

  # Create panel_frame that can df be merged on later 
  viol_panel <-  make_panelframe(df, df$gvkey, calendar = TRUE) %>% left_join(df, by =c("gvkey", "calendar")) %>% 
    arrange(gvkey, calendar) 
  # viol_panel %>% group_by(gvkey) %>% count() 
  
  # Extract all gvkeys of firms that have violated debt covenants and save in vector "firms_viol"
    firms_viol <- df %>% group_by(gvkey) %>% 
      mutate(nr_viol = max(cumsum(viol),  na.rm = TRUE)) %>% 
      select(gvkey, nr_viol )%>% distinct() %>% filter(nr_viol > 0) %>% select(gvkey)
    firms_viol <- as.vector(firms_viol$gvkey)
  # Extract all gvkeys of firms that have NOT violated debt covenants and save in vector "firms_viol"
    firms_noviol <- df %>% group_by(gvkey) %>% 
      mutate(nr_viol = max(cumsum(viol),  na.rm = TRUE)) %>% 
      select(gvkey, nr_viol )%>% distinct() %>% filter(nr_viol == 0) %>% select(gvkey)
    firms_noviol <- as.vector(firms_noviol$gvkey)
    
  # Separate df into ...
    # all observations with firms that have at least once violated 
    df_viol <- viol_panel %>% filter(gvkey %in% firms_viol) 
    # all observations with firms that have not violated
    df_noviol <- viol_panel %>% filter(gvkey %in% firms_noviol) 
  

  # for df_viol:= all observations with firms that have at least once violated a debt covenant 
    hf_viol <- data.frame(matrix(ncol = 4, nrow = 0)) # create data frame which is to be filled in the next loop 
    for(i in 1:length(firms_viol)){
      firmnr <- firms_viol[i]
      crop <- df_viol %>% select(viol, gvkey, calendar) %>% filter(gvkey == firmnr)
      crop$viol_ident <- NA # new variable 
      crop$viol_new <- 0 # new variable 
      
      crop$viol[is.na(crop$viol)] <- 9 # recode NA in "viol" variable to value 9 
      pattern <- paste0(crop$viol, collapse="") # grasp the viol pattern for each firm, e.g. for firm with gvkey == "1021" : "9999000000000111100100110110101000000000099010009"
      #print(paste(pattern, crop$gvkey))
      
      # Include observations only if the four consecutive previous observations are available: 
      # Check whether a 1 is preceded by a binary sequence of 4. 
      xxxxo <- str_locate_all(pattern, "(?<=[0,1]{4})1") # grasp pattern: xxxx1, whereby x:= either 0 or 1 (no missing value, which equals 9)
      xxxxo_rownr <- do.call(rbind, xxxxo)[,1] # save row number of the respective 1
      
      # Check whether a 0 is preceded by a binary sequence of 4. 
      xxxxz <- str_locate_all(pattern, "(?<=[0,1]{4})0") # grasp pattern: xxxx0, whereby x:= either 0 or 1 (no missing value, which equals 9)
      xxxxz_rownr <- do.call(rbind, xxxxz)[,1] # save row number of the respective 0
      
      # Define new debt covenant pattern
      zzzzo <- str_locate_all(pattern, "(?<=0{4})1") # grasp pattern: 00001
      zzzzo_rownr <- do.call(rbind,   zzzzo)[,1] # save row number of first 1 (= new debt covenant violation)
      
      # for each saved row number of pattern xxxx1 assign a 1 to for new variable "viol_ident"
      for (i in 1:length(xxxxo_rownr)) { 
        row <- xxxxo_rownr[i]
        crop$viol_ident[row] <- 1
      }
      
      # for each saved row number of pattern xxxx0 assign a 0 to for new variable "viol_ident"
      for (i in 1:length(xxxxz_rownr)) { 
        row <- xxxxz_rownr[i] # 
        crop$viol_ident[row] <- 0
      }
      
      # for each saved row number of pattern 00001 assign a 1 to for new variable "viol_new"
      for (i in 1:length(zzzzo_rownr)) { 
        row <- zzzzo_rownr[i] # 
        crop$viol_new[row] <- 1
      }
      
      hf_viol  <- rbind(hf_viol, crop)
    }
    hf_viol$viol[hf_viol$viol == 9] <- NA
   
    hf_viol <- hf_viol %>% drop_na(viol_ident)

  # for df_noviol:= all observations with firms that have never violated a debt covenant 
  hf_noviol <- data.frame(matrix(ncol = 4, nrow = 0)) # create data frame which is to be filled in the next loop 
  for(i in 1:length(firms_noviol)){
    firmnr <- firms_noviol[i]
    crop <- df_noviol %>% select(viol, gvkey, calendar) %>% filter(gvkey == firmnr) # 
    crop$viol_ident <- NA 
    
    crop$viol[is.na(crop$viol)] <- 9 # recode NA in "viol" variable to value 9 
    pattern <- paste0(crop$viol, collapse="") # grasp the viol pattern for each firm, e.g. for firm with gvkey == "1021" : "9999000000000111100100110110101000000000099010009"
    # print(paste(pattern, crop$gvkey))
    
    # Check whether a 0 is preceded by a binary sequence of 4. 
    xxxxz <- str_locate_all(pattern, "(?<=[0,1]{4})0") # grasp pattern: xxxx0, whereby x:= either 0 or 1 (no missing value, which equals 9)
    xxxxz_rownr <- do.call(rbind, xxxxz)[,1] # save row number of the respective 0
    
    # for each saved row number of pattern xxxx0 assign a 0 to for new variable "viol_ident"
    for (i in 1:length(xxxxz_rownr)) { 
      row <- xxxxz_rownr[i] # 
      crop$viol_ident[row] <- 0
    }
    hf_noviol  <- rbind(hf_noviol , crop)
  }
  hf_noviol$viol[hf_noviol$viol == 9] <- NA
  hf_noviol <- hf_noviol %>% drop_na(viol_ident)
  hf_noviol$viol_new <- 0
  
  df_out <- rbind(hf_noviol, hf_viol)

  # Add frequent and one-time violator variable BASED ON variable viol (NOT viol_ident!!)
  df_out <- df_in %>%  filter(calendar >= 19972) %>% 
    group_by(gvkey) %>%
    mutate(nr_viol = max(cumsum(viol)),
           # one-time violator dummy
           viol_once = case_when(nr_viol > 1 ~ 0,
                                 nr_viol == 1 ~ 1, 
                                 nr_viol == 0 ~ 0), 
           # frequent violator dummy
           viol_multi = case_when(nr_viol> 1 ~ 1, 
                                  nr_viol <=1 ~0)) %>%
    set_variable_labels(viol_once = "1=Firm is one-time violator; 0=otherwise") %>%
    set_variable_labels(viol_multi = "1=Firm is frequent violator; 0=otherwise") %>% 
    select(viol_once, viol_multi, gvkey, calendar) %>%
    right_join(df_out, by = c("gvkey", "calendar"))

  df_out <- left_join(df_out, df_in) # add  "datacqtr" and "datadate" back onto df
}



# 4 prepare data sets -----------------------------------------------------------
## 4.1 capitaliq ---------------------------------------------------------------
# Read in all Excel files from folder_capitaliq, apply the sub-functions 
# "formatting()" and "filtering()" on each file and return a list of 
# 3x13 = 39 elements (three Excel data sheets for each year; 13 years in total 
# (1996-2008)). This may take up to one minute! 
list1_capitaliq <- lapply(folder_capitaliq, function(z){
  raw_input <- read_excel(
    path = file.path(here(), path, "by S&P Capital IQ", z), 
    skip = 7, na = c("-", "NM"), col_types = "text")
  formatted <- formatting(raw_input)
  filtered <- filtering(formatted)
  return(filtered)
})
# Merge each element-year-trio from list1_capitaliq to one data frame according 
# to their year and return a list of 1x13 elements (for each year (1996-2008)). 
nyears = 13
firstyear = 1996
firstidx = 1
endidx = 3
list2_capitaliq <- list()
datnames <- list()
for(i in 1:nyears){
  # cat(paste0('\nfirstidx:', firstidx, '\nlastidx:', endidx)) # checking year index
  datname <-  paste0('data', firstyear)
  datnames[[i]] <- datname
  tmp     <- list1_capitaliq[firstidx:endidx]
  bound   <- data.frame(do.call('rbind.data.frame', tmp))
  assign(datname, bound)
  firstyear <- firstyear + 1
  firstidx  <- endidx + 1
  endidx    <- firstidx + 2
  rm(bound)
  if ( i == nyears){
    list2_capitaliq <- mget(ls(pattern = "data\\d"))
    rm(list = ls(pattern = "data\\d"), datnames, datname, endidx, firstidx, firstyear, 
       i, nyears, tmp) 
    return(list2_capitaliq)
}
}
# Merge all 13 data frames from list2_capitaliq to one data frame.
# This step may take up to 1min!
capitaliq <- list2_capitaliq %>% reduce(right_join, 
                      by = c("CompanyName", "StateofIncorporation",
                             "CountryRegionofIncorporation","GVKEY", 
                             "SICCodesPrimaryCodeOnly", "CompanyType"))
# Transform 'capitaliq' into panel data.
# Be patient, this step may take up to 2min!
capitaliq_panel <- capitaliq  %>% transform_into_panel() 
# Delete variables not relevant for further analysis: 
capitaliq_panel <- capitaliq_panel %>% 
  select(!c( CompanyType, Acquisitions,CashDividends, EBIT,
            CashfromOperatingActivities, CashShortTermInvestments, DeferredTaxes,
            GrossProfit, InvestmentTaxCredit, LongTermDebtIssuance, NetIncome, 
            OtherAssetsTotal, OtherLiabilities, TotalAssetTurnover, TotalCurrentAssets, 
            TotalCurrentDebt, TotalCurrentLiabilities, TotalDebtEquity, 
            TotalOperatingExpenses))
## !fact check! ----------------
# What is the sample period of "capitaliq_panel" ? 
min(capitaliq_panel$quarter) # 1996Q1 (fiscal)
max(capitaliq_panel$quarter) # 2008Q4 (fiscal)
# - End of fact check! -



## 4.2 viol --------------------------------------------------------------------
# Apply 'adjust_viol()' sub-function on 'viol'.
viol_added <- viol %>% adjust_viol 
# Remark: viol_added as one observation less than viol, check:  
# setdiff(viol, viol_added[,2:4])
# Reason: Firm with gvkey == 24710 has two seperate viol entries in calendar quarter 2003Q1, check 
# viol[viol$gvkey == 24710 & str_detect(viol$datadate, "2003"), ]
# namely the dates: 31/01/2003 (01312003) and 31/03/2003 (03312003. 
# As both viol entries take the value 0, no information value is lost by 
# dropping one of these variables.
## !fact check! ----------------
# What is the sample period of "viol_added" ? 
min(viol_added$datacqtr) # 1996Q2 (calendar)
max(viol_added$datacqtr) # 2008Q2 (calendar)
# - End of fact check! -



# 5 merge capitaliq_panel and suppl to universal data set ----------------------
# merge suppl and capitaliq_panel to universal
universal <- left_join(capitaliq_panel, suppl[,-c(2,3)], 
                       by = c("GVKEY" = "gvkey", "datafqtr")) 
# Remark: suppl[,-c(2,3)] excludes redundant columns "fyearq" and "fqtr"  
# 'universal' covers all observations for non-financial US firms. 
# rename variables of universal data set
universal <- universal %>% renaming()




# 6 filter according to reference paper  --------------------
## 6.1 filter universal --------------------------------------------------------
# universal_filtered presents the data set after applying the first filters via 
# the sub-function filter1_acc_refpaper(), as described on pages 1724 in the reference paper....
universal_filtered1 <- universal %>% filter1_acc_refpaper() 
# ... and adding all variables of interest via sub-function defining() ...
universal_filtered2 <- defining(df_out = universal_filtered1, df_in = universal) %>%
  # ... add 38 Fama-French industry classification dummy based on SIC codes.
  create_ffi() 
## !fact check! ----------------
# Calendar & fiscal time period:
max(universal_filtered2$datacqtr) # 2009Q2 (calendar)
min(universal_filtered2$datacqtr) # 1996Q2 (calendar)
max(universal_filtered2$datafqtr) # 2008Q4 (fiscal)
min(universal_filtered2$datafqtr) # 1996Q1 (fiscal)
# Any missing values in filter variables?
x <- universal_filtered2 %>% select(gvkey, atq, saleq, cshoq, prccq, datacqtr, calendar, quarter) 
summary(x) # No missing values in all 8 variables!
rm(x)
# What is the minimum value for atq_aver in the calendar year 2000? (Must be > 10Mio)
min(universal_filtered2$atq_aver[universal_filtered2$datacqtr %in% c("2000Q1","2000Q2","2000Q3","2000Q4")], na.rm = TRUE) # 10.025
# How many firms? 
length(unique(universal_filtered2$gvkey)) # 8102
# How many quarterly observations?
nrow(universal_filtered2) # 222838
# - End of fact check! -
# Select variables that are relevant for further analysis.
universal_filtered2 <- universal_filtered2 %>% 
  select(gvkey, quarter, ffi, ocf_atqaver, leverage, interestex_atqaver, 
       networth_atq, currentratio, currentratio_nowinz, mb, mb_nowinz, atq, 
       atq, atq_log, atq_diff, ppentq, ppentq_log, ppentq_atq, ppentq_atq_diff, 
       capex_atqaver, xrd_atq, atq_change, ppentq_change, capex_atqaver_change, 
       xrd_atq_change, atq_ratio, ppent_ratio, fic, incorp, calendar, cqtr, cyearq,
       datacqtr, fyearq, fqtr, datafqtr, name)



## 6.2 filter viol -------------------------------------------------------------
# This step may take up to 6min!
viol_filtered <- viol_added %>% filter2_acc_refpaper()
## !fact check! ----------------
# Time period of viol_filtered: 
min(viol_filtered$calendar) # 19972 (calendar)
max(viol_filtered$calendar) # 20082 (calendar)
# How many firms are listed in the data set viol/viol_added? 
length(unique(viol$gvkey)) # 10537 firms
# How many rows from the Compustat data file 'suppl' can be merged onto viol/viol_added?
compviol <- suppl %>% inner_join(viol_added)
length(unique(compviol$gvkey)) # 10,524 firms 'survived' the merge which equals
10524/10537*100 # 99.87663 percent. 
rm(compviol)
# How many rows from the S&P Capital IQ datafile 'capitaliq_panel' can be merged onto viol/viol_added?
spviol <- left_join(capitaliq_panel, suppl[,-c(2,3)], by = c("GVKEY" = "gvkey", "datafqtr")) %>%
  rename(gvkey = "GVKEY") %>% inner_join(viol_added)
length(unique(spviol$gvkey)) # 9,084 firms 'survived' the merge which equals
9084/10537*100 # 86.2105 percent. 
rm(spviol)



# 7 merge viol to universal  ------------------------------------------------------------
keys <- intersect(names(universal_filtered2), names(viol_filtered))
universal_filtered2$datacqtr <- as.character(universal_filtered2$datacqtr)
FINAL <- right_join(viol_filtered , universal_filtered2, by = keys) %>% 
  drop_na(viol_ident) %>% relocate(viol_once,viol_multi, .after = viol_new) %>% 
  relocate(datacqtr, quarter, datadate, .after = last_col()) %>%
  select(!viol_ident)
# Add obs. from universal_filtered2 for 2008Q3-2009Q2 for all firms that "survived" the merge 
f <- FINAL %>% select(gvkey) %>% distinct
f <- as.vector(f$gvkey)
e <- universal_filtered2 %>% filter(gvkey %in% f & calendar %in% c(20083,20084,20091,20092))
FINAL <- rbind(FINAL, e)
rm(f,e)
## !fact check! ----------------
# Calendar time period: 
max(FINAL$calendar) # 20092 (calendar)
min(FINAL$calendar) # 19972 (calendar)
# Fiscal time period: 
max(FINAL$quarter) # 20084 (fiscal)
min(FINAL$quarter) # 19971 (fiscal)
# No. of obs.:
nrow(FINAL) # 168340
# No. of firms: 
length(unique(FINAL$gvkey)) # 7089
# What is the maximum value of market-to-book without winzorising? 
max(FINAL$mb_nowinz, na.rm = TRUE) # 47289.5
# What is the maximum value of currentratio without winzorising? 
max(FINAL$currentratio_nowinz, na.rm = TRUE) # 291.8
# For which calendar time period does Change in Ln(Assets) have no missing values?
max(FINAL$calendar[FINAL$atq_change>0],na.rm = TRUE) # 20081
min(FINAL$calendar[FINAL$atq_change>0],na.rm = TRUE) # 19972
# How many rows for the calendar time period 2008Q2 exist?
x <- FINAL[(FINAL$calendar == 20082),] 
nrow(x) # 23
# -> only 23 observations for 2008Q2 with missing values for all dependent variables: 
# atq_change, ppentq_change, capex_atqaver_change, xrd_atq_change
# Change in Ln(Assest), Change in Ln(PPENT), Change in CAPEX/Average Assets, Change in RD/Assets, respectively. 
# -> for regression later: 1997Q2 - 2008Q1, thus 10x4+3+1 = 44 calendar year-quarter dummies 
# - End of fact check! -
# Delete no-winsorized market-to-book ratio and current ratio
FINAL <- FINAL %>% select(!c(mb_nowinz, currentratio_nowinz))



# 8 label FINAL data set --------------------------------------------------------
FINAL <- FINAL %>% labeling()



# 9 save & export data set ------------------------------------------------------
# to .csv file (no labels) 
write.csv(FINAL, file = file.path(path,"my_dataset.csv"), 
          row.names = FALSE)  
# to .dta file (with labels)
write_dta(FINAL, path = file.path(path,"my_dataset.dta"), 
          version = 14, label = attr("data", "label"))





