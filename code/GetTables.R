# credits  ---------------------------------------------------------------------
'
Author:     I. Fetzer
Purpose:    This R script produces raw tables code suitable for 
            inclusion in an LATEX environment for:
            - Summary statistics 
            - Summary statistics of reference paper
            - Regression results (2 tables)
            - Comparison of regression results with reference paper
            - Averages for one-time violator and frequent violator groups
            All tables are rendered from .tex to .pdf files (both made available) 
            and serve as automated draft versions of the manually style-adjusted 
            final versions in the submitted paper. 
Note:       The file "my_dataset.csv" -obtained by the R skript "GetDataset.R"- 
            is required to produce all tables.
            Please make sure to re-adjust the working directory path in line 23
            before running the code. 
' 
# 1 initialization -------------------------------------------------------------
rm(list=ls(all=TRUE))         # clear environment
graphics.off()                # clear console
# set working directory 
setwd("~/Library/Mobile\ Documents/com~apple~CloudDocs/Uni/BA - icloud")
# install & load packages
libraries = c("dplyr","tidyverse","lmtest", "fixest", "texreg", "sandwich", 
              "lmtest", "xtable")
lapply(libraries, function(x) if (!(x %in% installed.packages())) 
{ install.packages(x) })
lapply(libraries, library, quietly = TRUE, character.only = TRUE)



# 2 load in datatset and define as panel data ----------------------------------
data <- read.csv("data/my_dataset.csv")
pdata <- plm::pdata.frame(data, index = c("gvkey", "quarter")) 



# 3 create tables --------------------------------------------------------------
## 3.1 my summary statistics -------------------------------------------------------
data_stats <- data %>% select(gvkey, ocf_atqaver, leverage, interestex_atqaver, 
                              networth_atq, currentratio, mb, atq_change, ppentq_change, 
                              capex_atqaver, xrd_atq, atq, ppentq,
                              atq_ratio, ppent_ratio, ppentq_atq, viol, viol_new,
                              viol_once, viol_multi)
# Add number of firms per variable 
vars <- names(data_stats)[-1]
n_firms <- c()
for(i in 1:length(vars)){
  currentvar <- vars[i]
  n_firms[i] <- length(unique(data$gvkey[!is.na(data[, currentvar])]))
}
tab1_names <- c( "Operating cash flow/ Average assets",  
                 "Leverage", "Interest expense/ Average assets", 
                 "Net worth/ Assets", "Current ratio","Market-to-book", 
                 "Change in Ln(Assets)", "Change in Ln(PPENT)",    
                 "CAPEX/ Average assets", "R&D/ Assets", "Assets ($ millions)", 
                 "PPENT ($ millions)", "Assets (4 leads)/ Assets", 
                 "PPENT (4 leads)/ PPENT", "PPENT/ Assets",
                 "Financial covenant violation","New financial covenant violation", 
                 "One-time violator", "Frequent violator")
names(data_stats)[-1] <- tab1_names
# An output is created that shows a complete descriptive statistic with: 
# n_obs, mean, sd, min, p10, median, max, n_firms
table_summarystats <- data_stats %>% select(!gvkey) %>% 
  gather(variable, value) %>% 
  group_by(variable) %>% summarise(`No. of obs.` = sum(!is.na(value)),
                                   `Mean` = mean(value, na.rm = TRUE),
                                   `SD` = sd(value, na.rm = TRUE),
                                   `Min` = min(value, na.rm = TRUE), 
                                   `p10` = quantile(value, probs= c(0.1),na.rm = TRUE), 
                                   `Median` = median(value, na.rm = TRUE),
                                   `p90` = quantile(value, probs= c(0.9),na.rm = TRUE),
                                   `Max` = max(value, na.rm = TRUE)) %>%
  slice(match(tab1_names, variable))
table_summarystats$`No. of firms` <- n_firms
table_summarystats <- print(xtable(table_summarystats, digits = 3, type = "latex", method = "compact",
                                   caption = '\\textbf{Summary Statistics, DRAFT.} \\\\
                     Automated output. The table version in the submitted paper 
                     was manually adjusted in style due to space constraints. The resizebox{} argument 
             was manually added to the .tex code.'), 
                            include.rownames=FALSE, caption.placement = 'top', return.string = TRUE)
## save table ------------------------------------------------------------------
write_file(table_summarystats, 'output/tables/my_Table1.tex')
table_summarystats


## 3.2 summary statistics of reference paper (Table 5, p.1735) -----------------
ref_nobs <- c(179350, 176291,  172190,  181698, 177749, 181698, 165968, 164990, 177014)
ref_mean <- c(0.057, 0.242,  0.021, 0.475, 2.787,  2.027,  0.057,  0.038, 0.059)
ref_sd <- c(0.237, 0.242, 0.028, 0.306, 2.783, 1.775, 0.380, 0.484, 0.078)
ref_p10 <- c(-0.200, 0.000, 0.000, 0.145, 0.818, 0.850, -0.258, -0.299,0.005)
ref_median <- c(0.104 , 0.197 , 0.013, 0.498, 1.950, 1.442, 0.041, 0.023, 0.034)
ref_p90 <- c(0.260, 0.549, 0.050, 0.830, 5.482, 3.764, 0.381, 0.418, 0.138)
table_ref_summarystats <- data.frame(matrix(ncol = 0, nrow = 9))   
rownames(table_ref_summarystats) <- tab1_names[1:9]
table_ref_summarystats$`No. of obs.` <- ref_nobs
table_ref_summarystats$Mean <- ref_mean
table_ref_summarystats$SD <- ref_sd
table_ref_summarystats$p10 <- ref_p10
table_ref_summarystats$Median <- ref_median
table_ref_summarystats$p90 <- ref_p90
table_ref_summarystats <- print(xtable(table_ref_summarystats, digits = 3, 
                                       type = "latex", method = "compact",
                                       caption = '\\textbf{Summary Statistics of reference paper, DRAFT.} \\\\
                     Automated output. The table version in the submitted paper 
                     was manually adjusted in style due to space constraints.'), 
                                include.rownames=TRUE, caption.placement = 'top')
## save table ------------------------------------------------------------------
write_file(table_ref_summarystats, 'output/tables/my_Table2.tex')



## 3.3 regression table --------------------------------------------------------
### 3.3.1 Panel A --------------------------------------------------------------
# model: Change in ln(Assets)
m1_atq <- lm(atq_change ~ viol_new, data = pdata) 
m2_atq <- lm(atq_change ~ viol_new + ocf_atqaver+ leverage + interestex_atqaver+ 
               networth_atq + currentratio  + mb  + atq_log + ppentq_atq + atq_diff + 
               ppentq_atq_diff, data = pdata)
m3_atq <- feols(atq_change ~ viol_new +
                  ocf_atqaver + leverage + interestex_atqaver+ networth_atq + 
                  currentratio  + mb + atq_log + ppentq_atq + atq_diff + ppentq_atq_diff | 
                  calendar + ffi + fqtr ,  data = pdata) 
# control with ols and inclusion of dummies as FE
control_atq <- feols(atq_change ~ viol_new  + ocf_atqaver + leverage + 
                       interestex_atqaver+ networth_atq + currentratio  + mb +
                       atq_log + ppentq_atq + atq_diff + ppentq_atq_diff  + 
                       as.factor(fqtr) + as.factor(ffi) + as.factor(calendar), data = pdata)
screenreg(list(m3_atq, control_atq), digits = 3)
# -> same results expect for that m3_atq 
# Now m3_atq again but with clustered standard errors:
m3_atq <- feols(atq_change ~ viol_new +
                  ocf_atqaver + leverage + interestex_atqaver+ networth_atq + 
                  currentratio  + mb + atq_log + ppentq_atq + atq_diff + ppentq_atq_diff | 
                  calendar + ffi + fqtr , cluster = ~ gvkey + calendar, data = pdata) 
m4_atq <- feols(atq_change ~ viol_new  + viol_once + viol_multi + 
                    ocf_atqaver + leverage + interestex_atqaver+ networth_atq + 
                    currentratio  + mb + atq_log + ppentq_atq + atq_diff + ppentq_atq_diff | 
                    calendar + ffi + fqtr, cluster = ~ gvkey + calendar, data = pdata) 
# Number of firms for m1_atq:
atq1_firms <- pdata %>% drop_na(atq_change) %>% select(gvkey) %>% distinct() %>% nrow()
# Number of firms for m2_atq, m3_atq and m4_atq: 
atq234_firms <- pdata %>% drop_na(atq_change, ocf_atqaver, leverage, interestex_atqaver, 
                                  networth_atq, currentratio, mb, atq_log, ppentq_atq, atq_diff, 
                                  ppentq_atq_diff) %>% select(gvkey) %>% distinct() %>% nrow()
# model: Change in ln(PPENT)
m1_ppent <- lm(ppentq_change ~ viol_new, data = pdata) 
m2_ppent <- lm(ppentq_change ~ viol_new + ocf_atqaver+ leverage + interestex_atqaver+ 
               networth_atq + currentratio  + mb  + atq_log + ppentq_atq + atq_diff + 
               ppentq_atq_diff, data = pdata)
m3_ppent <- feols(ppentq_change ~ viol_new  + 
                    ocf_atqaver + leverage + interestex_atqaver+ networth_atq + 
                    currentratio  + mb + atq_log + ppentq_atq + atq_diff + ppentq_atq_diff | 
                    fqtr + ffi + calendar, cluster = ~ gvkey + calendar, data = pdata) 
m4_ppent <- feols(ppentq_change ~ viol_new  + viol_once + viol_multi + 
                    ocf_atqaver + leverage + interestex_atqaver+ networth_atq + 
                    currentratio  + mb + atq_log + ppentq_atq + atq_diff + ppentq_atq_diff | 
                    fqtr + ffi + calendar, cluster = ~ gvkey + calendar, data = pdata) 
# Number of firms for m1_ppent:
ppent1_firms <- pdata %>% drop_na(ppentq_change) %>% select(gvkey) %>% distinct() %>% nrow()
# Number of firms for m2_ppent, m3_ppent and m4_ppent: 
ppent234_firms <- pdata %>% drop_na(ppentq_change, ocf_atqaver, leverage, interestex_atqaver, 
                                  networth_atq, currentratio, mb, atq_log, ppentq_atq, atq_diff, 
                                  ppentq_atq_diff) %>% select(gvkey) %>% distinct() %>% nrow()
# to check in R console: 
screenreg(list(m1_atq, m2_atq, m3_atq, m4_atq), digits = 3)
screenreg(list(m1_ppent, m2_ppent, m3_ppent, m4_ppent), digits = 3)
# create LaTeX code: 
table_reg1 <- texreg(list(m1_atq, m2_atq, m3_atq, m4_atq, 
                          m1_ppent, m2_ppent, m3_ppent, m4_ppent), 
                     custom.header = list("\\textbf{Change in Ln(Assets)}" = 1:4, 
                                          "\\textbf{Change in Ln(PPENT)}" = 5:8),
                     custom.model.names = c("(1) I", "(2) II", "(3) III", "(4) IV", 
                                            "(5) I", "(6) II", "(7) III", "(8) IV"),
                     siunitx = TRUE, use.packages = TRUE, caption.above = TRUE, 
                     label = "reg1", reorder.coef = c(2,13,14,3,4,5,6,7,8,9,10,11,12,1),
                     stars = c(0.1, 0.05, 0.01), booktabs = TRUE, single.row = FALSE,
                     include.adjrs = TRUE , include.bic = FALSE, float.pos = "h", 
                     sideways=TRUE, return.string=TRUE, digits = 3, 
                     custom.gof.rows = list(`No. of firms` = c(atq1_firms, atq234_firms, atq234_firms , atq234_firms , 
                                                               ppent1_firms, ppent234_firms, ppent234_firms , ppent234_firms),
                                            `FE \\& Clustered SE` = c("No", "No", "Yes", "Yes", "No", "No", "Yes", "Yes")),
                     custom.coef.names = c("Constant", "New financial covenant violation",
                                           "Operating cash flow/ Average assets", 
                                           "Leverage", "Interest expense/ Average assets", 
                                           "Net worth/ Assets","Current ratio",
                                           "Market-to-book ratio", "Ln(Assets)", 
                                           "PPENT/ Assets", "First differences (Assets)", 
                                           "First differences (PPENT/Assets)","One-time violator", 
                                           "Frequent violator" ),
                     caption = "\\textbf{Panel A. Regression of Investment Measures. DRAFT.} \\\\
                     Automated output. The version in the submitted thesis 
                     was manually adjusted due to style and space constraints.")
table_reg1 <- gsub("\\begin{tabular}" ,"\\resizebox{\\textwidth}{!}{\\begin{tabular}"
                   ,table_reg1 ,fixed=TRUE)
table_reg1 <- gsub("\\end{tabular}" ,"\\end{tabular}}" ,table_reg1 ,fixed=TRUE)
### save table ------------------------------------------------------------------
write_file(table_reg1, 'output/tables/my_Table3A.tex')



### 3.3.2 Panel B --------------------------------------------------------------
# model: Change in CAPEX/Average assets 
m1_capex <- lm(capex_atqaver_change ~ viol_new, data = pdata) 
m2_capex <- lm(capex_atqaver_change ~ viol_new + ocf_atqaver+ leverage + interestex_atqaver+ 
               networth_atq + currentratio  + mb  + atq_log + ppentq_atq + atq_diff + 
               ppentq_atq_diff, data = pdata)
m3_capex <- feols(capex_atqaver_change ~ viol_new  + 
                    ocf_atqaver + leverage + interestex_atqaver+ networth_atq + 
                    currentratio  + mb + atq_log + ppentq_atq + atq_diff + ppentq_atq_diff | 
                    calendar + ffi + fqtr, cluster = ~ gvkey + calendar, data = pdata) 
m4_capex <- feols(capex_atqaver_change ~ viol_new  + viol_once + viol_multi + 
                    ocf_atqaver + leverage + interestex_atqaver+ networth_atq + 
                    currentratio  + mb + atq_log + ppentq_atq + atq_diff + ppentq_atq_diff | 
                    calendar + ffi + fqtr, cluster = ~ gvkey + calendar, data = pdata) 
# Number of firms for m1_capex:
capex1_firms <- pdata %>% drop_na(capex_atqaver_change) %>% select(gvkey) %>% distinct() %>% nrow()
# Number of firms for m2_capex, m3_capex and m4_capex: 
capex234_firms <- pdata %>% drop_na(capex_atqaver_change, ocf_atqaver, leverage, interestex_atqaver, 
                                 networth_atq, currentratio, mb, atq_log, ppentq_atq, atq_diff, 
                                 ppentq_atq_diff) %>% select(gvkey) %>% distinct() %>% nrow()
# model: Change in R&D/Assets
m1_xrd <- lm(xrd_atq_change ~ viol_new, data = pdata) 
m2_xrd <- lm(xrd_atq_change ~ viol_new + ocf_atqaver+ leverage + interestex_atqaver+ 
                 networth_atq + currentratio  + mb  + atq_log + ppentq_atq + atq_diff + 
                 ppentq_atq_diff, data = pdata)
m3_xrd <- feols(xrd_atq_change ~ viol_new  + 
                    ocf_atqaver + leverage + interestex_atqaver+ networth_atq + 
                    currentratio  + mb + atq_log + ppentq_atq + atq_diff + ppentq_atq_diff | 
                    calendar + ffi + fqtr, cluster = ~ gvkey + calendar, data = pdata) 
m4_xrd <- feols(xrd_atq_change ~ viol_new  + viol_once + viol_multi + 
                    ocf_atqaver + leverage + interestex_atqaver+ networth_atq + 
                    currentratio  + mb + atq_log + ppentq_atq + atq_diff + ppentq_atq_diff | 
                    calendar + ffi + fqtr, cluster = ~ gvkey + calendar, data = pdata) 
# Number of firms for m1_xrd:
xrd1_firms <- pdata %>% drop_na(xrd_atq_change) %>% select(gvkey) %>% distinct() %>% nrow()
# Number of firms for m2_xrd, m3_xrd and m4_xrd: 
xrd234_firms <- pdata %>% drop_na(xrd_atq_change, ocf_atqaver, leverage, interestex_atqaver, 
                                 networth_atq, currentratio, mb, atq_log, ppentq_atq, atq_diff, 
                                 ppentq_atq_diff) %>% select(gvkey) %>% distinct() %>% nrow()
# to check in R console: 
screenreg(list(m1_capex, m2_capex, m3_capex, m4_capex), digits = 3)
screenreg(list(m1_xrd, m2_xrd, m3_xrd,m4_xrd), digits = 3)
# create LaTeX code: 
table_reg2 <- texreg(list(m1_capex, m2_capex, m3_capex, m4_capex, 
                          m1_xrd, m2_xrd, m3_xrd, m4_xrd), 
                     custom.header = list("\\textbf{Change in CAPEX/ Average assets}" = 1:4, 
                                          "\\textbf{Change in R\\&D/ Assets}" = 5:8),
                     custom.model.names = c("(9) I", "(10) II", "(11) III", "(12) IV", 
                                            "(13) I", "(14) II", "(15) III", "(16) IV"),
                     siunitx = TRUE, use.packages = TRUE, caption.above = TRUE, 
                     reorder.coef = c(2,13,14,3,4,5,6,7,8,9,10,11,12,1),
                     stars = c(0.1, 0.05, 0.01), booktabs = TRUE, single.row = FALSE,
                     include.adjrs = TRUE , include.bic = FALSE, float.pos = "h", 
                     sideways=TRUE, return.string=TRUE, digits = 3, 
                     custom.gof.rows = list(`No. of firms` = c(capex1_firms, capex234_firms, capex234_firms , capex234_firms , 
                                                               xrd1_firms, xrd234_firms, xrd234_firms , xrd234_firms),
                                            `FE \\& Clustered SE` = c("No", "No", "Yes", "Yes", "No", "No", "Yes", "Yes")),
                     custom.coef.names = c("Constant", "New financial covenant violation",
                                           "Operating cash flow/ Average assets", 
                                           "Leverage", "Interest expense/ Average assets", 
                                           "Net worth/ Assets","Current ratio",
                                           "Market-to-book ratio", "Ln(Assets)", 
                                           "PPENT/ Assets", "First differences (Assets)", 
                                           "First differences (PPENT/Assets)","One-time violator", 
                                           "Frequent violator" ),
                     caption = "Table 3: \\textbf{Panel B. Regression of Investment Measures (continued). DRAFT.} \\\\
                     Automated output. The version in the submitted thesis 
                     was manually adjusted due to style and space constraints.")
table_reg2 <- gsub("\\begin{tabular}" ,"\\resizebox{\\textwidth}{!}{\\begin{tabular}"
                   ,table_reg2 ,fixed=TRUE)
table_reg2 <- gsub("\\end{tabular}" ,"\\end{tabular}}" ,table_reg2 ,fixed=TRUE)
### save table ------------------------------------------------------------------
write_file(table_reg2, 'output/tables/my_Table3B.tex')



## 3.4 comparison of regression results with reference paper (pages 1738, 1739) ----
mref_atq <- m3_atq
mref_atq <- extract(mref_atq)
mref_atq@gof.names <- mref_atq@gof.names[-c(2:6,8)]
mref_atq@gof.decimal <- mref_atq@gof.decimal[-c(2:6,8)]
mref_atq@gof <- mref_atq@gof[-c(2:6,8)]
mref_atq@gof[1] <- 150474 # no of obs. in model ln(atq) in ref paper
mref_atq@gof[2] <- 0.17 # (adj) R^2 in model ln(atq) in ref paper

mref_ppent <- m3_ppent
mref_ppent <- extract(mref_ppent)
mref_ppent@gof.names <- mref_ppent@gof.names[-c(2:6,8)]
mref_ppent@gof.decimal <- mref_ppent@gof.decimal[-c(2:6,8)]
mref_ppent@gof <- mref_ppent@gof[-c(2:6,8)]
mref_ppent@gof[1] <- 150019 # no of obs. in model ln(ppent) in ref paper
mref_ppent@gof[2] <- 0.14 # (adj) R^2 in model ln(ppent) in ref paper

mref_capex <- m3_capex
mref_capex <- extract(mref_capex)
mref_capex@gof.names <- mref_capex@gof.names[-c(2:6,8)]
mref_capex@gof.decimal <- mref_capex@gof.decimal[-c(2:6,8)]
mref_capex@gof <- mref_capex@gof[-c(2:6,8)]
mref_capex@gof[1] <- 145442 # no of obs. in model capex/average assets in ref paper
mref_capex@gof[2] <- 0.06 # (adj) R^2 in model capex/average assets in ref paper

table_regcompare <- suppressWarnings(texreg(list(m3_atq, mref_atq, m3_ppent, mref_ppent, 
                                                 m3_capex, mref_capex), 
                                            custom.header = list("\\textbf{Change in Ln(Assets)}" = 1:2, 
                                                                 "\\textbf{Change in Ln(PPENT)}" = 3:4,
                                                                 "\\textbf{Change in CAPEX/ Average assets}" = 5:6),
                                            custom.model.names = c("(1)", "(2)", "(3)", "(4)", 
                                                                   "(5)", "(6)"),
                                            siunitx = TRUE, use.packages = TRUE, caption.above = TRUE, 
                                            label = "regcompare",
                                            override.coef = list(0, # my model atq 
                                                                 c(-0.041,0.309,-0.095,0.604,0.031,-0.005,0.063,0,0,0,0), # model atq (ref paper)
                                                                 0, # my model ppent
                                                                 c(-0.042,0.312,-0.024, 0.785, 0.110, 0.006,0.047,0,0,0,0), # model ppent (ref paper)
                                                                 0, # my model capex
                                                                 c(-0.005,0.011,-0.007, 0.122, 0.001, 0.001,-0.001,0,0,0,0)), # model capex (ref paper)
                                            override.se =  list(0, # my model atq 
                                                                c(0.007,0.030,0.023,0.208,0.023,0.001,0.003,0,0,0,0), # model atq (ref paper)
                                                                0, # my model ppent
                                                                c(0.011,0.022,0.029, 0.240, 0.023, 0.001,0.002,0,0,0,0), # model ppent (ref paper)
                                                                0, # my model capex
                                                                c(0.002,0.002,0.003, 0.021, 0.001, 0.000,0.000 ,0,0,0,0)), # model capex (ref paper)               
                                            override.pval = list(0, # my model atq 
                                                                 c(0,0,0,0,0.2,0,0,0,0,0,0), # model atq (ref paper)
                                                                 0, # my model ppent
                                                                 c(0,0,0.2, 0, 0, 0,0,0,0,0,0), # model ppent (ref paper)
                                                                 0, # my model capex
                                                                 c(0.03,0,0.03, 0, 0.2, 0,0 ,0,0,0,0)), # model capex (ref paper)  
                                            stars = c(0.1, 0.05, 0.01), booktabs = TRUE, single.row = FALSE,
                                            include.adjrs = TRUE , include.bic = FALSE, float.pos = "h", 
                                            sideways=TRUE, return.string=TRUE, digits = 3, 
                                            omit.coef=c('atq_log|ppentq_atq|atq_diff|ppentq_atq_diff'),
                                            custom.coef.names = c("New financial covenant violation",
                                                                  "Operating cash flow/ Average assets", 
                                                                  "Leverage", "Interest expense/ Average assets", 
                                                                  "Net worth/ Assets","Current ratio",
                                                                  "Market-to-book ratio"),
                                            custom.gof.rows = list(`Controls` = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                                                                   `FE \\& Clustered SE` = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes")),
                                            caption = "\\textbf{Comparison of Regression Results. DRAFT} \\\\
                     Automated output. The table version in the submitted paper 
                     was manually adjusted in style due to space constraints."))
table_regcompare <- gsub("\\begin{tabular}" ,"\\resizebox{\\textwidth}{!}{\\begin{tabular}"
                         ,table_regcompare ,fixed=TRUE)
table_regcompare <- gsub("\\end{tabular}" ,"\\end{tabular}}" ,table_regcompare ,fixed=TRUE)
## save table ------------------------------------------------------------------
write_file(table_regcompare, 'output/tables/my_Table4.tex')



## 3.5 averages per group -------------------------------------------------------
table_meandiff <- data.frame(matrix(ncol = 6, nrow = 17))
colnames(table_meandiff) <- c("X", "OneTimeViolator", "FrequentViolator", "Difference", "tStatistic", "pValue")
vars <- c("ocf_atqaver", "leverage", "interestex_atqaver", "networth_atq",
                  "currentratio", "mb", "atq_change","ppentq_change","capex_atqaver",
                  "xrd_atq","atq","ppentq", "atq_ratio", "ppent_ratio", "ppentq_atq", 
          "No. of obs.", "No. of firms")
table_meandiff$X <- vars
sets <- c("once", "frequent")
for(i in 1:{length(vars)-2}){
var <- vars[i]
# once:
once <- data %>% filter(viol_once == 1) %>% drop_na(var)
set <- sets[1]
s <- get(set)[names(get(set)) == var]
s <- s %>% drop_na()
x1 <- mean(s[,1], na.rm = TRUE)
table_meandiff$OneTimeViolator[i] <- round(x1,3)
n1 <- length(s[,1])
# un-clustered SE:
# se1 <- summary(lm(s[,1] ~ 1))$coefficient[2] # equals sqtr(var(s[,1], na.rm = TRUE)) 
# clustered SE:
se1 <- coeftest(lm(s[,1] ~ 1, data = get(set)), vcov = vcovCL, cluster = ~ gvkey)[2]  
sd1 <- se1* sqrt(n1)
var1 <- sd1^2

# frequent:
frequent <- data %>% filter(viol_multi == 1) %>% drop_na(var)
set <- sets[2]
s <- get(set)[names(get(set)) == var]
s <- s %>% drop_na()
x2 <- mean(s[,1], na.rm = TRUE)
table_meandiff$FrequentViolator[i] <- round(x2,3)
n2 <- length(s[,1])
# un-clustered SE:
# se2 <- summary(lm(s[,1] ~ 1))$coefficient[2] # equals sqtr(var(s[,1], na.rm = TRUE)) 
# clustered SE:
se2 <- coeftest(lm(s[,1] ~ 1, data = get(set)), vcov = vcovCL, cluster = ~ gvkey)[2]  
sd2 <- se2* sqrt(n2)
var2 <- sd2^2

meandiff <- (x1 - x2)
table_meandiff$Difference[i]  <- round(meandiff,3)
tstat <- meandiff /sqrt(var1/n1 + var2/n2)
table_meandiff$tStatistic[i]  <- round(tstat,3)
dfree <- (var1/n1 + var2/n2)^2 / ( ((var1/n1)^2 / (n1-1)) + ((var2/n2)^2 / (n2-1)))
crit <- qt(p = 0.05, df = dfree)
pvalue <- 2*pt(-abs(tstat),df=dfree)
table_meandiff$pValue[i]  <- round(pvalue,3)

}
nrow(data)
table_meandiff$OneTimeViolator[16] <- data %>% filter(viol_once ==1) %>% count()
table_meandiff$OneTimeViolator[17] <- data %>% filter(viol_once ==1)   %>% select(gvkey)%>% distinct() %>% nrow()
table_meandiff$FrequentViolator[16] <- data %>% filter(viol_multi ==1) %>% count()
table_meandiff$FrequentViolator[17] <- data %>% filter(viol_multi ==1)   %>% select(gvkey)%>% distinct() %>% nrow()
table_meandiff$X <- c("Operating cash flow/ Average assets",  
                 "Leverage", "Interest expense/ Average assets", 
                 "Net worth/ Assets", "Current ratio","Market-to-book", 
                 "Change in Ln(Assets)", "Change in Ln(PPENT)",    
                 "CAPEX/ Average assets", "R&D/ Assets", 
                 "Assets ($ millions)",
                 "PPENT ($ millions)", "Assets (4 leads)/ Assets", 
                 "PPENT (4 leads)/ PPENT", "PPENT/ Assets",
                 "No. of obs.", "No. of firms")
colnames(table_meandiff) <- c(NA, "One-time Violator (1)", "Frequent Violator (2)", "Difference (1) - (2)", 
                              "t-Statistic", "p-Value")
table_meandiff <- print(xtable(table_meandiff, digits = 3, type = "latex", method = "compact", 
             caption = '\\textbf{Averages for One-time Violator and Frequent Violator Groups, DRAFT.} \\\\
                     Automated output. The table version in the submitted paper 
                     was manually adjusted in style due to space constraints.
                     The resizebox{} argument was manually added to the .tex code.'), 
      include.rownames=FALSE, caption.placement = 'top')
## save table ------------------------------------------------------------------
write_file(table_meandiff, 'output/tables/my_Table5.tex')




