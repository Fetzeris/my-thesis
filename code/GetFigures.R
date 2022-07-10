# credits  ---------------------------------------------------------------------
'
Author:     Isabell Fetzer
Purpose:    This R script generates all figures found in the submitted paper as 
            .png files: 
            - Financial Covenant Violation from 1997 to 2007
            - Time-To-Event(Violation)-Plot: Firms Investment Decision around New Financial Covenant Violations
            The file "my_dataset.csv" -obtained by the R skript "GetDataset.R"- 
            is required to produce all figures
' 
# 1 initialization -------------------------------------------------------------
rm(list=ls(all=TRUE))         # clear environment
graphics.off()                # clear console
# set working directory 
setwd("~/Library/Mobile\ Documents/com~apple~CloudDocs/Uni/BA - icloud")
# install & load packages
libraries = c("dplyr","tidyverse", "gridExtra", "lemon", "cowplot", "stringr")
lapply(libraries, function(x) if (!(x %in% installed.packages())) 
{ install.packages(x) })
lapply(libraries, library, quietly = TRUE, character.only = TRUE)


# 2 load in datatset -----------------------------------------------------------
data <- read.csv("data/my_dataset.csv")


# 3 figures  -------------------------------------------------------------------
## 3.1 financial covenant violation from 1997 to 2007 --------------------------
figure1 <- data  %>%  
  group_by(gvkey) %>%
  mutate(viol_newonce = viol_once + viol_new, 
         viol_newonce = replace(viol_newonce, viol_newonce ==1, 0), 
         viol_newonce = replace(viol_newonce, viol_newonce ==2, 1)) %>%
  filter(fyearq > 1996, fyearq < 2008) %>% 
  group_by(gvkey, fyearq) %>%
  summarise(viol_inyear = max(cumsum((viol))), 
            viol_yes = viol_inyear>0, 
            newviol_inyear = max(cumsum(viol_new)),
            newviol_yes = newviol_inyear > 0, 
            violnewonce_inyear = max(cumsum((viol_newonce))), 
            violnewonce_yes = violnewonce_inyear >0) %>%
  group_by(fyearq) %>%
  summarise(viol_per = sum(viol_yes)/n(), 
            newviol_per = sum(newviol_yes)/n(), 
            violnewonce_per = sum(violnewonce_yes)/n()) %>% 
  mutate(fyearq = lubridate::ymd(fyearq, truncated = 2L)) %>%
  ggplot(aes(x = fyearq, y = viol_per)) + 
  geom_line(mapping=aes( y = viol_per,  color = "Debt covenant violation"), size = 0.7) +  
  geom_line(aes(y = newviol_per, color = "New debt covenant violation"), size = 0.7,  linetype = "longdash") +  
  geom_line(aes(y = violnewonce_per), size = 0.3, color = "#d9d9d9") + 
  scale_color_manual(name = "", values = c("Debt covenant violation" = "#1d77b4",
                                           "New debt covenant violation" = "#aa2141"))+
  geom_ribbon(aes(ymin=violnewonce_per , ymax=pmax(newviol_per,violnewonce_per ), fill="Frequent violator"), alpha=0.5) +
  geom_ribbon(aes(ymin=pmin(newviol_per,violnewonce_per ), ymax=0, fill="One-time violator"), alpha=0.5) + 
  scale_fill_manual("",values=c("#d9d9d9", "#b3b3b3")) + 
  scale_x_date( date_labels = "%Y", date_breaks = "1 year") + 
  scale_y_continuous(limits = c(0,0.2), breaks = seq(0,0.2, by = 0.025), labels= c(0," ",5," ",10," ",15," ",20)) +
  theme_light() + 
  ggtitle("") +
  labs( x = "Fiscal year", y = "Percentage of violator firms")  +
  theme( axis.title.x= element_text(colour = "gray40", size = 9),
         axis.title.y = element_text(colour = "gray40", size = 9),
         axis.text = element_text(size = 7),
         legend.position = "top", 
         legend.margin=margin(),
         legend.box.margin=margin(-12,-12,-12,-12),
         panel.grid.major.x = element_blank(), 
         panel.grid.minor.x = element_blank(), 
         panel.grid.major.y = element_line( size = 0.03, color = "black" ),
         panel.grid.minor.y = element_blank(), 
         panel.border = element_blank(), 
         plot.background = element_rect(fill = "transparent",colour = NA), 
         panel.background = element_rect(fill = "transparent", color = NA), 
         legend.background = element_rect(fill = "transparent", color = NA),
         legend.box.background = element_rect(fill = "transparent", color = NA), 
         legend.key = element_rect(fill = "transparent", color = NA),
         legend.text = element_text(colour = "gray40"),
         axis.line.x = element_line(size = 0.1, colour = "gray40"),
         axis.line.y = element_line(size = 0.1, colour = "gray40")) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE), 
         colour = guide_legend(nrow = 2, byrow = TRUE, order = 1))
## save figure -----------------------------------------------------------------
png("output/figures/my_Violations1997to2008.png", units = "in", width =5, height = 4.5, res = 1000, bg = "transparent")
figure1
dev.off()


## 3.2 firms investment decision around new financial covenant violations ------
firms_viol <- data  %>% group_by(gvkey) %>% 
  mutate(nr_viol = max(cumsum(viol),  na.rm = TRUE)) %>% 
  select(gvkey, nr_viol )%>% distinct() %>% filter(nr_viol > 0) %>% select(gvkey)
firms_viol <- as.vector(firms_viol$gvkey)
# make panel frame: Fill all missing quarterly observations up to panel data frame 
firms_all <- unique(firms_viol)
calenders <- c(19962:19964, 19971:19974 , 19981:19984,  19991:19994,  20001:20004, 
                   20011:20014, 20021:20024, 20031:20034, 20041:20044, 20051:20054, 
                   20061:20064, 20071:20074, 20081:20084)
panel_frame <- data.frame(calender= rep(calenders , length(unique(firms_viol))), 
                          gvkey = rep(firms_all, each = length(calenders))) 
data$calender <- as.integer(data$calender)
data$gvkey <- as.integer(data$gvkey)
panel_frame <- panel_frame %>% left_join(data)
# the computation of following for-loop can take up to 3min!
data_figure2_all <- data.frame(matrix(ncol = 4, nrow = 0)) # create dataframe which is to be filled in the next loop 
for(i in 1:length(firms_viol)){
  firmnr <- firms_viol[i]
  crop <- panel_frame  %>% 
    select(gvkey, quarter, viol, viol_once, viol_multi,  
           atq_log, ppentq_log, capex_atqaver, xrd_atq) %>%
    filter(gvkey == firmnr)
  crop$viol_new_time <- NA       # 00001xxxx, whereby x:= either 0 or 1 , i.e. the new debt covenant violation is followed by a binary sequence of length 4 (i.e. excludes cases for which no data is available in the next 4 quarters after new debt covenant happened))
  crop$viol[is.na(crop$viol)] <- 9 # recode NA in "viol" variable to value 9 
  pattern <- paste0(crop$viol, collapse="") # grasp the viol pattern for each firm, e.g. for firm with gvkey == "1021" : "9999000000000111100100110110101000000000099010009"
  # grasp pattern: 00001xxxx , whereby x:= either 0 or 1
  zzzzoxxxx <- str_locate_all(pattern, "(?<=0{4})1(?=[0,1]{4})") 
  zzzzoxxxx_rownr <- do.call(rbind, zzzzoxxxx)[,1] # save row number of first 1 (= new debt covenant violation)
  zzzzoxxxx_rownr
  # fill new variable "viol_new_time", if viol == 1 AND is part of pattern "00001xxxx, whereby x:= either 0 or 1" ...
  if(length(zzzzoxxxx_rownr)>0){ 
    for (i in 1:length(zzzzoxxxx_rownr)) {
      row <- zzzzoxxxx_rownr[i]
      crop$viol_new_time[row] <- 0 # ... with value 0 at the 5th position in "00001xxxx" 
      crop$viol_new_time[row+1] <- 1 # ... with value 1 at the 6th  position in "00001xxxx" 
      crop$viol_new_time[row+2] <- 2 # ... with value 2 at the 7th  position in "00001xxxx" 
      crop$viol_new_time[row+3] <- 3 # ... with value 3 at the 8th  position in "00001xxxx" 
      crop$viol_new_time[row+4] <- 4 # ... with value 4 at the 9th  position in "00001xxxx"  
      crop$viol_new_time[row-4] <- -4 # ... with value -4 at the 4th  position in "00001xxxx" 
      crop$viol_new_time[row-3] <- -3 # ... with value -3 at the 3rd  position in "00001xxxx" 
      crop$viol_new_time[row-2] <- -2 # ... with value -2 at the 2nd  position in "00001xxxx" 
      crop$viol_new_time[row-1] <- -1 # ... with value -1 at the 1st  position in "00001xxxx" 
      data_figure2_all <- rbind(data_figure2_all, crop[(row-4):(row+4),])
    }
  }
}
data_figure2_once <- data_figure2_all %>% filter(viol_once==1) 
data_figure2_multi <- data_figure2_all %>% filter(viol_multi==1)
makefigure2 <- function(df, var, a, breaks_mean,breaks_median, ylim, title1, legendposition){ 
  z <- df %>% select(var, viol_new_time, gvkey) %>% drop_na(var)
  colnames(z)[1] <- c("x")
  a_current <- a
  breaks_mean_current <- breaks_mean
  breaks_median_current <- breaks_median
  ylim_current <- ylim
  crop1 <- as.data.frame(aggregate(z$x, list(z$viol_new_time), mean))
  crop1$mean <- crop1$x
  crop1$x <- NULL
  crop2 <- as.data.frame(aggregate(z$x, list(z$viol_new_time), median))
  crop2$median <- crop2$x
  crop2$x <- NULL
  merged <- merge(crop1, crop2)
  merged$title1 <-  title1
  merged$nr_obs <- nrow(z)
  merged$nr_firms <- length(unique(z$gvkey))
  if(unique(nchar(merged$nr_obs)>=4)){
    nr_obsv <- paste0(str_sub(merged$nr_obs, start= 1, end = (unique(nchar(merged$nr_obs))-3)), ",", 
                      str_sub(merged$nr_obs, start= -3),";")
  }else {
    nr_obsv <- paste0(merged$nr_obs,";")
  }
  if(unique(nchar(merged$nr_firms)>=4)){
    nr_firm <- paste0(str_sub(merged$nr_firms, start= 1, end = (unique(nchar(merged$nr_obs))-3)), ",", 
                      str_sub(merged$nr_firms, start= -3),")")
  }else {
    nr_firm <- paste0(merged$nr_firms,")")
  }
  
  caption <- paste("(No. of obs.", nr_obsv,
                   "No. of firms", nr_firm)
  
 figure2 <- ggplot(merged , aes(Group.1, mean)) +
    geom_line(aes(colour = "Mean"), size = 0.7) + 
    geom_line(aes(y = median + a_current, colour = "Median"), size = 0.7,  linetype = "longdash") +
   scale_color_manual(name = "", values = c("Mean" = "#1d77b4", "Median" = "#aa2141")) + 
   scale_y_continuous("Mean", breaks = breaks_mean_current, 
                       sec.axis = sec_axis(~ (. - a_current), name = "Median", 
                       breaks = breaks_median_current))+
    coord_cartesian(ylim= ylim_current) + 
    scale_x_continuous(breaks = c(-4,-3,-2,-1,0,1,2,3,4)) + 
    theme_light() + 
    facet_grid(. ~ title1 ) + 
    labs( x = "Quarters around new covenant violation", 
          caption = caption)  +
    theme( axis.title.x= element_text(colour = "gray40", size =9),
           axis.title.y = element_text(colour = "gray40", size = 9),
           panel.grid.major.x = element_blank(), 
           panel.grid.minor.x = element_blank(), 
           panel.grid.major.y = element_line( size = 0.03, color = "black" ),
           panel.grid.minor.y = element_blank(), 
           panel.border = element_blank(), 
           plot.background = element_rect(fill = "transparent",colour = NA), 
           panel.background = element_rect(fill = "transparent", color = NA), 
           legend.background = element_rect(fill = "transparent", color = NA),
           legend.box.background = element_rect(fill = "transparent", color = NA), 
           legend.key = element_rect(fill = "transparent", color = NA),
           legend.text = element_text(colour = "gray40"),
           legend.position =legendposition,
           axis.line.x = element_line(size = 0.1, colour = "gray40"),
           axis.line.y = element_line(size = 0.1, colour = "gray40"), 
           strip.text.x = element_text(size = 11, colour = "gray40"), 
           strip.background = element_rect(colour="gray80",fill="gray90"),
           plot.caption=element_text(margin=margin(t=2, b = 2),
                                     face="italic", size=9, color = "gray40",
                                     hjust = 0.5))+
    geom_vline(xintercept = -1, linetype="dotted", color = "gray40", size= 0.4) + 
    geom_vline(xintercept = 0, linetype="dotted", color = "gray40", size= 0.4)
 return(figure2)
}
# Legend
legend <- makefigure2(data_figure2_once, var = "atq_log", a = -0.03, 
                      breaks_mean = c(5.46, 5.5, 5.54, 5.58, 5.62), 
                      breaks_median = c(5.49, 5.53, 5.57, 5.61, 5.65), 
                      ylim = c(5.46,5.62), 
                      title1 = "One-time Violators",
                      legendposition = "bottom")

## 3.2.1 Panel A & B -----------------------------------------------------------
# Panel A: Ln(Assets)
f2atq_all <- makefigure2(data_figure2_all,var = "atq_log", a = 0.16, 
           breaks_mean = c(5.04, 5.06, 5.08, 5.1, 5.12, 5.14), 
           breaks_median = c(4.86, 4.88, 4.9, 4.92, 4.94, 4.96 ), 
           ylim = c(5.033, 5.125), 
           title1 = "All Violators", 
           legendposition = "none")
f2atq_multi <- makefigure2(data_figure2_multi, var = "atq_log", a = 0.18, 
           breaks_mean = c(4.96, 4.98, 5, 5.02, 5.04, 5.06), 
           breaks_median = c(4.78, 4.8, 4.82, 4.84, 4.86, 4.88 ), 
           ylim = c(4.96,5.065), 
           title1 = "Frequent Violators",
           legendposition = "none")
f2atq_once <- makefigure2(data_figure2_once, var = "atq_log", a = -0.03, 
           breaks_mean = c(5.46, 5.5, 5.54, 5.58, 5.62), 
           breaks_median = c(5.49, 5.53, 5.57, 5.61, 5.65), 
           ylim = c(5.46,5.62), 
           title1 = "One-time Violators",
           legendposition = "none")
gridded_atq <- grid.arrange(f2atq_all, f2atq_multi, f2atq_once, ncol=3) 
title_atq<-  ggplot() + 
  labs(title = "Panel A: Ln(Assets)") + 
  theme_minimal() + 
  theme(plot.title=element_text(hjust=0.05, size = 11.5))
figure2_atq <- cowplot::plot_grid(title_atq, gridded_atq, ncol = 1, 
                              rel_heights = c(0.05, 1))

# Panel B: ln(PPENT)
f2ppent_all <- makefigure2(data_figure2_all, var = "ppentq_log", a = 0.17, 
                           breaks_mean = c(3.36, 3.38, 3.4, 3.42, 3.44, 3.46, 3.48), 
                           breaks_median = c(3.21, 3.23, 3.25, 3.27, 3.29, 3.31), 
                           ylim = c(3.372, 3.487), 
                           title1 = "All Violators",
                           legendposition = "none")
f2ppent_multi <- makefigure2(data_figure2_multi, var = "ppentq_log", a = 0.2, 
                             breaks_mean = c(3.31, 3.33, 3.35, 3.37, 3.39, 3.41), 
                             breaks_median = c(3.11, 3.13, 3.15, 3.17, 3.19, 3.21), 
                             ylim = c(3.3, 3.42), 
                             title1 = "Frequent Violators",
                             legendposition = "none")
f2ppent_once <- makefigure2(data_figure2_once, var = "ppentq_log", a = -0.03, 
                            breaks_mean = c(3.88, 3.92, 3.96, 4), 
                            breaks_median = c(3.91, 3.95, 3.99, 4.03), 
                            ylim = c(3.86, 4.02), 
                            title1 = "One-time Violators",
                            legendposition = "none")
gridded_ppent <- grid.arrange(f2ppent_all, f2ppent_multi, f2ppent_once, ncol=3) 
title_ppent<-  ggplot() + 
  labs(title = "Panel B: Ln(PPENT)") + 
  theme_minimal() + 
  theme(plot.title=element_text(hjust=0.05, size = 11.5))
figure2_ppent <- cowplot::plot_grid(title_ppent, gridded_ppent, ncol = 1, 
                                  rel_heights = c(0.05, 1))
legend_atq <- g_legend(legend)
legend_ppent <- g_legend(legend)
legend_atq$vp$x <- unit(.275, 'npc')
legend_atq$vp$y <- unit(0.97, 'npc')
legend_ppent$vp$x <- unit(.279, 'npc')
legend_ppent$vp$y <- unit(0.47, 'npc')
## save figure -----------------------------------------------------------------
png("output/figures/my_TimeToViolation1.png", units = "in", width =12.5, height = 6.5, res = 1000, bg = "transparent")
cowplot::plot_grid(figure2_atq, figure2_ppent, nrow = 2, scale = 0.9)
grid::grid.draw(legend_atq)
grid::grid.draw(legend_ppent)
dev.off()


## 3.2.2 Panel C & D -----------------------------------------------------------
# Panel C: Capex/Average Assets
f2capex_all <- makefigure2(data_figure2_all, var = "capex_atqaver", a = 0.026, 
                           breaks_mean = c(0.045, 0.05, 0.055, 0.06, 0.065, 0.07), 
                           breaks_median = c(0.019, 0.024, 0.029, 0.034, 0.039, 0.044), 
                           ylim = c(0.045, 0.07), 
                           title1 = "All Violators",
                           legendposition = "none")
f2capex_multi <- makefigure2(data_figure2_multi, var = "capex_atqaver", a = 0.026, 
                             breaks_mean = c(0.045, 0.05, 0.055, 0.06, 0.065, 0.07), 
                             breaks_median = c(0.019, 0.024, 0.029, 0.034, 0.039, 0.044), 
                             ylim = c(0.045, 0.07), 
                             title1 = "Frequent Violators",
                             legendposition = "none")
f2capex_once <- makefigure2(data_figure2_once, var = "capex_atqaver", a = 0.026, 
                            breaks_mean = c( 0.05, 0.055, 0.06, 0.065, 0.07, 0.075), 
                            breaks_median = c( 0.024, 0.029, 0.034, 0.039, 0.044, 0.049), 
                            ylim =  c(0.05, 0.075), 
                            title1 = "One-time Violators",
                            legendposition = "none")
gridded_capex <- grid.arrange(f2capex_all, f2capex_multi, f2capex_once, ncol=3) 
title_capex <-  ggplot() + 
  labs(title = "Panel C: CAPEX/ Average Assets") + 
  theme_minimal() + 
  theme(plot.title=element_text(hjust=0.05, size = 11.5))
figure2_capex <- cowplot::plot_grid(title_capex, gridded_capex, ncol = 1, 
                                  rel_heights = c(0.05, 1))
# Panel D: XRD/Assest
f2xrd_all <- makefigure2(data_figure2_all, var = "xrd_atq", a = 0.01, 
                         breaks_mean = c( 0.022, 0.024, 0.026, 0.028), 
                         breaks_median = c( 0.012, 0.014, 0.016, 0.018), 
                         ylim =  c(0.022, 0.028), 
                         title1 = "All Violators",
                         legendposition = "none")
f2xrd_multi <- makefigure2(data_figure2_multi, var = "xrd_atq", a = 0.009, 
                           breaks_mean = c( 0.021, 0.023, 0.025, 0.027), 
                           breaks_median = c( 0.012, 0.014, 0.016, 0.018), 
                           ylim =  c(0.021, 0.027), 
                           title1 = "Frequent Violators",
                           legendposition = "none")
f2xrd_once <- makefigure2(data_figure2_once, var = "xrd_atq", a = 0.015, 
                          breaks_mean = c( 0.025, 0.029, 0.033, 0.037), 
                          breaks_median = c( 0.01, 0.014,0.018, 0.022), 
                          ylim =  c(0.025, 0.037), 
                          title1 = "One-time Violators",
                          legendposition = "none")
gridded_xrd <- grid.arrange(f2xrd_all, f2xrd_multi, f2xrd_once, ncol=3) 
title_xrd <-  ggplot() + 
  labs(title = "Panel D: R&D/ Assets") + 
  theme_minimal() + 
  theme(plot.title=element_text(hjust=0.05, size = 11.5))
figure2_xrd <- cowplot::plot_grid(title_xrd, gridded_xrd, ncol = 1, 
                                    rel_heights = c(0.05, 1))
legend_capex <- g_legend(legend)
legend_xrd <- g_legend(legend)
legend_capex$vp$x <- unit(.35, 'npc')
legend_capex$vp$y <- unit(0.97, 'npc')
legend_xrd$vp$x <- unit(.287, 'npc')
legend_xrd$vp$y <- unit(0.47, 'npc')
## save figure -----------------------------------------------------------------
png("output/figures/my_TimeToViolation2.png", units = "in", width =12.5, height = 6.5, res = 1000, bg = "transparent")
cowplot::plot_grid(figure2_capex, figure2_xrd, nrow = 2, scale = 0.9)
grid::grid.draw(legend_capex)
grid::grid.draw(legend_xrd)
dev.off()


