rm(list=ls())
setwd("C:/Users/Angelica Ronconi/OneDrive/Desktop/CORSO ARCA- OPEN SCIENCE/JURE2022")

#Libraries
source("~/R/Statistical Methods/Pippo/Datasets/utilities.R")
library(tableone)
library(tableone)
library(ggplot2)
library(nlme)
library(lme4)
library(ggeffects)
library(sjPlot)
library(interactions)
library(lattice)

#Dataset
datax = read.csv2("C:/Users/Angelica Ronconi/OneDrive/Desktop/CORSO ARCA- OPEN SCIENCE/JURE2022/JURE2022/Data/jure_mydataset.csv")

#datax$Reading_Comprehension_levels<- sjmisc:: rec(datax$Reading_Comprehension, rec= "1:12= 0; 13:18 = 1; 19:24= 2" ) #ricodifica i valori in una nuova colonna
#datax$Reading_Comprehension_levels <- factor(datax$Reading_Comprehension_levels, levels= c(0,1, 2), labels = c("Low", "Intermediate", "High"))
#datax$Gender<-factor(datax$Gender, levels= c(1, 2), labels = c("Boys", "Girls"))
#datax$Medium<-factor(datax$Medium, levels= c("C", "D"), labels = c("Paper", "Digital"))
#str(datax)

#write.table(datax, file = "mydataset.csv",sep = "\t", row.names = F)

mydataset <- read.table(file = "mydataset.csv", header= T)
str(mydataset)

table(mydataset$Reading_Comprehension_levels, group= mydataset$Medium)
#        Paper Digital
#Low       44      44
#Medium    79      79
#High      27      27

#Descriptive plots (for numeric variables)


ggplot(data = mydataset,mapping = aes(y = Reading_Time,x=Reading_Comprehension_levels)) + geom_violin() + facet_wrap(~Medium) + 
  stat_summary(fun = "mean",geom = "point",color = "red") + stat_summary(fun = "median",geom = "point",color = "blue")
ggplot(data = mydataset,mapping = aes(y = Global_Comprehension,x=Medium)) + geom_violin() + facet_wrap(~Reading_Comprehension_levels) + 
  stat_summary(fun = "mean",geom = "point",color = "red") + stat_summary(fun = "median",geom = "point",color = "blue")
ggplot(data = mydataset,mapping = aes(y = Calibration_Bias,x=Medium)) + geom_violin() + facet_wrap(~Reading_Comprehension_levels) + 
  stat_summary(fun = "mean",geom = "point",color = "red") + stat_summary(fun = "median",geom = "point",color = "blue")

#Dependent variables
psych::describeBy(mydataset[,c(2:6,10)], group= mydataset$Medium)

#group: Paper
#                     vars   n   mean     sd median trimmed    mad    min     max   range  skew kurtosis    se
#Reading_Time            1 150 556.47 216.14 497.13  536.99 172.60 175.34 1214.60 1039.26  0.79     0.04 17.65
#Main_Idea               2 150   0.92   0.54   0.50    0.83   0.00   0.00    2.50    2.50  1.04     0.16  0.04
#Key_Points              3 150   0.69   0.98   0.00    0.52   0.00   0.00    4.00    4.00  1.16     0.18  0.08
#Relevant_Info           4 150   3.01   2.02   3.00    2.96   2.97   0.00    7.00    7.00  0.08    -0.98  0.16
#Global_Comprehension    5 150   4.62   2.70   4.50    4.53   2.97   0.50   12.00   11.50  0.24    -0.68  0.22
#Calibration_Bias        6 150   3.89   1.89   3.92    3.90   1.68  -2.17    8.44   10.61 -0.12     0.26  0.15
#-------------------------------------------------------------------------------------- 
#  group: Digital
#                     vars   n   mean     sd median trimmed    mad  min     max   range  skew kurtosis    se
#Reading_Time            1 150 550.34 259.92 510.83  531.81 289.12 68.7 1339.74 1271.04  0.57    -0.26 21.22
#Main_Idea               2 150   0.81   0.50   0.50    0.72   0.00  0.0    2.50    2.50  1.42     1.31  0.04
#Key_Points              3 150   0.53   0.77   0.00    0.38   0.00  0.0    3.00    3.00  1.29     0.81  0.06
#Relevant_Info           4 150   2.73   2.00   3.00    2.64   2.97  0.0    8.00    8.00  0.23    -0.93  0.16
#Global_Comprehension    5 150   4.07   2.58   3.50    3.91   2.97  0.0   11.00   11.00  0.44    -0.55  0.21
#Calibration_Bias        6 150   4.32   1.74   4.61    4.43   1.73 -0.5    7.94    8.44 -0.52    -0.16  0.14

psych::describeBy(mydataset[,c(2:6,10)],list(mydataset$Reading_Comprehension_levels,mydataset$Medium)) #statistiche descrittive suddivise per gruppo e per mezzo. 


#Control variables
psych::describeBy(mydataset[,c(14,15)], group= mydataset$Medium)
#                     vars   n  mean   sd median trimmed  mad  min  max range  skew kurtosis   se
#Reading_Comprehension    1 150 14.52 4.28  15.00   14.86 4.45 2.00 23.0 21.00 -0.71     0.16 0.35
#Prior_Knowledge          2 150  1.16 0.54   1.13    1.14 0.56 0.13  2.5  2.37  0.36    -0.52 0.04



#---- MODELS with MEDIUM_PREFERENCE_STUDY and PRIOR KNOWLEDGE
# RT ----

#single effects
RT <- lmer(Reading_Time ~ Medium + Gender +Prior_Knowledge + Reading_Comprehension_levels  +(1|CLASS) + (1|ID), data = mydataset, REML = T)
tab_model(RT)
#model plot
plot_model(RT,title = "Effects on reading time", show.intercept=T)
#single effets (With title)
plot_model(RT, type = "pred", terms =  "Medium", 
           title = "Effect of medium on reading time", axis.title= c("Medium","Reading time"),
           grid= F) + stat_summary(fun = "mean",geom = "line",color = "darkgrey" )

plot_model(RT, type = "pred", terms =  "Reading_Comprehension_levels", 
           title = "Effect of comprehension ability on reading time", axis.title= c("Reading comprehension ability","Reading time"),
           grid= F) + stat_summary(fun = "mean",geom = "line",color = "darkgrey" )

#single effets (no title)
plot_model(RT, type = "pred", terms =  "Medium", 
           title = "", axis.title= c("Medium","Reading time"),
           grid= F) + stat_summary(fun = "mean",geom = "line",color = "darkgrey" )

plot_model(RT, type = "pred", terms =  "Reading_Comprehension_levels", 
           title = "", axis.title= c("Reading comprehension ability","Reading time"),
           grid= F) + stat_summary(fun = "mean",geom = "line",color = "darkgrey" )

#interaction effect
RT_int<- lmer(Reading_Time ~ Medium+ Gender + Prior_Knowledge + Reading_Comprehension_levels  + Medium*Reading_Comprehension_levels + (1|CLASS) + (1|ID), data = mydataset, REML = T)
tab_model(RT_int)
plot_model(RT_int,title = "Effects on reading time",show.intercept=T )
plot_model(GC_int, type = "int",terms = "Reading_Comprehension_levels*Medium",
           title= "Predicted values of reading time",
           axis.title= c("Medium","Reading time"),
           grid= T)+ stat_summary(fun = "mean",geom = "line",color = "darkgrey" )
#plot(effects::effect(RT_int, term= "Medium*Reading_Comprehension_levels"))
effects::effect(RT_int, term= "Medium*Reading_Comprehension_levels")
AIC(RT, RT_int)
#       df      AIC
#RT      8 4033.145
#RT_int 10 4036.015



#GLOBAL COMPREHENSION

GC <- lmer(Global_Comprehension ~ Medium+ Gender + Prior_Knowledge + Reading_Comprehension_levels 
           +(1|CLASS)+ (1|ID), data = mydataset, REML = T)
tab_model(GC)

#Model plot
plot_model(GC,title = "Effects on Comprehension",show.intercept=T)

#single effets plot (With title)
plot_model(RT, type = "pred", terms =  "Medium", 
           title = "Effect of medium on comprehension", axis.title= c("Medium","Comprehension"),
           grid= F) + stat_summary(fun = "mean",geom = "line",color = "darkgrey" )

plot_model(RT, type = "pred", terms =  "Reading_Comprehension_levels", 
           title = "Effect of comprehension ability on comprehension", axis.title= c("Reading comprehension ability","Comprehension"),
           grid= F) + stat_summary(fun = "mean",geom = "line",color = "darkgrey" )

#single effets plot(no title)
plot_model(RT, type = "pred", terms =  "Medium", 
           title = "", axis.title= c("Medium","Comprehension"),
           grid= F) + stat_summary(fun = "mean",geom = "line",color = "darkgrey" )

plot_model(RT, type = "pred", terms =  "Reading_Comprehension_levels", 
           title = "", axis.title= c("Reading comprehension ability","Comprehension"),
           grid= F) + stat_summary(fun = "mean",geom = "line",color = "darkgrey" )

#interaction effect
GC_int<- lmer(Global_Comprehension ~ Medium+ Gender + Prior_Knowledge + Reading_Comprehension_levels + Medium*Reading_Comprehension_levels
              + (1|CLASS) + (1|ID), data = mydataset, REML = T)
tab_model(GC_int)
#Model plot
plot_model(GC_int,  title="Effects on comprehension",show.intercept=T)
#Interaction effect plot
plot_model(GC_int, type = "int",terms = "Reading_Comprehension_levels*Medium",
           title= "Predicted values of comprehension",
           axis.title= c("Medium","Comprehension"),
           grid= T)+ stat_summary(fun = "mean",geom = "line",color = "darkgrey" )

#plot(effects::effect(GC_int, term= "Medium*Reading_Comprehension_levels"))

AIC(GC, GC_int)
#        df      AIC
#GC      9 1310.491
#GC_int 11 1314.307


#TOT_CAL----


CAL <- lmer(Calibration_Bias ~ Medium+ Gender + Prior_Knowledge + Reading_Comprehension_levels 
            + (1|CLASS) + (1|ID), data = mydataset, REML =T)

tab_model(CAL)
plot(effects::effect(CAL, term= "Medium"))
plot_model(CAL,  title="Effects on calibration bias",show.intercept=T)
#single effets plot (With title)
plot_model(CAL, type = "pred", terms =  "Medium", 
           title = "Effect of medium on calibration bias", axis.title= c("Medium","Calibration bias"),
           grid= F) + stat_summary(fun = "mean",geom = "line",color = "darkgrey" )

plot_model(CAL, type = "pred", terms =  "Reading_Comprehension_levels", 
           title = "Effect of comprehension ability on calibration bias", axis.title= c("Reading comprehension ability","Calibration bias"),
           grid= F) + stat_summary(fun = "mean",geom = "line",color = "darkgrey" )

#single effets plot(no title)
plot_model(CAL, type = "pred", terms =  "Medium", 
           title = "", axis.title= c("Medium","Calibration bias"),
           grid= F) + stat_summary(fun = "mean",geom = "line",color = "darkgrey" )

plot_model(CAL, type = "pred", terms =  "Reading_Comprehension_levels", 
           title = "", axis.title= c("Reading comprehension ability","Calibration bias"),
           grid= F) + stat_summary(fun = "mean",geom = "line",color = "darkgrey" )

#interaction effect
CAL_int <- lmer(Calibration_Bias ~ Medium + Gender+ Prior_Knowledge  + Reading_Comprehension_levels + Medium*Reading_Comprehension_levels 
                + (1|CLASS) + (1|ID), data = mydataset, REML = T)# + (1|CLASS)
tab_model(CAL_int)
#plot(effects::effect(CAL_int, term= "Medium*Reading_Comprehension_levels"))
plot_model(CAL_int,  title="Effects on calibration bias",show.intercept=T)
plot_model(CAL_int, type = "int",terms = "Reading_Comprehension_levels*Medium",
           title= "",
           axis.title= c("Medium","Calibration bias"),
           grid= T)+ stat_summary(fun = "mean",geom = "line",color = "darkgrey" )

plot(effects::effect(CAL_int, term= "Medium*Reading_Comprehension_levels"))

AIC(CAL, CAL_int)
#        df      AIC
#CAL      8 1147.382
#CAL_int 10 1150.670
