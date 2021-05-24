# Clear Memory and Import Libraries

rm(list=ls())
#library(tidyverse)
library(zoo)
library(xts)
library(lubridate)

# Import Dataset for Low Income (LICO and LIM), 

lowinc_df <- read.table(file="lowincome.csv", header=FALSE, sep=',', skip=9, nrows=20)
colnames(lowinc_df) <- c("Year",
                         # Canada 
                         "cdn.all.lim","cdn.all.lico.aftertax","cdn.all.lico.beforetax",
                         "cdn.male.lim","cdn.male.lico.aftertax","cdn.male.lico.beforetax",
                         "cdn.fem.lim","cdn.fem.lico.aftertax","cdn.fem.lico.beforetax",
                         "cdn.fam.lim","cdn.fam.lico.aftertax","cdn.fam.lico.beforetax",
                         "cdn.single.lim","cdn.single.lico.aftertax","cdn.single.lico.beforetax",
                         # Quebec
                         "qbc.all.lim","qbc.all.lico.aftertax","qbc.all.lico.beforetax",
                         "qbc.male.lim","qbc.male.lico.aftertax","qbc.male.lico.beforetax",
                         "qbc.fem.lim","qbc.fem.lico.aftertax","qbc.fem.lico.beforetax",
                         "qbc.fam.lim","qbc.fam.lico.aftertax","qbc.fam.lico.beforetax",
                         "qbc.single.lim","qbc.single.lico.aftertax","qbc.single.lico.beforetax",
                         # Ontario 
                         "ont.all.lim","ont.all.lico.aftertax","ont.all.lico.beforetax",
                         "ont.male.lim","ont.male.lico.aftertax","ont.male.lico.beforetax",
                         "ont.fem.lim","ont.fem.lico.aftertax","ont.fem.lico.beforetax",
                         "ont.fam.lim","ont.fam.lico.aftertax","ont.fam.lico.beforetax",
                         "ont.single.lim","ont.single.lico.aftertax","ont.single.lico.beforetax",
                         # British Columbia
                         "bc.all.lim","bc.all.lico.aftertax","bc.all.lico.beforetax",
                         "bc.male.lim","bc.male.lico.aftertax","bc.male.lico.beforetax",
                         "bc.fem.lim","bc.fem.lico.aftertax","bc.fem.lico.beforetax",
                         "bc.fam.lim","bc.fam.lico.aftertax","bc.fam.lico.beforetax",
                         "bc.single.lim","bc.single.lico.aftertax","bc.single.lico.beforetax")


# Import Dataset for Labour Force (Employment Rate), 

lf_df <- read.table(file="labourforce.csv", header=FALSE, sep=',', skip=11, nrows=21)
colnames(lf_df) <- c("Year",
                     #Canada
                     "cdn.lf.15andover","cdn.lf.15to24","cdn.lf.25to54","cdn.lf.55andover",
                     "cdn.employ.15andover","cdn.employ.15to24", "cdn.employ.25to54", "cdn.employ.55andover",
                     "cdn.ftemploy.15andover","cdn.ftemploy.15to24", "cdn.ftemploy.25to54", "cdn.ftemploy.55andover",
                     "cdn.ptemploy.15andover", "cdn.ptemploy.15to24", "cdn.ptemploy.25to54", "cdn.ptemploy.55andover",
                     "cdn.unemploy.15andover", "cdn.unemploy.15to24","cdn.umemploy.25to54","cdn.unemploy55andover",
                     # Quebec 
                     "qbc.lf.15andover","qbc.lf.15to24","qbc.lf.25to54","qbc.lf.55andover",
                     "qbc.employ.15andover","qbc.employ.15to24", "qbc.employ.25to54", "qbc.employ.55andover",
                     "qbc.ftemploy.15andover","qbc.ftemploy.15to24", "qbc.ftemploy.25to54", "qbc.ftemploy.55andover",
                     "qbc.ptemploy.15andover", "qbc.ptemploy.15to24", "qbc.ptemploy.25to54", "qbc.ptemploy.55andover",
                     "qbc.unemploy.15andover", "qbc.unemploy.15to24","qbc.umemploy.25to54","qbc.unemploy55andover",
                     # Ontario 
                     "ont.lf.15andover","ont.lf.15to24","ont.lf.25to54","ont.lf.55andover",
                     "ont.employ.15andover","ont.employ.15to24", "ont.employ.25to54", "ont.employ.55andover",
                     "ont.ftemploy.15andover","ont.ftemploy.15to24", "ont.ftemploy.25to54", "ont.ftemploy.55andover",
                     "ont.ptemploy.15andover", "ont.ptemploy.15to24", "ont.ptemploy.25to54", "ont.ptemploy.55andover",
                     "ont.unemploy.15andover", "ont.unemploy.15to24","ont.umemploy.25to54","ont.unemploy55andover",
                     # British Columbia
                     "bc.lf.15andover","bc.lf.15to24","bc.lf.25to54","bc.lf.55andover",
                     "bc.employ.15andover","bc.employ.15to24", "bc.employ.25to54", "bc.employ.55andover",
                     "bc.ftemploy.15andover","bc.ftemploy.15to24", "bc.ftemploy.25to54", "bc.ftemploy.55andover",
                     "bc.ptemploy.15andover", "bc.ptemploy.15to24", "bc.ptemploy.25to54", "bc.ptemploy.55andover",
                     "bc.unemploy.15andover", "bc.unemploy.15to24","bc.umemploy.25to54","bc.unemploy55andover")   

# Import Data Set for Provincial Minimum Wages

minwage <- read.table(file="minimumwage.csv", header=TRUE, sep=',')
minwage_df <- minwage[1:3]

#months <- substr(minwage$Effective.Date,start=4,stop=6)
#date <- match(months, month.abb)
#dates <- as.Date(date,"%d-%m-%y")

minwage_df = minwage[,c(2,1,3)]

qbc_mw <- minwage_df[87:105,]
dmy(qbc_mw$EffectiveDate)

bc_mw <- minwage_df[223:234,]
dmy(bc_mw$EffectiveDate)

ont_mw <- minwage_df[470:481,]
dmy(ont_mw$EffectiveDate)



# Exploratory Data Analysis for Both Genders 

df_both = df[,0:23]
ref_period= unlist(df_both['Reference period'])
y_both = unlist(df_both['15 years and over'])

df_both %>%
  ggplot(aes(x=ref_period, y=y_both)+geom_line())

# Exploratory Data Analysis for Male Only

df_male = cbind(df[,0],df[,24:45])

df_male %>%
  ggplot(aes(ref_period))
#x_male = 
#y_male =

# Exploratory Data Analysis for Female Only

df_female = cbind(df[,0],df[,46:67])

df_female %>% 
  ggplot(aes())
#x_female = 
#y_female = 
