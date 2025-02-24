# Clear Memory and Import Libraries

rm(list=ls())

library(stargazer)
library(tidyverse)
library(ggplot2)
library(lmtest)
library(plm)

# Import Data Set for Low Income (LICO and LIM), 

lowinc_df <- read.table(file="lowincome1976-2019.csv", header=FALSE, sep=',', skip=10, nrows=44)
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

# Import Data Set for Labour Force (Employment Rate), 

lf_df <- read.table(file="labourforce1976-2019.csv", header=FALSE, sep=',', skip=12, nrows=45)
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

# Convert Columns' Data Type from Factor to Numeric/Double

lf_df$Year <- as.double(sub(',','',lf_df$Year))
lf_df$cdn.lf.15andover <- as.double(sub(',', '',lf_df$cdn.lf.15andover))
lf_df$cdn.lf.15to24 <- as.double(sub(',', '',lf_df$cdn.lf.15to24))
lf_df$cdn.lf.25to54 <- as.double(sub(',', '',lf_df$cdn.lf.25to54))
lf_df$cdn.lf.55andover <- as.double(sub(',', '',lf_df$cdn.lf.55andover))
lf_df$cdn.employ.15andover <- as.double(sub(',', '',lf_df$cdn.employ.15andover))
lf_df$cdn.employ.15to24 <- as.double(sub(',', '',lf_df$cdn.employ.15to24))
lf_df$cdn.employ.25to54 <- as.double(sub(',', '',lf_df$cdn.employ.25to54))
lf_df$cdn.employ.55andover <- as.double(sub(',', '',lf_df$cdn.employ.55andover))
lf_df$cdn.ftemploy.15andover <- as.double(sub(',', '',lf_df$cdn.ftemploy.15andover))
lf_df$cdn.ftemploy.15to24 <- as.double(sub(',', '',lf_df$cdn.ftemploy.15to24))
lf_df$cdn.ftemploy.25to54 <- as.double(sub(',', '',lf_df$cdn.ftemploy.25to54))
lf_df$cdn.ftemploy.55andover <- as.double(sub(',', '',lf_df$cdn.ftemploy.55andover))

lf_df$cdn.ptemploy.15andover <- as.double(sub(',', '',lf_df$cdn.ptemploy.15andover))
lf_df$cdn.ptemploy.15to24 <- as.double(sub(',', '',lf_df$cdn.ptemploy.15to24))
lf_df$cdn.ptemploy.25to54 <- as.double(sub(',', '',lf_df$cdn.ptemploy.25to54))
lf_df$cdn.ptemploy.55andover <- as.double(sub(',', '',lf_df$cdn.ptemploy.55andover))

lf_df$qbc.lf.15andover <- as.double(sub(',', '',lf_df$qbc.lf.15andover))
lf_df$qbc.lf.25to54 <- as.double(sub(',', '',lf_df$qbc.lf.25to54))
lf_df$qbc.employ.15andover <- as.double(sub(',', '',lf_df$qbc.employ.15andover))
lf_df$qbc.employ.25to54 <- as.double(sub(',', '',lf_df$qbc.employ.25to54))
lf_df$qbc.ftemploy.15andover <- as.double(sub(',', '',lf_df$qbc.ftemploy.15andover))
lf_df$qbc.ftemploy.25to54 <- as.double(sub(',', '',lf_df$qbc.ftemploy.25to54))

lf_df$ont.lf.15andover <- as.double(sub(',', '',lf_df$ont.lf.15andover))
lf_df$ont.lf.15to24 <- as.double(sub(',', '',lf_df$ont.lf.15to24))
lf_df$ont.lf.25to54 <- as.double(sub(',', '',lf_df$ont.lf.25to54))
lf_df$ont.lf.55andover <- as.double(sub(',', '',lf_df$ont.lf.55andover))
lf_df$ont.employ.15andover <- as.double(sub(',', '',lf_df$ont.employ.15andover))
lf_df$ont.employ.15to24 <- as.double(sub(',', '',lf_df$ont.employ.15to24))
lf_df$ont.employ.25to54 <- as.double(sub(',', '',lf_df$ont.employ.25to54))
lf_df$ont.employ.55andover <- as.double(sub(',', '',lf_df$ont.employ.55andover))
lf_df$ont.ftemploy.15andover <- as.double(sub(',', '',lf_df$ont.ftemploy.15andover))
lf_df$ont.ftemploy.25to54 <- as.double(sub(',', '',lf_df$ont.ftemploy.25to54))
lf_df$ont.ftemploy.55andover <- as.double(sub(',', '',lf_df$ont.ftemploy.55andover))
lf_df$ont.ptemploy.15andover <- as.double(sub(',', '',lf_df$ont.ptemploy.15andover))

lf_df$bc.lf.15andover <- as.double(sub(',', '',lf_df$bc.lf.15andover))
lf_df$bc.lf.25to54 <- as.double(sub(',', '',lf_df$bc.lf.25to54))
lf_df$bc.employ.15andover <- as.double(sub(',', '',lf_df$bc.employ.15andover))
lf_df$bc.employ.25to54 <- as.double(sub(',', '',lf_df$bc.employ.25to54))
lf_df$bc.ftemploy.15andover <- as.double(sub(',', '',lf_df$bc.ftemploy.15andover))
lf_df$bc.ftemploy.25to54 <- as.double(sub(',', '',lf_df$bc.ftemploy.25to54))

# Import Data Set for Provincial Minimum Wages

minwage <- read.table(file="minimumwage.csv", header=TRUE, sep=',')
minwage_df <- minwage[1:3]
minwage_df <- minwage[,c(2,1,3)]

# Change Data Types as for Effective Date & Minimum Wage

minwage_df$Effective.Date <- as.Date(minwage_df$Effective.Date,format="%d-%b-%y")
minwage_df$Minimum.Wage <- as.double(sub('.', '', minwage_df$Minimum.Wage))

# Split Data Set into Ontario, Quebec and British Columbia

qbc_mw <- minwage_df[87:128,]
bc_mw <- minwage_df[223:245,]
ont_mw <- minwage_df[470:498,]
qbc_bc_ont_mw <- rbind(qbc_mw,bc_mw,ont_mw)

# Plot the Historical Minimum Wage Levels of Each Province

ggplot(qbc_bc_ont_mw,aes(Effective.Date,Minimum.Wage,colour=Jurisdiction,linetype=Jurisdiction))+
  geom_step(aes(Effective.Date,Minimum.Wage),size=1.2)+
  labs(x="Year",y="Minimum Wage (Canadian $)",title="Figure 1: Minimum Wage by Province")+
  theme_minimal()+  
  theme(text=element_text(size=20),plot.title = element_text(hjust = 0.5),legend.position="right")

# Plot Labour Force in Canada by Province for all ages

ggplot((lf_df),colour=c("Quebec"="blue", "Ontario"="green","British Columbia"="red"),linetype=c("Quebec"=1, "Ontario"=3,"British Columbia"=5))+ 
  geom_line(aes(Year, qbc.lf.15andover, colour="Quebec",linetype="Quebec"),size=1.2)+
  geom_line(aes(Year, ont.lf.15andover, colour="Ontario",linetype="Ontario"),size=1.2)+
  geom_line(aes(Year, bc.lf.15andover, colour="British Columbia",linetype="British Columbia"),size=1.2)+
  labs(x="Year",y="Labour Force (in thousands)",title="Figure 2: Labour Force (15 Years and Over)",linetype="Province",colour="Province")+
  theme_minimal()+
  theme(text=element_text(size=12),plot.title=element_text(hjust=0.5), legend.position="right")

# Plot LIM and LICO in Canada by Province for Individuals 15 and over 

ggplot((lowinc_df),colour=c("Quebec"="blue", "Ontario"="green","British Columbia"="red"),linetype=c("Quebec"=1, "Ontario"=3,"British Columbia"=5))+ 
  geom_line(aes(Year, qbc.all.lim, colour="Quebec",linetype="Quebec"),size=1.2)+
  geom_line(aes(Year, ont.all.lim, colour="Ontario",linetype="Ontario"),size=1.2)+
  geom_line(aes(Year, bc.all.lim, colour="British Columbia",linetype="British Columbia"),size=1.2)+
  labs(x="Year",y="Low Income (%)",title="Figure 3: After-Tax LIM (15 Years and Over)",linetype="Province",colour="Province")+
  theme_minimal()+
  theme(text=element_text(size=12),plot.title=element_text(hjust=0.5), legend.position="right")

ggplot((lowinc_df),colour=c("Quebec"="blue", "Ontario"="green","British Columbia"="red"),linetype=c("Quebec"=1, "Ontario"=3,"British Columbia"=5))+ 
  geom_line(aes(Year, qbc.all.lico.aftertax, colour="Quebec",linetype="Quebec"),size=1.2)+
  geom_line(aes(Year, ont.all.lico.aftertax, colour="Ontario",linetype="Ontario"),size=1.2)+
  geom_line(aes(Year, bc.all.lico.aftertax, colour="British Columbia",linetype="British Columbia"),size=1.2)+
  labs(x="Year",y="Low Income (%)",title="Figure 4: After-Tax LICO (15 Years and Over)",linetype="Province",colour="Province")+
  theme_minimal()+
  theme(text=element_text(size=12),plot.title=element_text(hjust=0.5), legend.position="right")


# Combine the Labor Force and Low Income Data Sets

data <-as.data.frame(c(lowinc_df, lf_df[-nrow(lf_df),]))
data$Year <- as.double(sub(',','',data$Year))

# Create Columns with the Annual Minimum Wage Breakdown in each Province
# Transition Years are weighted averages based on the effective date of the minimum wage increase
# e.g. Increase from 6.85 to 7.15 in February 1 2004 = ((1/12)*6.85) + ((11/12)*7.15) = 7.125

min_wage_avgfct <- function(prov_mw){
  years = as.character(rev(seq(from = 1976,to = 2019,by = 1)))
  loop = rep(0,length(years))
  count = 0
  mwyears = as.numeric(substr(prov_mw[,1],1,4))
  for(year in years){
    count = count + 1
    curr_mw = as.numeric(prov_mw[match(year,mwyears),3])
    prev_mw = as.numeric(prov_mw[match(year,mwyears)+1,3]) 
    future_mw = loop[count-1] # since the list we are looping through is in reverse order 
    month = as.numeric(substr(prov_mw[match(year,mwyears),1],6,7))
    # for the first iteration of the loop only (i.e. 2019)
    if(count == 1){loop[count] = prov_mw[count,3]}
    # populate values that are in the data set and start at the beginning of the year (i.e. January 1st) 
    else if(!is.na(match(year,mwyears)) && month == 1){loop[count] = prov_mw[match(year,mwyears),3]}
    # populate values that are in the inputted data set
    else if(!is.na(match(year,mwyears))){loop[count] = curr_mw * ((12-month)/12) + prev_mw * (month/12)}
    # populate values that are not in the inputted data set
    else if(is.na(match(year,mwyears))){loop[count] = future_mw} 
  }
return(loop)
}

# Turn Averages into data frames and add Jurisdiction Column 

ont_mw_avg = as.data.frame(min_wage_avgfct(ont_mw))
ont_mw_avg$Jurisdiction = c(rep('Ontario',nrow(ont_mw_avg)))
ont_mw_avg = ont_mw_avg[,c(2,1)]
colnames(ont_mw_avg) = c("Jurisdiction", "ont_mw")

qbc_mw_avg = as.data.frame(min_wage_avgfct(qbc_mw))
qbc_mw_avg$Jurisdiction = c(rep('Quebec', nrow(qbc_mw_avg)))
qbc_mw_avg =qbc_mw_avg[,c(2,1)]
colnames(qbc_mw_avg) = c("Jurisdiction", "qbc_mw")

bc_mw_avg = as.data.frame(min_wage_avgfct(bc_mw))
bc_mw_avg$Jurisdiction = c(rep('British Columbia',nrow(bc_mw_avg)))
bc_mw_avg = bc_mw_avg[,c(2,1)]
colnames(bc_mw_avg) = c("Jurisdiction", "bc_mw")


# Add New Columns to Data, Data Frame

alldata<-cbind(data,ont_mw_avg,qbc_mw_avg,bc_mw_avg)
attach(alldata)

# Create Panel for Quebec 

quebec = select(alldata,17:31,83:102)
colnames(quebec) = c("all.lim","all.lico.aftertax","all.lico.beforetax",
                     "male.lim","male.lico.aftertax","male.lico.beforetax",
                     "fem.lim","fem.lico.aftertax","fem.lico.beforetax",
                     "fam.lim","fam.lico.aftertax","fam.lico.beforetax",
                     "single.lim","single.lico.aftertax","single.lico.beforetax",
                     "lf.15andover","lf.15to24","lf.25to54","lf.55andover",
                     "employ.15andover","employ.15to24", "employ.25to54", "employ.55andover",
                     "ftemploy.15andover","ftemploy.15to24", "ftemploy.25to54", "ftemploy.55andover",
                     "ptemploy.15andover", "ptemploy.15to24", "ptemploy.25to54", "ptemploy.55andover",
                     "unemploy.15andover", "unemploy.15to24","umemploy.25to54","unemploy55andover")
quebec$year = seq(from=1976,to=2019,by=1)
quebec$jurisdiction = rep("QC",each=44)
quebec$minwage = rev(qbc_mw_avg$qbc_mw)
quebec = quebec[,c(36,37,38,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
                   21,22,23,24,25,26,27,28,29,30,31,32,33,34,35)]

# Create Panel for Ontario

ontario = select(alldata,32:46,103:122)
colnames(ontario) = c("all.lim","all.lico.aftertax","all.lico.beforetax",
                     "male.lim","male.lico.aftertax","male.lico.beforetax",
                     "fem.lim","fem.lico.aftertax","fem.lico.beforetax",
                     "fam.lim","fam.lico.aftertax","fam.lico.beforetax",
                     "single.lim","single.lico.aftertax","single.lico.beforetax",
                     "lf.15andover","lf.15to24","lf.25to54","lf.55andover",
                     "employ.15andover","employ.15to24", "employ.25to54", "employ.55andover",
                     "ftemploy.15andover","ftemploy.15to24", "ftemploy.25to54", "ftemploy.55andover",
                     "ptemploy.15andover", "ptemploy.15to24", "ptemploy.25to54", "ptemploy.55andover",
                     "unemploy.15andover", "unemploy.15to24","umemploy.25to54","unemploy55andover")
ontario$year = seq(from=1976,to=2019,by=1)
ontario$jurisdiction = rep("ON",each=44)
ontario$minwage = rev(ont_mw_avg$ont_mw)
ontario = ontario[,c(36,37,38,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
                     21,22,23,24,25,26,27,28,29,30,31,32,33,34,35)]

# Create Panel for British Columbia 

britishcolumbia = select(alldata,47:61,123:142)
colnames(britishcolumbia) = c("all.lim","all.lico.aftertax","all.lico.beforetax",
                              "male.lim","male.lico.aftertax","male.lico.beforetax",
                              "fem.lim","fem.lico.aftertax","fem.lico.beforetax",
                              "fam.lim","fam.lico.aftertax","fam.lico.beforetax",
                              "single.lim","single.lico.aftertax","single.lico.beforetax",                     
                              "lf.15andover","lf.15to24","lf.25to54","lf.55andover",
                              "employ.15andover","employ.15to24", "employ.25to54", "employ.55andover",
                              "ftemploy.15andover","ftemploy.15to24", "ftemploy.25to54", "ftemploy.55andover",
                              "ptemploy.15andover", "ptemploy.15to24", "ptemploy.25to54", "ptemploy.55andover",
                              "unemploy.15andover", "unemploy.15to24","umemploy.25to54","unemploy55andover")
britishcolumbia$year = seq(from=1976,to=2019,by=1)
britishcolumbia$jurisdiction = rep("BC",each=44)
britishcolumbia$minwage = rev(bc_mw_avg$bc_mw)
britishcolumbia = britishcolumbia[,c(36,37,38,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
                                     21,22,23,24,25,26,27,28,29,30,31,32,33,34,35)]

detach(alldata)

# Combine the three panels into a Panel Data Set 

panel = bind_rows(quebec, ontario, britishcolumbia)
panel = pdata.frame(panel,index=c("jurisdiction", "year"))


attach(panel)

# Run fixed effects model 

ctrl_vars = cbind(lf.15andover, unemploy.15andover)


# Fixed Effects Model with All LIM (after-tax)

panel_fe_all_lim_lm = lm(all.lim ~ minwage + ctrl_vars + jurisdiction + year - 1, data=panel)
panel_fe_all_lim_lm_se = coeftest(panel_fe_all_lim_lm, vcov.= vcovHC, type = "HC1")

panel_fe_all_lim_plm = plm(all.lim ~ minwage + ctrl_vars + jurisdiction + year - 1, data = panel, index = c("jurisdiction", "year"), model = "within", effect="twoways")
panel_fe_all_lim_plm_se = coeftest(panel_fe_all_lim_plm, vcov.= vcovHC, type = "HC1")


# Fixed Effects Model with All LICO (after-tax)

panel_fe_all_licoat_lm = lm(all.lico.aftertax ~ minwage + ctrl_vars + jurisdiction + year - 1, data=panel)
panel_fe_all_licoat_lm_se = coeftest(panel_fe_all_licoat_lm, vcov.= vcovHC, type = "HC1")

panel_fe_all_licoat_plm = plm(all.lico.aftertax ~ minwage + ctrl_vars + jurisdiction + year - 1, data = panel, index = c("jurisdiction", "year"), model = "within", effect="twoways")
panel_fe_all_licoat_plm_se = coeftest(panel_fe_all_licoat_plm, vcov.= vcovHC, type = "HC1")


# Fixed Effects Model with Male LIM (after-tax)

panel_fe_male_lim_lm = lm(male.lim ~ minwage + ctrl_vars + jurisdiction + year - 1, data=panel)
panel_fe_male_lim_lm_se = coeftest(panel_fe_male_lim_lm, vcov.= vcovHC, type = "HC1")

panel_fe_male_lim_plm = plm(male.lim ~ minwage + ctrl_vars + jurisdiction + year - 1, data = panel, index = c("jurisdiction", "year"), model = "within", effect="twoways")
panel_fe_male_lim_plm_se = coeftest(panel_fe_male_lim_plm, vcov.= vcovHC, type = "HC1")

# Fixed Effects Model with Male LICO (after-tax)

panel_fe_male_licoat_lm = lm(male.lico.aftertax ~ minwage + ctrl_vars + jurisdiction + year - 1, data=panel)
panel_fe_male_licoat_lm_se = coeftest(panel_fe_male_licoat_lm, vcov.= vcovHC, type = "HC1")

panel_fe_male_licoat_plm = plm(male.lico.aftertax ~ minwage + ctrl_vars + jurisdiction + year - 1, data = panel, index = c("jurisdiction", "year"), model = "within", effect="twoways")
panel_fe_male_licoat_plm_se = coeftest(panel_fe_male_licoat_plm, vcov.= vcovHC, type = "HC1")


# Fixed Effects Model with Female LIM (after-tax)

panel_fe_fem_lim_lm = lm(fem.lim ~ minwage + ctrl_vars + jurisdiction + year - 1, data=panel)
panel_fe_fem_lim_lm_se = coeftest(panel_fe_fem_lim_lm, vcov.= vcovHC, type = "HC1")

panel_fe_fem_lim_plm = plm(fem.lim ~ minwage + ctrl_vars + jurisdiction + year - 1, data = panel, index = c("jurisdiction", "year"), model = "within", effect="twoways")
panel_fe_fem_lim_plm_se = coeftest(panel_fe_fem_lim_plm, vcov.= vcovHC, type = "HC1")

# Fixed Effects Model with Female LICO (after-tax)

panel_fe_fem_licoat_lm = lm(fem.lico.aftertax ~ minwage + ctrl_vars + jurisdiction + year - 1, data=panel)
panel_fe_fem_licoat_lm_se = coeftest(panel_fe_fem_licoat_lm, vcov.= vcovHC, type = "HC1")

panel_fe_fem_licoat_plm = plm(fem.lico.aftertax ~ minwage + ctrl_vars + jurisdiction + year - 1, data = panel, index = c("jurisdiction", "year"), model = "within", effect="twoways")
panel_fe_fem_licoat_plm_se = coeftest(panel_fe_fem_licoat_plm, vcov.= vcovHC, type = "HC1")


# Fixed Effects Model with Family LIM (after-tax)

panel_fe_fam_lim_lm = lm(fam.lim ~ minwage + ctrl_vars + jurisdiction + year - 1, data=panel)
panel_fe_fam_lim_lm_se = coeftest(panel_fe_fam_lim_lm, vcov.= vcovHC, type = "HC1")

panel_fe_fam_lim_plm = plm(fam.lim ~ minwage + ctrl_vars + jurisdiction + year - 1, data = panel, index = c("jurisdiction", "year"), model = "within", effect="twoways")
panel_fe_fam_lim_plm_se = coeftest(panel_fe_fam_lim_plm, vcov.= vcovHC, type = "HC1")

# Fixed Effects Model with Family LICO (after-tax)

panel_fe_fam_licoat_lm = lm(fam.lico.aftertax ~ minwage + ctrl_vars + jurisdiction + year - 1, data=panel)
panel_fe_fam_licoat_lm_se = coeftest(panel_fe_fam_licoat_lm, vcov.= vcovHC, type = "HC1")

panel_fe_fam_licoat_plm = plm(fam.lico.aftertax ~ minwage + ctrl_vars + jurisdiction + year - 1, data = panel, index = c("jurisdiction", "year"), model = "within", effect="twoways")
panel_fe_fam_licoat_plm_se = coeftest(panel_fe_fam_licoat_plm, vcov.= vcovHC, type = "HC1")


# Fixed Effects Model with Single LIM (after-tax)

panel_fe_single_lim_lm = lm(single.lim ~ minwage + ctrl_vars + jurisdiction + year - 1, data=panel)
panel_fe_single_lim_lm_se = coeftest(panel_fe_single_lim_lm, vcov.= vcovHC, type = "HC1")

panel_fe_single_lim_plm = plm(single.lim ~ minwage + ctrl_vars + jurisdiction + year - 1, data = panel, index = c("jurisdiction", "year"), model = "within", effect="twoways")
panel_fe_single_lim_plm_se = coeftest(panel_fe_single_lim_plm, vcov.= vcovHC, type = "HC1")

# Fixed Effects Model with Single LICO (after-tax)

panel_fe_single_licoat_lm = lm(single.lico.aftertax ~ minwage + ctrl_vars + jurisdiction + year - 1, data=panel)
panel_fe_single_licoat_lm_se = coeftest(panel_fe_single_licoat_lm, vcov.= vcovHC, type = "HC1")

panel_fe_single_licoat_plm = plm(all.lim ~ minwage + ctrl_vars + jurisdiction + year - 1, data = panel, index = c("jurisdiction", "year"), model = "within", effect="twoways")
panel_fe_single_licoat_plm_se = coeftest(panel_fe_single_licoat_plm, vcov.= vcovHC, type = "HC1")


# CREATE A TABLE OF WITH THE LIM RESULTS OF THE PLM() FOR EACH POVERTY RATE 

lim_standard_errs <- list(sqrt(diag(vcovHC(panel_fe_all_lim_plm, type="HC1"))),
                      sqrt(diag(vcovHC(panel_fe_male_lim_plm, type="HC1"))),
                      sqrt(diag(vcovHC(panel_fe_fem_lim_plm, type="HC1"))),
                      sqrt(diag(vcovHC(panel_fe_fam_lim_plm, type="HC1"))),
                      sqrt(diag(vcovHC(panel_fe_single_lim_plm, type="HC1"))))
                      
lim_results <- stargazer(panel_fe_all_lim_plm,
                         panel_fe_male_lim_plm, 
                         panel_fe_fem_lim_plm, 
                         panel_fe_fam_lim_plm, 
                         panel_fe_single_lim_plm,
                         digits = 3,
                         header = FALSE,
                         type = "text",
                         se = lim_standard_errs,
                         title = "LIM Panel Regression Models Results",
                         model.numbers = FALSE,
                         column.labels = c("All", "Male", "Female", "Family", "Single"))


# CREATE A TABLE OF WITH THE LICO RESULTS OF THE PLM() FOR EACH POVERTY RATE 

lico_standard_errs <- list(sqrt(diag(vcovHC(panel_fe_all_licoat_plm, type="HC1"))),
                      sqrt(diag(vcovHC(panel_fe_male_licoat_plm, type="HC1"))),
                      sqrt(diag(vcovHC(panel_fe_fem_licoat_plm, type="HC1"))),
                      sqrt(diag(vcovHC(panel_fe_fam_licoat_plm, type="HC1"))),
                      sqrt(diag(vcovHC(panel_fe_single_licoat_plm, type="HC1"))))


lico_results <- stargazer(panel_fe_all_licoat_plm,
                          panel_fe_male_licoat_plm,
                          panel_fe_fem_licoat_plm,
                          panel_fe_fam_licoat_plm,
                          panel_fe_single_licoat_plm,
                          digits = 3,
                          header = FALSE,
                          type = "text",
                          se = lico_standard_errs,
                          title = "LICO Panel Regression Models Results",
                          model.numbers = FALSE,
                          column.labels = c("All", "Male", "Female", "Family", "Single"))

                      