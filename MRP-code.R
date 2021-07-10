# Clear Memory and Import Libraries

rm(list=ls())

library(ggplot2)
library(sandwich)

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

ggplot(qbc_bc_ont_mw,aes(Effective.Date,Minimum.Wage,color=factor(Jurisdiction)))+
  geom_step(aes(Effective.Date,Minimum.Wage))+
  labs(x="Year",y="Minimum Wage (Canadian Dollars)",title="Minimum Wage by Province",colour="Provinces")+
  theme_minimal()+  
  theme(plot.title = element_text(hjust = 0.5), legend.position="right")

# Plot Labour Force in Canada by Province

ggplot(lf_df)+ 
  geom_line(aes(Year, qbc.lf.15andover, colour="Quebec"))+
  geom_line(aes(Year, ont.lf.15andover, colour="Ontario"))+
  geom_line(aes(Year, bc.lf.15andover, colour="British Columbia"))+
  labs(x="Year",y="Labour Force",title="Labour Force (15 Years and Over)",colour="Provinces")+
  theme_minimal()+
  theme(plot.title=element_text(hjust=0.5), legend.position="right")

# Plot LIM and LICO in Canada by Province for Individuals 15 and over 

ggplot(lowinc_df)+
  geom_line(aes(Year, qbc.all.lim, colour="Quebec"))+
  geom_line(aes(Year, ont.all.lim, colour="Ontario"))+
  geom_line(aes(Year, bc.all.lim, colour="British Columbia"))+
  labs(x="Year",y="Low Income (%)",title="LIM (15 Years and Over)",colour="Provinces")+
  theme_minimal()+
  theme(plot.title=element_text(hjust=0.5), legend.position="right")

ggplot(lowinc_df)+
  geom_line(aes(Year, qbc.all.lico.beforetax, colour="Quebec"))+
  geom_line(aes(Year, ont.all.lico.beforetax, colour="Ontario"))+
  geom_line(aes(Year, bc.all.lico.beforetax, colour="British Columbia"))+
  labs(x="Year",y="Low Income (%)",title="LICO (15 Years and Over)",colour="Provinces")+
  theme_minimal()+
  theme(plot.title=element_text(hjust=0.5), legend.position="right")                                                                                                                                             


# Combine the Labour Force and Low Income Data Sets

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
    print(month)
    print(year)
    print(loop)
    # for the first iteration of the loop only 
    if(count == 1){loop[count] = prov_mw[count,3]}
    # populate values that in the data set and start at the beginning of the year i.e. January 1st 
    else if(!is.na(match(year,mwyears)) && month == 1){loop[count] = prov_mw[count,3]}
    # populate values that are in the inputted data set
    else if(!is.na(match(year,mwyears))){loop[count] = curr_mw * ((12-month)/12) + prev_mw * (month/12)}
    # populate values that are not in the inputted data set
    else if(is.na(match(year,mwyears))){loop[count] = future_mw}
  }
return(loop)
}
ont.mw.avg = min_wage_avgfct(ont_mw)
qbc.mw.avg = min_wage_avgfct(qbc_mw)
bc.mw.avg = min_wage_avgfct(bc_mw)


# Add New Columns to Data, Data Frame

alldata<-cbind(data,ont.mw.avg,qbc.mw.avg,bc.mw.avg)
attach(alldata)

#    ------   LINEAR MODELS USING THE lm() FUNCTION  ------   #

# Ontario Model <- POVRATE = alpha(MINWAGE) + beta(CONTROL VARS) + Residuals

beta <- cbind(1, Year, ont.lf.15andover, ont.lf.15to24, ont.lf.25to54, ont.lf.55andover,
              ont.employ.15andover,ont.employ.15to24, ont.employ.25to54, ont.employ.55andover,
              ont.ftemploy.15andover,ont.ftemploy.15to24, ont.ftemploy.25to54, ont.ftemploy.55andover,
              ont.ptemploy.15andover, ont.ptemploy.15to24, ont.ptemploy.25to54, ont.ptemploy.55andover,
              ont.unemploy.15andover, ont.unemploy.15to24, ont.umemploy.25to54, ont.unemploy55andover)

# All Ontario

ont_lim_all_lin_reg = lm(ont.all.lim ~ 0 + ont.mw.avg + beta)                 # Low Income Measure
ont_licobt_all_lin_reg = lm(ont.all.lico.beforetax ~ 0 + ont.mw.avg + beta)   # Low Income Cut Off Before Tax
ont_licoat_all_lin_reg = lm(ont.all.lico.aftertax ~ 0 + ont.mw.avg + beta)    # Low Income Cut Off After Tax

alphas = coef(ont_lim_all_lin_reg)[2]

# Males in Poverty in Ontario

ont_lim_male_lin_reg = lm(ont.male.lim ~ 0 + ont.mw.avg + beta) 
ont_licobt_male_lin_reg = lm(ont.male.lico.beforetax ~ 0 + ont.mw.avg + beta) 
ont_licoat_male_lin_reg = lm(ont.male.lico.aftertax ~ 0 + ont.mw.avg + beta) 

# Females in Poverty in Ontario

ont_lim_fem_lin_reg = lm(ont.fem.lim ~ 0 + ont.mw.avg + beta) 
ont_licobt_fem_lin_reg = lm(ont.fem.lico.beforetax ~ 0 + ont.mw.avg + beta)
ont_licoat_fem_lin_reg = lm(ont.fem.lico.aftertax ~ 0 + ont.mw.avg + beta)

# Families in Poverty in Ontario

ont_lim_fam_lin_reg = lm(ont.fam.lim ~ 0 + ont.mw.avg + beta) 
ont_licobt_fam_lin_reg = lm(ont.fam.lico.beforetax ~ 0 + ont.mw.avg + beta)
ont_licoat_fam_lin_reg = lm(ont.fam.lico.aftertax ~ 0 + ont.mw.avg + beta)

# Single Individuals in Poverty in Ontario

ont_lim_single_lin_reg = lm(ont.single.lim ~ 0 + ont.mw.avg + beta)
ont_licobt_single_lin_reg = lm(ont.single.lico.beforetax ~ 0 + ont.mw.avg + beta)
ont_licoat_single_lin_reg = lm(ont.single.lico.aftertax ~ 0 + ont.mw.avg + beta)


# Comparing the Fitted Values for Ontario Minimum Wage across the various Poverty Measures 

ont_lim_a = as.double(coef(ont_lim_all_lin_reg)[2])
ont_licobt_a = as.double(coef(ont_licobt_all_lin_reg)[2])
ont_licoat_a = as.double(coef(ont_licoat_all_lin_reg)[2])

ont_lim_male = as.double(coef(ont_lim_all_lin_reg)[2])
ont_licobt_male = as.double(coef(ont_lim_all_lin_reg)[2])
ont_licoat_male = as.double(coef(ont_lim_all_lin_reg)[2])

ont_lim_fem = as.double(coef(ont_lim_all_lin_reg)[2])
ont_licobt_fem = as.double(coef(ont_licobt_fem_lin_reg)[2])
ont_licoat_fem = as.double(coef(ont_licoat_fem_lin_reg)[2])

ont_lim_fam = as.double(coef(ont_lim_fam_lin_reg)[2])
ont_licobt_fam = as.double(coef(ont_licobt_fam_lin_reg)[2])
ont_licoat_fam = as.double(coef(ont_licoat_fam_lin_reg)[2])

ont_lim_single = as.double(coef(ont_lim_single_lin_reg)[2])
ont_licobt_single = as.double(coef(ont_licobt_single_lin_reg)[2])
ont_licoat_single = as.double(coef(ont_licoat_single_lin_reg)[2])


alphas = rbind(ont_lim_a,ont_licoat_a,ont_licobt_a,
               ont_lim_male,ont_licoat_male,ont_licobt_male,
               ont_lim_fem,ont_licoat_fem,ont_licobt_fem,
               ont_lim_fam,ont_licoat_fam,ont_licobt_fam,
               ont_lim_single,ont_licoat_single,ont_licobt_single)

colnames(alphas) = c("Min Wage Coefficients")

# --------      LINEAR MODEL USING THE SANDWICH PACKAGE      ----------------

HC_ont_lim_all_lin_reg = sqrt(diag(vcovHC(ont_lim_all_lin_reg,type="HC0")))

