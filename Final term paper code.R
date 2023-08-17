
####################################CROSS-SECTIONAL DATA PREPARATION#################################################################
term_paper$year = format(as.Date(term_paper$published_at, format="%d/%m/%Y"),"%Y")
term_paper$mean_salary = (term_paper$salary.from + term_paper$salary.to)/2
df = subset(term_paper, salary.currency == "RUR")
df$probability = ifelse(is.na(df$mean_salary), 0, 1)
####################################CROSS-SECTIONAL DATA PREPARATION#################################################################

###################################CROSS-SECTIONAL DATA ANALYSIS#####################################################################
library(sampleSelection)
library(plm) 
library(lfe) 
library(sandwich) 
library(lmtest) 
library(texreg) 
library(gplots) 
library(stargazer) 
library(tidyr)
library(tidyverse)
library(caret)
library(lmtest)
library(estimatr)

df_cross = df

df_cross$auto_dummy = as.factor(df_cross$auto_dummy)
df_cross$year = as.factor(df_cross$year)
df_cross$year <- factor(df_cross$year, levels=c("2019","2020", "2021"))
df_cross$exp_factor <- factor(df_cross$experience.id, levels=c("noExperience","between1And3", "between3And6", "moreThan6"))
df_cross$schedule_factor <- factor(df_cross$schedule.id, levels=c("flexible","flyInFlyOut", "fullDay", "remote", "shift"))
df_cross$emp_factor <- factor(df_cross$employment.id, levels=c("full","part", "probation", "project", "volunteer"))
df_cross$region_factor = factor(df_cross$area.id)
df_cross$company_factor = factor(df_cross$employer.id)

ols_1 = lm(log(mean_salary) ~ auto_dummy, data = df_cross)
ols_2 = lm(log(mean_salary) ~ auto_dummy + year, data = df_cross)
ols_3 = lm(log(mean_salary) ~ auto_dummy + year + exp_factor, data = df_cross)
ols_4 = lm(log(mean_salary) ~ auto_dummy + year + exp_factor + schedule_factor, data = df_cross)
ols_all = lm(log(mean_salary) ~ auto_dummy + year + exp_factor + schedule_factor + emp_factor, data = df_cross)
ols_all_robust = lm_robust(log(mean_salary) ~ auto_dummy + year + exp_factor + schedule_factor + emp_factor, data = df_cross)

summary(list(ols_1, ols_2, ols_3, ols_4, ols_all, ols_all_robust))
stargazer(list(ols_2, ols_3), type = 'text')
stargazer(list(ols_3, ols_4), type = 'text')
stargazer(ols_all, type = 'text')
summary(ols_all_robust)

heckman = selection(probability ~ auto_dummy + year + exp_factor + schedule_factor + emp_factor, 
                    log(mean_salary) ~ auto_dummy + year + exp_factor + schedule_factor + emp_factor, data = df_cross,
                    method = '2step')

model_felm_region = felm(log(mean_salary) ~ auto_dummy + year + exp_factor + schedule_factor + emp_factor| area.id, data = df_cross)
model_felm_all = felm(log(mean_salary) ~ auto_dummy + year + exp_factor + schedule_factor + emp_factor| area.id + employer.id, data = df_cross)

stargazer(list(model_felm_region, model_felm_all), type = 'text')
stargazer(list(heckman, ols_all), type = 'text')
stargazer(list(ols_all, model_felm_all), type = 'text')

car::vif(ols_all)
check_collinearity(model_felm_all) 
lmtest::bptest(ols_all)  
lmtest::bptest(model_felm_all) 
resettest(ols_all) 
resettest(model_felm_all) 
###################################CROSS-SECTIONAL DATA ANALYSIS#####################################################################

###################################PANEL DATA PREPARATION############################################################################
df_panel = df

#creating subsets by year
df_panel_2019 = subset(df_panel, year == 2019)
df_panel_2020 = subset(df_panel, year == 2020)

#creating dataframe with companies, which have vacancies in both 2019 and 2020 
list_companies_19 = as.data.frame(unique(df_panel_2019$employer.id))
list_companies_20 = as.data.frame(unique(df_panel_2020$employer.id))

list_companies_19$uni_emp = list_companies_19$`unique(df_panel_2019$employer.id)`
list_companies_20$uni_emp = list_companies_20$`unique(df_panel_2020$employer.id)`

list_companies_19$dummy = ifelse(list_companies_19$uni_emp %in% list_companies_20$uni_emp, 1, 0)
list_companies_20$dummy = ifelse(list_companies_20$uni_emp %in% list_companies_19$uni_emp, 1, 0)

compan19 = subset(list_companies_19, dummy == 1) 
compan20 = subset(list_companies_20, dummy == 1) 

companies_19_20 = subset(df_panel, employer.id %in% compan19$uni_emp & year != 2021)

companies19 = subset(companies_19_20, year == 2019) #final df
companies20 = subset(companies_19_20, year == 2020) #final df
length(unique(companies19$employer.id))
length(unique(companies20$employer.id))
companies19 = companies19[complete.cases(companies19$mean_salary), ]
companies20 = companies20[complete.cases(companies20$mean_salary), ]
###################################PANEL DATA PREPARATION############################################################################

###################################PANEL DATA ANALYSIS###############################################################################
library(plm) 
library(lfe) 
library(sandwich) 
library(lmtest) 
library(texreg) 
library(gplots) 
library(stargazer) 
library(tidyr)

#merging into a one panel dataset 
companies_19_20 = rbind(companies19, companies20)

#removing NA from time and entity variables 
df_test = companies_19_20
sum(is.na(df_test$employer.id))
sum(is.na(df_test$year))

df_test = df_test[!(is.na(df_test$employer.id)), ]

#preparing variables 
df_test$auto_dummy = as.factor(df_test$auto_dummy)
df_test$exp_factor = as.factor(df_test$experience.id)
df_test$schedule_factor = as.factor(df_test$schedule.id)
df_test$emp_factor = as.factor(df_test$employment.id)

#pooled
pooled = plm(log(mean_salary+1) - 1 ~ auto_dummy + exp_factor + schedule_factor + emp_factor, df_test, 
             index = c("employer.id", "year"), model = "pooling")
summary(pooled)
car::vif(pooled)

#first-difference model
fd_1 = plm(log(mean_salary+1) - 1 ~ auto_dummy, df_test, 
           index = c("employer.id", "year"), model = "fd")

fd_2 = plm(log(mean_salary+1) - 1 ~ auto_dummy + exp_factor, df_test, 
           index = c("employer.id", "year"), model = "fd")

fd_3 = plm(log(mean_salary+1) - 1 ~ auto_dummy + exp_factor + schedule_factor, df_test, 
           index = c("employer.id", "year"), model = "fd")

fd_all = plm(log(mean_salary+1) - 1 ~ auto_dummy + exp_factor + schedule_factor + emp_factor, df_test, 
             index = c("employer.id", "year"), model = "fd")       

screenreg(list(pooled, fd_1), 
          custom.model.names =c("Pooled", "First-difference"),
          digits = 4)

screenreg(list(fd_2, fd_3, fd_all),
          custom.model.names = c("FD 1", "FD 2", "FD 3"),
          digits = 4) 

car::vif(fd_all)
lmtest::bptest(fd_all)
resettest(fd_all)

#within-estimator model
within_com = plm(log(mean_salary+1) - 1 ~ auto_dummy + exp_factor + schedule_factor + emp_factor, df_test, 
                 index = c("employer.id", "year"), model = "within")
summary(within_com)

#within-estimator model with fixed effects and clustered errors 
within_com_cl = felm(log(mean_salary+1) - 1 ~ auto_dummy + exp_factor + schedule_factor + emp_factor |  
                       factor(employer.id) + factor(year) + factor(area.id) | 
                       0 |
                       employer.id,  
                     df_test)

summary(within_com_cl)

#output 
screenreg(list(fd_all, within_com, within_com_cl), 
          custom.model.names =c("FD", "Within", "Within Cl"), 
          digits = 4) 

screenreg(list(fd_com, within_com), 
          custom.model.names =c("FD", "Within"),
          digits = 4) 

screenreg(list(within_com, within_com_cl), 
          custom.model.names =c("Within", "Within Cl"), 
          digits = 4) 
###################################PANEL DATA ANALYSIS###############################################################################

