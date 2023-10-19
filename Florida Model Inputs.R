##################################################################
##                  Model Inputs & Assumptions                  ##
##################################################################

# rm(list = ls())
library("readxl")
library(tidyverse)
library(dplyr)
library(zoo)

#1. Actuarial and economic assumptions:
dr_current_ <- 0.067   #discount rate for current members
dr_new_ <- 0.067   #discount rate for new members
payroll_growth_ <- 0.0325   #payroll growth assumption
pop_growth_ <- 0   #plan's active population growth assumption

#2. Benefit assumptions:
#Cost of living adjustment (COLA) assumptions:
COLA_current_active <- 0.03   #COLA for current active members
COLA_new_active <- 0.03   #COLA for new active members (note that this applies to the cash balance plan too if new members go to the CB plan)
COLA_current_retire <- 0.03   #COLA for current retirees
COLA_current_retire_one <- 0   #One-time COLA for current retirees
one_time_cola_ <- F   #One-time COLA or not? True means yes, False means no


#3. Funding assumptions
funding_policy_ <- "ADC"

#Amortization policy
amo_pay_growth_ <- 0.0325
amo_period_current_ <- 20
amo_period_new_ <- 20
amo_method_ <- "level %"


#4. Investment assumptions
return_scen_ <- "assumption"
model_return_ <- 0.067


#5. Plan design assumptions

#6. Model assumptions 
ModelPeriod <- 100    #Projection period (typically 30 years)
MinAge <- 20          #Age of the typical youngest member
MaxAge <- 120         #Max age from mortality assumptions
YearStart <- 2022     #Year of the latest val report
MinYear <- 1977       #Matching the minimum entry year in the head count data
MaxYear <- YearStart + ModelPeriod + MaxAge - MinAge

EntryYear <- MinYear:(YearStart + ModelPeriod)
RetYear <- MinYear:(YearStart + ModelPeriod)
Years <- MinYear:MaxYear
Age <- MinAge:MaxAge
YOS <- 0:70
RetirementAge <- Age


retiree_pop_current <- 156225
ben_payment_current <- 7167927000
retire_refund_ratio_ <- 0.8

cal_factor_ <- 1
nc_cal_ <- 10.86 / 12.56535


PVFB_term_current <- 105264324785 - 89466332440
amo_period_term <- 50


#7. Import key data tables
FileName <- 'Florida.xlsx'

SurvivalRates <- read_excel(FileName, sheet = 'Mortality Rates')#Updated* (to RP-2014 General)
MaleMP <- read_excel(FileName, sheet = 'MP-2018_Male') #Updated* (to MP-2019)
FemaleMP <- read_excel(FileName, sheet = 'MP-2018_Female')#Updated* (to MP-2019)
SalaryGrowth <- read_excel(FileName, sheet = "Salary Growth")#Updated* (How to combined YOS & AGE increases?)
WithdrawalRates <- read_excel(FileName, sheet = 'Withdrawal Rates')#Updated*
RetirementRates <- read_excel(FileName, sheet = 'Retirement Rates')#Updated*
SalaryEntry_ <- read_excel(FileName, sheet = "Entrant Profile")
SalaryMatrix <- read_excel(FileName, sheet = "Salary Distribution Regular")
HeadCountMatrix <- read_excel(FileName, sheet = "HeadCount Distribution Regular")

RetireeDistribution <- read_excel(FileName, sheet = "Retiree Distribution") 

funding_data <- read_excel(FileName, sheet = "Funding Data")
return_scenarios <- read_excel(FileName, sheet = "Return Scenarios")
