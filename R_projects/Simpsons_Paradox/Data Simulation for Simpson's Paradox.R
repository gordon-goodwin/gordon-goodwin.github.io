# -------------------------------------
# Script: Simulate Simpons Paradox Data
# Author: Gordon Goodwin
# Purpose: Sim Data to Reproduce Simps Paradox
# Notes:
#   
# -------------------------------------

# No Scientific Notation
options(scipen = 999)

# Necessary Packages
library(tidyverse)
library(janitor)
library(ggfun)
library(ggtext)
library(showtext)
library(ggmosaic)
library(patchwork)
library(tidymodels)
library(RColorBrewer)
library(MetBrewer)
library(gtsummary)
library(GGally)



# Paradox 1: Simulate Data -----------------------------------------------------------


# Paradox #1
## Salary vs Seniority is NEGATIVELY correlated overall
### Salary is POSITIVELY correlated with Seniority WITHIN Departments
### Confound is due to the higher-paid departments containing newer hires

deps <- c("DataScience","HR","Product")

set.seed(12345)

dat1 <- data.frame(department = rep(deps, times = c(333, 333, 333)))

# Split by Department - we'll set up the trends within each department
split(dat1, dat1$department) -> dep_groups

#####################################
# Correlations WITHIN groups
######################################

# Data Science 
###  positive cor(salary, seniority)
### HIGH avg salary and LOW avg Seniority

## DS Salary = Mean of 200k, SD of 30k
## DS Tenure weeks = Mean of 120w, SD of 30w
## Cor(DS Salary, DS Tenure) = 0.3

n_DS <- nrow(dep_groups$DataScience)

# Mean vector = Avgs for seniority weeks and salary (k)
mean_vec_DS <- c(120, 200)

# create the variance covariance matrix
## diags = var of X and Y respectively = 30^2 and 30^2 = 900 and 900
## COV = Cor * SX * SY -> 0.3 * 30 * 30 = 270
covmat_DS <- matrix(data = c(900, 270,
                             270, 900),
                    nrow = 2,
                    byrow = T)

# generate the multivariate normal distribution and store as dataframe
dat1_DS <-as.data.frame(MASS::mvrnorm(n=n_DS, mu=mean_vec_DS, Sigma=covmat_DS))


# Set names back to x and y (defaults to V1, V2, etc...)
names(dat1_DS) <- c("seniority_weeks","salary")

## Department ID
dat1_DS$department <- "DataScience"

####################################################

# Product 

# Product
###  positive cor(salary, seniority)
### MEDIUM avg salary and MEDIUM avg Seniority

## Product Salary = Mean of 160k, SD of 25k
## Product Tenure weeks = Mean of 180d, SD of 30d
## Cor(Product Salary, Product Tenure) = 0.3

# Sample Size
n_Product <- nrow(dep_groups$Product)

# Mean vector = Avg for tenure weeks and salary (k)
mean_vec_Product <- c(180, 160)

# create the variance covariance matrix
## diags = var of X and Y respectively = 30^2 and 25^2 = 900 and 625
## COV = Cor * SX * SY -> 0.3 * 30 * 25 = 225
covmat_Product <- matrix(data = c(900, 225,
                                  225, 625),
                         nrow = 2,
                         byrow = T)

# generate the multivariate normal distribution and store as dataframe
dat1_Product <-as.data.frame(MASS::mvrnorm(n=n_Product, mu=mean_vec_Product, Sigma=covmat_Product))

# Set names back to x and y (defaults to V1, V2, etc...)
names(dat1_Product) <- c("seniority_weeks","salary")

## Department ID
dat1_Product$department <- "Product"

##########################################################

# HR
###  positive cor(salary, seniority)
### LOW avg salary and HIGH avg Seniority

## HR Salary = Mean of 120k, SD of 20k
## HR Tenure weeks = Mean of 240d, SD of 30d
## Cor(HR Salary, HR Tenure) = 0.3
n_HR <- nrow(dep_groups$HR)

# Mean vector = MU for tenure weeks and salary (k)
mean_vec_HR <- c(240, 120)

# VARCOV Matrix
## diags = var of X and Y respectively = 30^2 and 20^2 = 900 and 400
## COV = Cor * SX * SY -> 0.3 * 30 * 20 = 180
covmat_HR <- matrix(data = c(900, 180,
                             180, 400),
                    nrow = 2,
                    byrow = T)

# generate the multivariate normal distribution and store as dataframe
dat1_HR <-as.data.frame(MASS::mvrnorm(n=n_HR, mu=mean_vec_HR, Sigma=covmat_HR))

# Set names back to x and y (defaults to V1, V2, etc...)
names(dat1_HR) <- c("seniority_weeks","salary")

## Department ID
dat1_HR$department <- "HR"

#############################################################

# UNION BACK TO COMBINED DATAFRAME
# Marge Back
dat1_DS |> 
  union_all(dat1_Product) |> 
  union_all(dat1_HR) -> dat1main

# Create Seniority Years instead of weeks (more intuitive)
# Create a salary measured in $k and measured in reg USD
dat1main |> 
  mutate(seniority_years = seniority_weeks/52,
         .after = seniority_weeks) |> 
  mutate(salary_k = salary, 
         salary = salary*1000)-> dat1main

# Round to 2 digits for ease
dat1main |> 
  mutate(across(where(is.numeric),
                ~round(.x, 2))) |> 
  select(department, seniority_weeks, seniority_years,
         salary, salary_k)-> dat1main

# Add Employee ID
dat1main |> 
  rowid_to_column(var = "emp_ID") |> 
  mutate(emp_ID = paste0("x",emp_ID)) -> dat1main
  
## Preview
dat1main |> head()
 

# Paradox 1: Write CSV ---------------------------------------------------------------

dat1main |> write_csv("Simpson_Paradox_Data_CS1.csv")


# Paradox 2: Simulate Data ------------------------------------------------


# Paradox 2: Satisfaction & referral Confounded by location 
## Appears at first that referrals are LESS satisfied than non-referrals
## BUT when partitioned by work location, referrals are MORE satisfied


# Sample Size for referrals/non-referrals
n_referrals <- 300
n_nonref <- 700

referral <- c("Referred","Applied")
location <- c("Remote","Office")
satisfied <- c("Favorable","Unfavorable")

# Initialize Dataframe w/ 100 men, 100 women
dat2 <- data.frame(referral = rep(referral, times = c(n_referrals, n_nonref)))

set.seed(12345)

# Location Disparity amongst referral/non-referral
## NON-Referrals much more likely to be Remote employees
dat2 |> 
  rowwise() |> 
  mutate(location = case_when(referral == "Applied" ~ sample(location, 1, prob = c(.7, .3)),
                              referral == "Referred" ~ sample(location, 1, prob = c(.2, .8)))) |> 
  ungroup() -> dat2


# Split into Remote  vs Not Remote
split(dat2, dat2$location) -> location_groups



# Set Satisfaction rates by LOCATION GROUP
## Goal is to have:
### Referrals  SLIGHTLY HIGHER in Satisfaction WITHIN both Location groups
### Remote satisfaction overall MUCH HIGHER satisfaction
### Paradox will result due to majority of referrals being In-Office
#### due to unequal location balance by referral/non-referral status

# REMOTE
## High overall Satisfaction with Referrals slightly higher than Non-Ref
location_groups$Remote |> 
  rowwise() |> 
  mutate(satisfied = case_when(referral == "Applied" ~ sample(satisfied,1, prob = c(.7,.3)),
                               referral == "Referred" ~ sample(satisfied, 1, prob = c(.85, .15)))) |> 
  ungroup() -> dat2_Remote

# OFFICE
## Lower overall satisfaction w/ Referrals slightly higher than Non-Ref
location_groups$Office |> 
  rowwise() |> 
  mutate(satisfied = case_when(referral == "Applied" ~ sample(satisfied,1, prob = c(.15,.85)),
                               referral == "Referred" ~ sample(satisfied, 1, prob = c(.35, .65)))) |> 
  ungroup() -> dat2_Office

# Merge Back to Main Dataframe
dat2_Remote |> 
  union_all(dat2_Office) -> dat2combined

# Add Employee ID (Random, not used for anything)
dat2combined |> 
  rowid_to_column(var = "emp_ID") |> 
  mutate(emp_ID = paste0("x", emp_ID)) -> dat2combined 

# Glimpse
dat2combined |> head()

# Paradox 2: Write CSV ----------------------------------------------------

dat2combined |> write_csv("Simpson_Paradox_Data_CS2.csv")
