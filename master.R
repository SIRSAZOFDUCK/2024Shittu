### Inequalities in childhood vaccination administration in England
### COVER data, UKHSA
### Shittu et al 2024


## SET UP ----------

# Clear environment

rm(list=ls())

# Define paths

path.project <- "C:/Users/sirsa/OneDrive/Documents/2024Shittu"

# Load packages

list.of.packages <- c("data.table", "dplyr","fingertipsR","Cairo", "ggplot2", "stringr", "readxl", "janitor", "betareg")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(dplyr)
library(data.table)
library(fingertipsR)
library(Cairo)
library(ggplot2)
library(stringr)
library(readxl)
library(janitor)
library(betareg)

# Create results directory

dir.create(paste0(path.project,"/results"))

## LOAD DATA --------

# Vaccination data
# Source: https://www.gov.uk/government/publications/cover-of-vaccination-evaluated-rapidly-cover-programme-annual-data

data.vaccine <- read_excel("cover-gp-annual-2022-to-2023.xlsx", sheet = 2, skip = 2)

# IMD data
# Source: OHID Fingertips

data_imd <- fingertips_data(IndicatorID = 93553, AreaTypeID = 7) %>% # IMD scores by GP practice
  # Keep IMD(2019) scores only
  filter(Timeperiod == "2019") %>% 
  # Remove England value
  filter(AreaType != "England") %>% 
  # Keep required fields only
  select(AreaCode, AreaName, ParentName, Value) %>% 
  # Rename columns
  dplyr::rename("practice_code" = "AreaCode", 
                "gp.name" = "AreaName",
                "pcn.name" = "ParentName",
                "imd.score" = "Value") %>%
  # Keep required columns
  select(practice_code, imd.score)

# Practice demographics data
# Source: https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice/september-2023

  # Read data - males
  
  data_listsize_m <- read.csv("gp-reg-pat-prac-sing-age-male.csv")  
  
  # Get total males per practice
  
  data_listsize_m_all <- data_listsize_m %>%
    # Filter all age rows
    filter(AGE == "ALL") %>%
    # Select required rows
    select(4,8)
  
  # Get total males up to 5 years per practice
  
  data_listsize_m_5y <- data_listsize_m %>%
    # Filter all age rows
    filter(AGE == 0 | AGE == 1 | AGE == 2 | AGE == 3 | AGE == 4 | AGE == 5) %>%
    # Select required rows
    select(4,8) %>%
    # Aggregate
    group_by(ORG_CODE) %>%
    summarize(age0to5 = sum(NUMBER_OF_PATIENTS))
  
  # Read data - females
  
  data_listsize_f <- read.csv("gp-reg-pat-prac-sing-age-female.csv")  
  
  # Get total females per practice
  
  data_listsize_f_all <- data_listsize_f %>%
    # Filter all age rows
    filter(AGE == "ALL") %>%
    # Select required rows
    select(4,8)
  
  # Get total females up to 5 years per practice
  
  data_listsize_f_5y <- data_listsize_f %>%
    # Filter all age rows
    filter(AGE == 0 | AGE == 1 | AGE == 2 | AGE == 3 | AGE == 4 | AGE == 5) %>%
    # Select required rows
    select(4,8) %>%
    # Aggregate
    group_by(ORG_CODE) %>%
    summarize(age0to5 = sum(NUMBER_OF_PATIENTS))


## PROCESS DATA -----------

# Number of practices in original dataset

print(paste0("There are ", nrow(data.vaccine)," practices in the COVER dataset"))

# Process vaccine data

data.vaccine <- data.vaccine %>%
  # Remove practices with [note1] - small numbers
  filter(!apply(., 1, function(row) any(row == "[note1]"))) %>%
  # Convert all numbers to numeric type
  mutate(`12m denominator` = as.numeric(`12m denominator`)) %>%
  mutate(`24m Denominator` = as.numeric(`24m Denominator`)) %>%
  mutate(`5y Denominator` = as.numeric(`5y Denominator`)) %>%
  #Calculate vaccination numerators
  mutate(`12mDTaPIPVHibHepB` = `12mDTaPIPVHibHepB%` * `12m denominator` / 100) %>%
  mutate(`12m MenB` = `12m MenB%` * `12m denominator` / 100) %>%
  mutate(`12m PCV1` = `12m PCV1%` * `12m denominator` / 100) %>%
  mutate(`12m Rota` = `12m Rota%` * `12m denominator` / 100) %>%
  mutate(`24m DTaP/IPV/Hib(Hep)` = `24m DTaP/IPV/Hib(Hep) %` * `24m Denominator` / 100) %>%
  mutate(`24m MMR1` = `24m MMR1%` * `24m Denominator` / 100) %>%
  mutate(`24m Hib/MenC` = `24m Hib/MenC%` * `24m Denominator` / 100) %>%
  mutate(`24m PCV Booster` = `24m PCV Booster%` * `24m Denominator` / 100) %>%
  mutate(`24m MenB Booster` = `24m MenB Booster%` * `24m Denominator` / 100) %>%
  mutate(`5y DTaP/IPV/Hib` = `5y DTaP/IPV/Hib%` * `5y Denominator` / 100) %>%
  mutate(`5y Hib/MenC` = `5y Hib/MenC%` * `5y Denominator` / 100) %>%
  mutate(`5y DTaPIPV` = `5y DTaPIPV%` * `5y Denominator` / 100) %>%
  mutate(`5y MMR1` = `5y MMR1%` * `5y Denominator` / 100) %>%
  mutate(`5y MMR2` = `5y MMR2%` * `5y Denominator` / 100)
  
# Number of practices remaining

print(paste0("There are ", nrow(data.vaccine), " practices remaining after those with denominators <5 were excluded (data not reported to protect patient identifiable information"))

# Link to IMD data

data <- data.vaccine %>%
  # Rename column to GP code columns are named the same
  rename(practice_code = `GP code`) %>%
  # Join IMD data
  left_join(data_imd, by = "practice_code") %>%
  # Remove practice with no IMD score
  filter(!is.na(imd.score))

# Number of practices remaining

print(paste0("There are ", nrow(data), " practices remaining after those without an IMD score were excluded"))

# Process practice demographic data

  # Practice listsize

  listsize <- data_listsize_f_all %>%
    # Bind both male and female list sizes
    rbind(data_listsize_m_all) %>%
    # Aggregate
    group_by(ORG_CODE) %>%
    summarize(listsize = sum(NUMBER_OF_PATIENTS))
  
  # Number of children 0 to 5 years
  
  age0to5 <- data_listsize_f_5y %>%
    # Bind both male and female list sizes
    rbind(data_listsize_m_5y) %>%
    # Aggregate
    group_by(ORG_CODE) %>%
    summarize(age0to5 = sum(age0to5))

  # Calculate proportions
  
  demographics <- listsize %>%
    # Join to number of females
    left_join(data_listsize_f_all, by = "ORG_CODE") %>%
    # Join to number of 0 to 5s
    left_join(age0to5, by = "ORG_CODE") %>%
    # Rename columns
    rename(
      "practice_code" = "ORG_CODE",
      "n.females" = "NUMBER_OF_PATIENTS",
      "n.under5s" = "age0to5"
    ) %>%
    # Calculate proportion of females
    mutate(p.females = 100* n.females / listsize) %>%
    # Calculate proportion of under 5s
    mutate(p.under5s = 100*n.under5s / listsize) %>%
    # Remove not needed columns
    select(-c(3,4))

# Link demographics to vaccine data and define quintiles
  
data <- data %>%
  # Join demographic data
  left_join(demographics, by = "practice_code") %>%
  # Define quintiles for independent variables, as keep as factors
  mutate(imd.quintile = as.factor(ntile(imd.score, 5))) %>%
  mutate(listsize.quintile = as.factor(ntile(listsize, 5))) %>%
  mutate(under5s.quintile = as.factor(ntile(p.under5s, 5))) %>%
  mutate(females.quintile = as.factor(ntile(p.females, 5)))
  

## ANALYSIS (QUASIBINOMIAL LOGISTIC REGRESSION - overdispersion) ---------
## Note: estimated coefficients are log odds

# Set results directory

setwd(paste0(path.project,"/results"))

# EXAMPLE: MMR1 at 2 years

  # Identify which vaccine is being analysed

  vaccine <- "MMR1 at 24 months"

  # Get required data
  
  test <- data %>%
    # Select required columns
    select(practice_code, imd.quintile, listsize.quintile, females.quintile, under5s.quintile,
           `24m MMR1`, `24m Denominator`) %>%
    # Rename numerator (success) and denominator (total)
    rename(
      success = 6,
      total = 7
    )

  # Fit UNIVARIATE binomial regression model
  
  model <- glm(cbind(success, total - success) ~ imd.quintile, family = quasibinomial, data = test)
  summary(model)
  
  # Calculate odds ratio
  
  odds_ratios <- exp(coef(model))
  
  # Calculate 95% confidence intervals
  
  conf_int <- exp(confint(model))
  
  # Combine odds ratios and confidence intervals into a data frame
  
  results <- as.data.frame(cbind(odds_ratios, conf_int)) %>%
    # Clean output
    mutate(OR = round(odds_ratios, 3)) %>%
    mutate(`95% CI` = paste0("(",round(`2.5 %`,3)," - ",round(`97.5 %`,3),")")) %>%
    # remove unneeded columns
    select(-c(1,2,3))
  
  # Clean rownames
  
  rownames(results)[1] <- "IMD quintile 1"
  rownames(results)[2] <- "IMD quintile 2"
  rownames(results)[3] <- "IMD quintile 3"
  rownames(results)[4] <- "IMD quintile 4"
  rownames(results)[5] <- "IMD quintile 5"
  
  # Indicate first row is the reference row
  
  results[1,1] <- "1"
  results[1,2] <- "Reference"
  
  # Save
  
  write.csv(file = paste0(vaccine," univariate.csv"), results, row.names = T)
  
  
  # Fit MULTIVARIATE binomial regression model
  
  model <- glm(cbind(success, total - success) ~ imd.quintile + listsize.quintile + females.quintile + under5s.quintile, family = quasibinomial, data = test)
  summary(model)
  
  # Calculate odds ratio
  
  odds_ratios <- exp(coef(model))
  
  # Calculate 95% confidence intervals
  
  conf_int <- exp(confint(model))
  
  # Combine odds ratios and confidence intervals into a data frame
  
  results <- as.data.frame(cbind(odds_ratios, conf_int)) %>%
    # Clean output
    mutate(OR = round(odds_ratios, 3)) %>%
    mutate(`95% CI` = paste0("(",round(`2.5 %`,3)," - ",round(`97.5 %`,3),")")) %>%
    # remove unneeded columns
    select(-c(1,2,3))
  
  # Clean rownames
  
  rownames(results)[1] <- "IMD quintile 1"
  rownames(results)[2] <- "IMD quintile 2"
  rownames(results)[3] <- "IMD quintile 3"
  rownames(results)[4] <- "IMD quintile 4"
  rownames(results)[5] <- "IMD quintile 5"
  rownames(results)[6] <- "List size quintile 2"
  rownames(results)[7] <- "List size quintile 3"
  rownames(results)[8] <- "List size quintile 4"
  rownames(results)[9] <- "List size quintile 5"
  rownames(results)[10] <- "Females quintile 2"
  rownames(results)[11] <- "Females quintile 3"
  rownames(results)[12] <- "Females quintile 4"
  rownames(results)[13] <- "Females quintile 5"
  rownames(results)[14] <- "Under 5s quintile 2"
  rownames(results)[15] <- "Under 5s quintile 3"
  rownames(results)[16] <- "Under 5s quintile 4"
  rownames(results)[17] <- "Under 5s quintile 5"
  
  # Create reference rows
  
  ref_row <- data.frame(column1 = "1", column2 = "Reference") %>%
    rename("OR" = column1, "95% CI" = column2)
  
  # Indicate reference rows
  
  results[1,1] <- "1"
  results[1,2] <- "Reference"
  
  results <- rbind(results[1:5, ], ref_row, 
                             results[6:9, ], ref_row, 
                             results[10:13, ], ref_row, 
                             results[14:17, ])
  
  rownames(results)[6] <- "List size quintile 1"
  rownames(results)[11] <- "Females quintile 1"
  rownames(results)[16] <- "Under 5s quintile 1"

  
  # Save
  
  write.csv(file = paste0(vaccine," multivariate.csv"), results, row.names = T)
  
