################################################
# Student names (Matrikelnummer):
# Date submitted:
# Objective:                       Exam of the course "Introduction to R for health scientists"
# Lecturer:                        Ralf Krumkamp
# contact details:                 krumkamp@bnitm.de
################################################


################# NOTES ########################
# Document your R-code under each question. The code should be running without
# an error or warning message. Please write notes to the script, which briefly
# explain what happens at a particular step in the code.
# Please complete the script, save it and send it to krumkamp@bnitm.de until 11.03.2022.
# The script can be written in groups up to 3 people.
#
# Study data description:
# This data was collected  during the Ebola outbreak 2014/15 at an hospital
# in Guinea. Data on infection, outcome and demographics are recorded.
################################################



# <!-- ---------------------------------------------------------------------- -->
# <!--                       load the required packages                       -->
# <!-- ---------------------------------------------------------------------- --> 
packages<-c("readxl","tidyverse", "knitr", "papeR", "kableExtra")
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
ipak(packages)


# Suppress warnings 
oldwarning_Setting <- getOption("warn")
options(warn = -1) 


# import study data from the excel file "ebola_guinea.xlsx"
# @Yimeng: please change the path with you local path
ebola_guinea <- read_excel("C:/Users/zbai/Documents/GitHub/R-Projects/SAS/2021Oct15 Yimeng/2022.02.24/ebola_guinea.xlsx")
 



### Describe study data:
# describe age of all patients (age in years; min, max interquartile range, mean, median)
str(ebola_guinea)

# ageinyears as continious variable
ebola_guinea$ageinyears <- ifelse(!is.na(ebola_guinea$ageinyears),
                                  as.numeric(ebola_guinea$ageinyears),NA)

# Descriptive statistics for continious variable
DecConti <- papeR::summarize(ebola_guinea %>% select(ageinyears), 
                             type = "numeric", 
                             show.NAs = T, 
                             test = F)

# Adjust output
rownames(DecConti) <- NULL
kable(DecConti, caption = "Descriptive statistics of age in years", 
      format = "html") %>%
  kable_styling(latex_options = "striped")

 

# frequency and percentage of Ebola diagnosis, show missings (EBOVcase)
# frequency and percentage of malaria diagnosis, show missings (malaria)
# EBOVcase and malaria as factor variables
ebola_guinea$EBOVcase <- factor(ebola_guinea$EBOVcase,
                                levels = c("0","1"),
                                labels = c("0: negative","1: positive"))
ebola_guinea$malaria <- factor(ebola_guinea$malaria,
                               levels = c("neg","pos","NA"),
                               labels = c("negative","positive","missing"))

 
# Descriptive statistics for categorical variables
DecCateg <- papeR::summarize(ebola_guinea %>% select(EBOVcase, malaria),  
                             type = "factor", 
                             show.NAs = T, 
                             test = F)
 
# Adjust output
rownames(DecCateg) <- NULL
kable(DecCateg, caption = "Descriptive statistics of Ebola diagnosis and malaria diagnosis,", 
      format = "html") %>%
  kable_styling(latex_options = "striped")


 

### generate and recode variables: 
# generate a new variable called death_d, which is 1 for patients who died and 0 for survives

# check the outcome
table(ebola_guinea$outcome)
ebola_guinea <- ebola_guinea %>%
  mutate(death_d = if_else(outcome =="dead", 1, 0))

# check if recoding for the death_d variable worked correctly

# recheck the results
table(ebola_guinea$outcome,ebola_guinea$death_d)





# cut age variables into groups 0-19, 10-39, 40-59, and 60-max. 
# save to a new variable called age_c
ebola_guinea <- ebola_guinea %>%
  mutate(
    age_c = case_when(
    is.na(ageinyears)  ~ 999,  
    ageinyears <= 19 ~ 1,
    ageinyears <= 39 ~ 2,
    ageinyears <= 59 ~ 3,
    ageinyears >= 60 ~ 4), 
    age_c = factor(age_c,
                   levels = c("1","2","3","4","999"),
                   labels = c("0-19", "10-39", "40-59", "60-max", "missing")))
 
 
# check with a table if coding worked correctly 
# check using min and max of ageinyears
papeR::summarize(ebola_guinea %>% select(ageinyears, age_c),
                 group = "age_c",
                 type = "numeric", 
                 show.NAs = T, 
                 test = F) %>%
  kable(caption = "Descriptive statistics of Ebola diagnosis and malaria diagnosis,", 
        format = "html") %>%
  kable_styling(latex_options = "striped")




 
 


## analysis of study data
# generate a barplot showing the number of positive Ebola cases (those 1 in EBOVcase) 
# over the study months (study_month variable)
theme_set(theme_classic())
ebola_guinea %>% 
  filter(EBOVcase == "1: positive") %>%
  mutate(study_monthorder = factor(study_month, 
                                   levels=(as.character(3:15)))) %>%
  ggplot() + 
  geom_bar(mapping = aes(x = study_monthorder), width = 0.7) + 
  labs(title="Barplot showing the number of positive Ebola cases over the study months ", 
       subtitle="Manufacturer across Vehicle Classes") +
  xlab("Study months") +
  ylab("Number of positive Ebola cases")


# CT_value show the circle-threshold of the PCR test for Ebola, lower values indicate
# high viral load and high ct-values a low viral load. Do a boxplot showing
# ct-value distribution of Ebola patients who died and who survived
theme_set(theme_classic())
ebola_guinea %>% 
  mutate(outcome = factor(outcome, levels = c("dead", "survived")),
         CT_value = as.numeric(CT_value)) %>%
  ggplot(aes(outcome, CT_value ))+
  geom_boxplot(varwidth=T) + 
  labs(title="Box plot showing ct-value distribution of Ebola patients who died and who survived", 
       x="Survival outcome",
       y="Circle-threshold of the PCR test for Ebola")


# calculate the odds ratio on the association between Ebola infection and the chance of death
# using the package epiR

# install.packages("epiR")
# library("epiR")
epiR::epi.2by2(table(ebola_guinea$EBOVcase, ebola_guinea$outcome))
 


# Restore warning settings
options(warn = oldwarning_Setting)

