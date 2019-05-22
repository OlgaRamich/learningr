# Load the packages
library(tidyverse)
library(NHANES)

# Check column names
colnames(___)

source(here::here("R/package_loading.R"))

# Check column names
colnames(NHANES)

# Look at contents
str(NHANES)
glimpse(NHANES)

# See summary
summary(NHANES)

# Look over the dataset documentation
?NHANES

NHANES %>%
colnames()

length(colnames(NHANES))

NHANES %>%
  mutate(Height=Height/100,
         Testing="yes",
         Highlyactive=if_else(PhysActiveDays>=5, "yes", "no"))
#means if somebody is active more than 5 days a week, yes; otherwise not


NHANES_updated <- NHANES %>%
mutate(UrineVilAverage = (UrineVol1 + UrineVol2)/ 2)

# #Pipeline ---------------------------------------------------------------

# Check the names of the variables
colnames(NHANES)

# Pipe the data into mutate function and:

NHANES_modified <- NHANES %>%
  mutate(UrineVolAverage = (UrineVol1 + UrineVol2)/ 2)

NHANES_modified <- NHANES %>%
mutate(PulsePerSec = Pulse/60)

NHANES_modified <- NHANES %>%
  mutate(YoungChild=if_else(Age>=6, TRUE, FALSE))

summary(NHANES_modified$UrineVolAverage)


# #Selecting Variables ----------------------------------------------------

NHANES %>%
  select(-BMI)

NHANES %>%
  select(Gender, BMI)

NHANES %>%
  select(starts_with("Smoke"),
         contains("Vol"),
         matches("[123]"))

NHANES %>%
  rename(
    #NewName=OldName
    NumberBabies=nBabies,
    Sex=Gender
    )

NHANES %>%
  select(BMI, NumberBabies=nBabies, Gender, Height)

NHANES %>%
  filter(Gender!="female")

NHANES %>%
  filter(BMI>=25 & Gender == "female")

NHANES %>%
  filter(BMI>=25 | Gender == "female")

NHANES %>%
  arrange(Age) %>%
  select (Age)


NHANES %>%
  arrange(desc(Age), Gender) %>%
  select (Age, Gender)



# #Exercise ---------------------------------------------------------------

# To see values of categorical data
summary(NHANES)

# 1. BMI between 20 and 40 and who have diabetes
NHANES %>%
   filter(BMI >= 20 & BMI <= 40 & Diabetes == "Yes")


# 2. Working or renting, and not diabetes
NHANES %>%
  filter("Work" == "Working" | HomeOwn == "Rent" & Diabetes == "No") %>%
  select(Age, Gender, Work, HomeOwn, Diabetes)





# 3. How old is person with most number of children.
summary(NHANES$nBabies)

NHANES %>%
  arrange(nBabies) %>%
  select(Age)



# #Group by and summarize -------------------------------------------------

NHANES %>%
  summarise(MaxAge=max(Age, na.rm=FALSE))

NHANES %>%
  summarise(MaxAge=max(Age, na.rm=TRUE),
            MinBMI= min(BMI, na.rm=TRUE))


NHANES %>%
  group_by(Gender) %>%
  summarise (MeanBMI=mean(BMI, na.rm=TRUE))

NHANES %>%
  group_by(Gender, Work) %>%
  summarise (
    MeanBMI=mean(BMI, na.rm=TRUE),
    MeanAge=mean(Age, na.rm=TRUE)
    )



# #Conversion to long form ------------------------------------------------

table4b %>%
gather (year, population, -country)


table4b %>%
  gather (year, population)

table4b %>%
  gather (year, population, '1999', '2000')

nhanes_simple <- NHANES %>%
select(SurveyYr, Gender, Age, Weight, Height, BMI, BPSysAve)


nhanes_long <- nhanes_simple %>%
  gather(Measure, Value, -SurveyYr, -Gender)

nhanes_long  %>%
  group_by (SurveyYr, Gender, Measure) %>%
  summarise(
    MeanValue=mean(Value, na.rm=TRUE)
  )



table2 %>%
  spread(type, count)

nhantes_summary <- nhanes_long  %>%
  group_by (SurveyYr, Gender, Measure) %>%
  summarise(
    MeanValue=mean(Value, na.rm=TRUE)
  )

nhantes_summary %>%
  spread (SurveyYr, MeanValue)

nhantes_summary %>%
  spread (Gender, MeanValue)




# #Final_exercise ---------------------------------------------------------


  colnames(NHANES)
summary(NHANES)

NHANES_modified <- NHANES %>%
  rename(
    AgeDiabetesDiagnosis = DiabetesAge,
    DrinksOfAlcoholInDay = AlcoholDay,
    NumberOfBabies=nBabies,
    TotalCholesterol=TotChol
  ) %>%
mutate(MoreThan5DaysActive=if_else(PhysActiveDays>=5, "yes", "no")) %>%
select(SurveyYr, Gender, Age, AgeDiabetesDiagnosis,  BMI,  BPDiaAve,  BPSysAve,  DrinksOfAlcoholInDay,
         MoreThan5DaysActive, NumberOfBabies, Poverty, TotalCholesterol)


NHANES_mod_long <- NHANES_modified %>%
  gather(Measure, Value, -Gender, -SurveyYr)

filter(Age >= 18, Age <= 75)

NHANES_mod_long  %>%
  group_by (SurveyYr, Gender, Measure) %>%
  summarise(MeanValue=mean(Value, na.rm=TRUE) ) %>%
  arrange(Measure, Gender, SurveyYr) %>%
  spread(SurveyYr, MeanValue)

