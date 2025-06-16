#HIV data 2000-2023

##Importing the dataset
library(readr)
HIV_data_2000_2023 <- read_csv("CEMAproject/Datasets/HIV data 2000-2023.csv")
View(HIV_data_2000_2023)

##Loading the neccesary packages
library(dplyr)
library(magrittr)

##Cleaning the value column
HIV_data_2000_2023$Value <- gsub("^([0-9]+)\\s([0-9]+).*","\\1\\2", HIV_data_2000_2023$Value)

HIV_data_2000_2023$Value <- gsub("^([0-9]+)\\s.*","\\1", HIV_data_2000_2023$Value)

HIV_data_2000_2023$Value <- gsub("<([0-9]+)\\s.*","\\1", HIV_data_2000_2023$Value)

HIV_data_2000_2023 = HIV_data_2000_2023[HIV_data_2000_2023$Value != "No data",]

HIV_data_2000_2023$Value <-  as.numeric(HIV_data_2000_2023$Value)

##Filtering data for 2023
hiv_2023 <- filter(HIV_data_2000_2023, Period == 2023)
View(hiv_2023)

##Arranging in descending order
country_case_2023 <- hiv_2023 %>%
  group_by(Location) %>%
  summarise(total_cases = sum(Value)) %>%
  arrange(desc(total_cases))
View(country_case_2023)

##Calculating cumulative percent
country_case_2023 <-  country_case_2023 %>%
  mutate(cumulative_percent = cumsum(total_cases)/sum(total_cases) * 100)


##filtering the top 75 countries
top_75 <- country_case_2023 %>%
  filter(cumulative_percent<=75.6) %>%
  pull(Location)
View(top_75)

##Full dataset for top 75 countries
top_75_2000_2023 <- HIV_data_2000_2023 %>%
  filter(Location %in% top_75)
View(top_75_2000_2023)

##Trend of HIV cases in the countries that contribute to 75% of the global burden 
c <- top_75_2000_2023 %>%
  ggplot()+
  geom_line(aes(x =Period , y = Value, colour = Location))+
  theme_bw()+
  labs(title = "Trend of HIV cases in the countries that contribute to 75% of the global burden ",
       x = "Year(2000-2023)", y = "Estimated number of people (all ages) living with HIV")
  
ggplotly(c)

###Trend of HIV cases in the countries contributing to 75% of the burden within each WHO region
d <- top_75_2000_2023 %>%
  ggplot()+
  geom_line(aes(x =Period , y = Value, colour = Location))+
  theme_bw()+
  facet_wrap(vars(ParentLocation))+
  labs(title = "Trend of HIV cases in the countries contributing to 75% of the burden within each WHO region",
       x = "Year(2000-2023)", y = "Estimated number of people (all ages) living with HIV")
  
ggplotly(d)





#neonatal and under five mortality rate

##Importing the dataset
library(readr)
dataset_datascience <- read_csv("CEMAproject/Datasets/dataset_datascience.csv")
View(dataset_datascience)

##Filtering data for only required columns
neonatal_underfive_data <-  dataset_datascience %>%
  select(`Geographic area`,Indicator,`Observation Value`,`Series Year`,`Reference Date`)
View(neonatal_underfive_data)

##Renaming the columns
colnames(neonatal_underfive_data) = c("Geographic_area","Indicator","Observation_Value",
                                      "Series_Year", "Reference_Date")



##Countries in the EAC
eac_countries = c("Burundi", "Democratic Republic of the Congo", "Kenya",
                  "Rwanda", "Somalia", "South Sudan", "Tanzania", "Uganda")

##Filtering data for EAC countries
eac_data <-  neonatal_underfive_data %>%
  filter(Geographic_area %in% eac_countries)
View(eac_data)


###Filtering data for neo natal mortality rate in EAC countries
neo_natal <-filter(eac_data, Indicator == "Neonatal mortality rate")
View(neo_natal)

###Filtering data for under five mortality rate in EAC countries
under_five <- filter(eac_data, Indicator == "Under-five mortality rate")
View(under_five)


##Getting the latest estimate for neo natal mortality rate
latest_estimate_neo <- filter(neo_natal, Reference_Date == 2023.5)
View(latest_estimate_neo)

##Getting the latest estimate for under five mortality rate
latest_estimate_underfive <- under_five %>%
  group_by(Geographic_area) %>%
  filter(Reference_Date == 2023.5,Series_Year == 2023)
View(latest_estimate_underfive)









 





