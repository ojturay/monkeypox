library(tidyverse)
library(ggthemes)
library(stringr)
library(visdat)
library(janitor)
library(sqldf)
library(gsubfn)
library(lubridate)

str(monkeypox)
colnames(monkeypox)

#cleaning data of unwanted column
monkeypoxdata <- monkeypox %>%
  select(-c(Date_onset, Date_hospitalisation, Date_isolation,
            Date_entry, Date_last_modified, Contact_ID, Contact_location,
            Genomics_Metadata, Confirmation_method, Source, Source_II,
            Source_III, Source_IV, Source_V, Source_VI, Source_VII,
            Country_ISO3, Travel_history_entry, Travel_history_start,
            Travel_history_location))

monkeypoxdataclean <- clean_names(monkeypoxdata)
vis_miss(monkeypoxdata)
#data cleaned enough and ready for analysis

#occurence of symptoms in confirmed cases 
monkeypoxdata_confirmed <- monkeypoxdataclean %>%
  filter(status == 'confirmed') %>%
  mutate(Symptoms = tolower(symptoms))

symptoms <- c('fever', 'headache', 'muscle pain', 'backache', 'lymph nodes', 'vesicular rash',
              'fatigue', 'lesion', 'pustule', 'blister', 'cough', 'rash','ulcer')

symptoms_count <- sapply(symptoms, function(symptoms) sum(grepl(symptoms, monkeypoxdata_confirmed$symptoms)))

symptoms_table <- data.frame(symptoms, symptoms_count); symptoms_table


ggplot(symptoms_table, aes(fill=symptoms, x= symptoms_count,
                           y= fct_reorder(symptoms,symptoms_count))) +
  geom_col() + theme_economist() +
  labs('Frequency of Occurence of Symptoms in Confirmed Cases', y = '',
       x = 'Number of Cases')


#distribution of confirmed cases based on location 
monkeypox_distribution <- monkeypoxdata_confirmed %>%
  group_by(country) %>%
  summarise(number_of_cases = n()) %>%
  arrange(desc(number_of_cases))

#renaming the country column in the distribution data for easy joining to the world map
monkeypox_distribution_region <- rename(monkeypox_distribution, region = country)

#renaming 'United States' in the monkeypoxdistribution to USA to join to the world map
monkeypox_distribution_usa_corrected <- monkeypox_distribution_region %>%
  mutate(region = replace(region, region == 'United States', 'USA'))

#top 10 Infected countries
monkeypox_distribution_usa_corrected_top10<- monkeypox_distribution_usa_corrected %>%
  select(region, number_of_cases) %>%
  top_n(10) %>%
  arrange(desc(number_of_cases)) 


  ggplot(monkeypox_distribution_usa_corrected_top10,  aes(fill= region, x=number_of_cases, 
             y=fct_reorder(region,number_of_cases))) +
  geom_col() + theme_economist() + 
  labs(x = 'Number Of Cases', y = '')
  
#world map data
  world <- map_data('world')
  head(world)
  
#joining the distribution of conirmed cases  to the world map
  monkeypox_map <- left_join(world, monkeypox_distribution_usa_corrected, by = 'region')
  view(monkeypox_map)
  
  
#graphical representation on the distribution of cases 
  ggplot(monkeypox_map, aes(x= long, y = lat, group = group)) +
    geom_polygon(aes(fill = number_of_cases))  + 
    theme(axis.ticks = element_blank(), rect = element_blank()) +
    labs('Distribution of Cases', x = '', y = '') +
    scale_fill_gradient(name = 'number_of_cases', low = 'pink', high = 'red')

