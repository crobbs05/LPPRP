
install.packages("treemapify")
library(tidycensus)
library(tidyverse)
library(stringr)
library(scales)
library(sf)
library(fs)
library(ggthemes)
library(esquisse)
library(treemapify)


getwd()

setwd("C:\\Users\\chauncey.robbs\\Documents\\Projects\\LPPRP 2022\\02 Data\\Chapter 1")

vignette(package = "tidycensus")
help(package = "tidycensus")

#get net migration for the all counties in the state of maryland

#set variable for years we want net migration data. Data goes back to 2015

years <- lst(2017,2018,2019)


get_estimates(geography = "county",state = "Maryland",county = "Prince George's",variables = "NETMIG",year = years)


#load variables from census using tidycensus api

data_profile_variables <- load_variables(year = 2019,dataset = "acs5/profile")

subject_profile_variables <- load_variables(year = 2019,dataset = "acs5/subject")



####1. Prince George's County Racial Composition ####

#variables we need to collect for section are directly below#
#need to evaluate the racial composition as not hispanic to a get percent total of 100. Otherwise its greater than 100 when using other variables.#

#Hispanic is not race but but if Hispanic is a race and if given the opportunity people would choose hispanic or latino as a race#

#_p = percentage#

#adding three or more races makes the total greater than 100 percent

racial_composition_estimate <- c(african_a_not_hispanic = "DP05_0078",
                                 white_not_hispanic = "DP05_0077",
                                 asian_not_hispanic  = "DP05_0080",
                                 american_i_not_hispanic = "DP05_0079",
                                 native_h_not_hispanic = "DP05_0081",
                                 some_other_r_not_hispanic = "DP05_0082",
                                 hispanic = "DP05_0071",
                                 two_or_more_not_hispanic = "DP05_0083",
                                 two_or_more_some_other_not_hispanic= "DP05_0084",
                                 three_or_more_races_not_hispanic = "DP05_0085")


racial_composition_estimate_p <- c(african_a_not_hispanic_p = "DP05_0078P",
                                 white_not_hispanic_p = "DP05_0077P",
                                 asian_not_hispanic_p  = "DP05_0080P",
                                 american_i_not_hispanic_p = "DP05_0079P",
                                 native_h_not_hispanic_p = "DP05_0081P",
                                 some_other_r_not_hispanic_p = "DP05_0082P",
                                 hispanic_p = "DP05_0071P",
                                 two_or_more_not_hispanic_p = "DP05_0083P",
                                 two_or_more_some_other_not_hispanic_p = "DP05_0084P",
                                 three_or_more_races_not_hispanic_p = "DP05_0085P")

#get racial composition estimates and percentages#
racial_composition_estimates_county <- get_acs(variables = racial_composition_estimate, year = 2019, state = "MD",county = "Prince George's",geography = "county",geometry = FALSE)

racial_composition_estimates_county_p <- get_acs(variables = racial_composition_estimate_p, year = 2019, state = "MD",county = "Prince George's",geography = "county",geometry = TRUE)




# Multi Year Racial Composition

#Step1
multi_year_race_composition_p <- map_dfr(years, ~get_acs(variables = racial_composition_estimate_p, year = .x, state = "MD",county = "Prince George's",geography = "county",geometry = FALSE),.id = "year") %>% select(-moe) %>% arrange(variable,NAME)


#Step 2: Convert Data From Long to Wide
multi_year_race_composition_p <- multi_year_race_composition_p %>% spread(year,estimate,sep = "_")

#step 3 write csv file
write_csv(x = multi_year_race_composition_p,path = "multi_year_race_composition_percentage_v01.csv")

multi_year_race_composition_tract <- map(years, ~get_acs(variables = racial_composition_estimate, year = .x, state = "MD",county = "Prince George's",geography = "tract",geometry = TRUE)) %>% map2(years,    ~ mutate(.x, year =.y))

multi_year_race_composition_tract <- reduce(multi_year_race_composition_tract,rbind) %>% 
  select(-moe) %>% spread(year, estimate, sep = "_") 




#get racial composition estimates and percentages at the census tract level#
racial_composition_estimates_tract <- get_acs(variables = racial_composition_estimate, year = 2019, state = "MD",county = "Prince George's",geography = "tract",geometry = TRUE,output = "wide")

racial_composition_estimates_tract_p <- get_acs(variables = racial_composition_estimate_p, year = 2019, state = "MD",county = "Prince George's",geography = "tract",geometry = TRUE, output = "wide")




####2. Prince George's County Population by Age & Sex####

age_groups_estimate <- c("Under_5_years" ="DP05_0005",
"5_to_9_years" = "DP05_0006", 
"10_to_14_years" = "DP05_0007", 
"15_to_19_years" = "DP05_0008", 
"20_to_24_years" = "DP05_0009",
"25_to_34_years" = "DP05_0010",
"35_to_44_years" = "DP05_0011",
"45_to_54_years" ="DP05_0012",
"55_to_59_years" = "DP05_0013",
"60_to_64 years" = "DP05_0014",
"65_to_74_years" = "DP05_0015",
"75_to_84_years" = "DP05_0016",
"85_years_and_over" = "DP05_0017")


age_groups_percent <- c("Under_5_years_p" ="DP05_0005P",
"5_to_9_years_p" ="DP05_0006P","10_to_14_years_p" = "DP05_0007P",
"15_to_19_years_p" = "DP05_0008P","20_to_24_years_p" = "DP05_0009P",
"25_to_34_years_p" = "DP05_0010P","35_to_44_years_p" = "DP05_0011P",
"45_to_54_years_p" = "DP05_0012P","55_to_59_years_p" = "DP05_0013P",
"60_to_64 years_p" = "DP05_0014P","65_to_74_years_p" = "DP05_0015P",
"75_to_84_years_p" = "DP05_0016P","85_years_and_over_p" = "DP05_0017P")

total_pop_male_by_age <- c(
"Under_5_years" = "S0101_C03_002",
           
"5_to_9_years" = "S0101_C03_003",
"10_to_14_years" = "S0101_C03_004",
"15_to_19_year" = "S0101_C03_005",
"20_to_24_years" = "S0101_C03_006",
"25_to_29_years" = "S0101_C03_007",
"30_to_34_years" = "S0101_C03_008",
"35_to_39_years" = "S0101_C03_009",
"40_to_44_years" = "S0101_C03_010",
"45_to_49_years" = "S0101_C03_011",
"50_to_54_years" = "S0101_C03_012", 
"55_to_59_years" = "S0101_C03_013", 
"60_to_64 years" = "S0101_C03_014",
"65_to_69_years" = "S0101_C03_015",
"70_to_74_years" = "S0101_C03_016",
"75_to_79_years" = "S0101_C03_017",
"80_to_84_years" ="S0101_C03_018",
"85_years_and_over" = "S0101_C03_019")


total_pop_female_by_age <- c("Under_5_years" = "S0101_C05_002",
                           
                           "5_to_9_years" = "S0101_C05_003",
                           "10_to_14_years" = "S0101_C05_004",
                           "15_to_19_year" = "S0101_C05_005",
                           "20_to_24_years" = "S0101_C05_006",
                           "25_to_29_years" = "S0101_C05_007",
                           "30_to_34_years" = "S0101_C05_008",
                           "35_to_39_years" = "S0101_C05_009",
                           "40_to_44_years" = "S0101_C05_010",
                           "45_to_49_years" = "S0101_C05_011",
                           "50_to_54_years" = "S0101_C05_012", 
                           "55_to_59_years" = "S0101_C05_013", 
                           "60_to_64 years" = "S0101_C05_014",
                           "65_to_69_years" = "S0101_C05_015",
                           "70_to_74_years" = "S0101_C05_016",
                           "75_to_79_years" = "S0101_C05_017",
                           "80_to_84_years" ="S0101_C05_018",
                           "85_years_and_over" = "S0101_C05_019")



#multi year age group estimates and percentages

#step 1

#multi year age group estimates and percent. Change variable between estimate and percent 
multi_year_age_group_composition_percent <- map_dfr(years, ~get_acs(variables = age_groups_percent, year = .x, state = "MD",county = "Prince George's",geography = "county",geometry = FALSE),.id = "year") %>% select(-moe) %>% arrange(variable,NAME)


#Step 2: Convert Data From Long to Wide
multi_year_age_group_composition_percent <- multi_year_age_group_composition_percent %>% 
  spread(year,estimate,sep = "_")

#step 3 write csv file
write_csv(x = multi_year_age_group_composition_percent, path = "multi_year_age_group_composition_percent_v01.csv")


#get age by gender using subject profile variables

pop_male_by_age <- get_acs(variables = total_pop_male_by_age, year = 2019, state = "024", county = "033", geography = "county", geometry = FALSE, summary_var = "S0101_C03_001",survey = "acs5")

pop_male_by_age <- pop_male_by_age %>%  mutate(pop_male_by_age, percent = round(100*(estimate/summary_est),1))


pop_female_by_age <- get_acs(variables = total_pop_female_by_age, year = 2019, state = "024", county = "033", geography = "county", geometry = FALSE, summary_var = "S0101_C05_001")


pop_female_by_age <- mutate(pop_female_by_age, percent = round(100*(estimate/summary_est),1))

#export male csv file
write.csv(x = pop_male_by_age, file =  "pop_male_by_age_v01.csv")


#export female csv file
write.csv(x = pop_female_by_age, file =  "pop_female_by_age_v01.csv")

#get age group data estimates and percentages#
county_population_age_data <- get_acs(variables = age_groups_estimate, year = 2019, state = "MD",county = "Prince George's",geography = "county",geometry = FALSE, summary_var = "DP05_0001")


county_population_age_gender_male_p <- get_acs(variables ="S0101_C03_004":"S0101_C03_007" , year = 2019, state = "MD",county = "Prince George's",geography = "county",geometry = FALSE)


county_population_age_percent_data <- get_acs(variables = age_groups_percent, year = 2019, state = "MD",county = "Prince George's",geography = "county",geometry = FALSE, summary_var = "DP05_0001")


#get age group data estimates and percentages at the tract level
county_population_age_data_tract <- get_acs(variables = age_groups_estimate, year = 2019, state = "MD",county = "Prince George's",geography = "tract",geometry = TRUE, output = "wide", summary_var = "DP05_0001")

county_population_age_p_data_tract <- get_acs(variables = age_groups_percent, year = 2019, state = "MD",county = "Prince George's",geography = "tract",geometry = TRUE, output = "wide", summary_var = "DP05_0001")


####3. Prince George's County Median Income and US Median Income####

income <- c("Less_than_10,000" = "DP03_0052",
            "$10,000 to $14,999" = "DP03_0053",
            "$15,000 to $24,999" ="DP03_0054",
            "$25,000 to $34,999" = "DP03_0055",
            "$35,000 to $49,999" ="DP03_0056",
            "$50,000 to $74,999" = "DP03_0057",
            "$75,000 to $99,999" = "DP03_0058",
            "$100,000 to $149,999" = "DP03_0059",
            "$150,000 to $199,999" = "DP03_0060",
            "$200,000 or more" = "DP03_0061")


income_p <- c("Less_than_10,000_p" = "DP03_0052P",
              "$10,000 to $14,999_p" = "DP03_0053P",
              "$15,000 to $24,999_p" ="DP03_0054P",
              "$25,000 to $34,999_p" = "DP03_0055P",
              "$35,000 to $49,999_p" ="DP03_0056P",
              "$50,000 to $74,999_p" = "DP03_0057P",
              "$75,000 to $99,999_p" = "DP03_0058P",
              "$100,000 to $149,999_p" = "DP03_0059P",
              "$150,000 to $199,999_" = "DP03_0060P",
              "$200,000 or more_p" = "DP03_0061P")



median_and_mean_income <- c("Median household income (dollars)" = "DP03_0062",
                            "Median household income (dollars)_p" = "DP03_0062P",
                            "Mean household income (dollars)" = "DP03_0063",
                            "Mean household income (dollars)_p" = "DP03_0063P")


#get income estimates and percentages#
income_estimates <- get_acs(variables = income, year = 2019, state = "MD",county = "Prince George's",geography = "county",geometry = FALSE)

income_estimates_percent_tract <- get_acs(variables = income_p, year = 2019, state = "MD",county = "Prince George's",geography = "county",geometry = TRUE)

#3.1 Multiyear Income Estimates####
multi_year_income_estimates <- map_dfr(years, ~get_acs(variables = income, year = .x, state = "MD",county = "Prince George's",geography = "county",geometry = FALSE),.id = "year") %>% select(-moe) %>% arrange(variable,NAME)


#Step 2: Convert Data From Long to Wide
multi_year_income_estimates  <- multi_year_income_estimates  %>% 
  spread(year,estimate,sep = "_")


#3.2 Multiyear Income Estimate Percents####
multi_year_income_estimates_percent <- map_dfr(years, ~get_acs(variables = income_p, year = .x, state = "MD",county = "Prince George's",geography = "county",geometry = FALSE),.id = "year") %>% select(-moe) %>% arrange(variable,NAME)


#Step 2: Convert Data From Long to Wide
multi_year_income_estimates_percent  <- multi_year_income_estimates_percent %>% 
  spread(year,estimate,sep = "_")

write_csv(multi_year_income_estimates_percent,path = "multi_year_income_estimates_percent_v01.csv")



#get income estimates and percentages at census tract#
income_estimates_tract <- get_acs(variables = income, year = 2019, state = "MD",county = "Prince George's",geography = "tract",geometry = TRUE, output = "wide")

income_estimates_percent_tract <- get_acs(variables = income_p, year = 2019, state = "MD",county = "Prince George's",geography = "tract",geometry = TRUE, output = "wide")


#3.3 Multiyear/ Median####

#multi year median and means
multi_year_mean_median_income <- map_dfr(years, ~get_acs(variables = median_and_mean_income, year = .x, state = "MD",county = "Prince George's",geography = "county",geometry = FALSE),.id = "year") %>% select(-moe) %>% arrange(variable,NAME)


#Step 2: Convert Data From Long to Wide
multi_year_mean_median_income  <- multi_year_mean_median_income  %>% 
  spread(year,estimate,sep = "_") %>%  na.omit(multi_year_mean_median_income)


#Step 3 write file to folder as csv file
write_csv(multi_year_mean_median_income,path = "multi_year_mean_median_income_v01.csv")



#Get values for single year 
median_mean_income_estimates <- get_acs(variables = median_and_mean_income, year = 2019, state = "MD",county = "Prince George's",geography = "county",geometry = FALSE) %>% na.omit(median_mean_income_estimates)

median_mean_income_estimates_state_level <- get_acs(variables = median_and_mean_income, year = 2019, state = "MD",geography = "state",geometry = FALSE)



####4. Export datasets as csv files or geopackage ####

write_csv(x = county_population_age_data, path = "county_population_age_data.csv",col_name = TRUE)

st_write(obj = county_population_age_p_data_tract,dsn = "county_population_age_p_data_tract_v03.gpkg")


#5. Get Population Estimate Data for 2015 - 2019####
vignette(package = "tidycensus")

pgco_estimates <- get_estimates(geography ="county",year = 2019, state = 24,county = 033,
product = "characteristics", breakdown = c("SEX","AGEGROUP","RACE","HISP"), breakdown_labels = TRUE)

pgco_pop_estimates <- get_estimates(geography ="county",year = 2019, state = 24,county = 033,
product = "population", breakdown_labels = TRUE, time_series = TRUE)

pgco_pop_estimates <- mutate(pgco_pop_estimates,YEAR = case_when(DATE == 1 ~ "2010",
                                           DATE == 2 ~"2010",
                                           DATE == 3 ~"2010",
                                           DATE == 4 ~"2011",
                                           DATE == 5 ~"2012",
                                           DATE == 6 ~"2013",
                                           DATE == 7 ~"2014",
                                           DATE == 8 ~"2015",
                                           DATE == 9 ~"2016",
                                           DATE == 10 ~"2017",
                                           DATE == 11 ~"2018",
                                           DATE == 12 ~"2019", 
                                           TRUE ~ "Unknown Year")) %>% 
                                          filter(DATE != c(1,2)) %>% 
                                          select(-DATE)
# create percent change population value
pgco_multi_year_pop_estimates <- filter(pgco_pop_estimates,variable != "DENSITY") %>% 
mutate(pct_change = (value - lag(value))/lag(value)*100)

#round population estimates
pgco_multi_year_pop_estimates$pct_change <- round(pgco_multi_year_pop_estimates$pct_change,digits = 2)

#filter out population density
pgco_multi_year_pop_density <- filter(pgco_pop_estimates,variable != "POP")




pgco_components_estimates <- get_estimates(geography ="county",year = 2019, state = 24,county = 033,
product = "components", breakdown_labels = TRUE)

pgco_housing_estimates <- get_estimates(geography ="county",year = 2019, state = 24,county = 033,
product = "housing", breakdown_labels = TRUE, time_series = TRUE)


#write data to local file
write_csv(x = pgco_multi_year_pop_estimates,path = "pgco_multi_year_pop_estimates_v01.csv")








# path to folder that holds multiple .csv files
pop_estimates_folder <-list.files(path = "C:\\Users\\chauncey.robbs\\Documents\\Projects\\LPPRP 2022\\02 Data\\Chapter 1\\population_estimates",pattern = "*.csv", full.names = TRUE) 

#reads the list of files and applies the read_csv function to them
pop_estimate_files <- lapply(pop_estimates_folder,read_csv)

#subsetting the pop_estimate_files
agegroup <- pop_estimate_files[[1]]


# filtering the data to just include annual population counts. add in population years with mutate
agegroup_updated <- agegroup %>% filter(str_detect(AGEGROUP, "^All")) %>% arrange(DATE) %>% mutate(YEAR = 2010:2021) 


#add population change to the agegroup_update. use lag function to get percent change
agegroup_updated <- agegroup_updated %>% mutate(PCT_CHANGE = 100*((value - lag(value))/lag(value)))


# ^ matches the start of a string
agegroup_updated %>% ggplot(aes(x = YEAR, y = value))+
  geom_segment(aes(x = YEAR, xend = YEAR, y = 0, yend = value))+
  geom_point(color  = "red")+
  geom_line()+
  scale_x_continuous(aes(shape =3),breaks = agegroup_updated$YEAR, labels = agegroup_updated$YEAR)+
  scale_y_log10()+
  theme_minimal()+
  ylab(label = "Population")+
  theme(panel.grid =  element_blank(),axis.title.x = element_blank())
  

agegroup_updated %>% ggplot(aes(x = YEAR, y = PCT_CHANGE))+
  geom_col(aes(fill = PCT_CHANGE), fill = "orange")+
  theme_minimal()+
  xlab(label = NULL)+
  ylab(label = "Percent Change")+
  scale_x_continuous(breaks = agegroup_updated$YEAR, labels = agegroup_updated$YEAR)+
  theme_minimal()


agegroup_updated %>% ggplot(aes(x = value))+
  geom_dotplot(method = "histdot" ,binwidth = 1500)+
  theme_minimal()+
  xlab(label = NULL)+
  ylab(label = "Percent Change")+
  theme(panel.grid = element_blank())


sex <- classInt::classIntervals(sex_age_total_pop_transform$sex_age_total_popE,n = 5, style = "pretty")

classInt::classIntervals(agegroup_updated$value,n = 5, style = "pretty")


#6. Population Estimates 2010 to 2019####
vignette("tidycensus")

pop_years <- c(2010:2019)

pop_estimates <- get_estimates(geography = "county",product = "population",state = "Maryland",county = "Prince George's County",year = 2019,time_series = TRUE,output = "wide")



prince_georges_county_population_2010_2019<- pop_estimates %>% slice(-c(1,2)) %>% mutate(years = replace(x = DATE, values = c(2010:2019))) %>% rename(geoid=GEOID,name = NAME,pop=POP,density = DENSITY,date = DATE)

#export file to a csv
write_csv(x = prince_georges_county_population_2010_2019,file ="pgco_population_estimates_2010_2019.csv")


# visualize the data quicky using geom point 
#need to add line segements to the plot
prince_georges_county_population_2010_2019 %>% ggplot(mapping = aes(x = years,y = pop))+
  geom_point(stat = "identity")+coord_flip()+scale_y_continuous(labels = comma)


