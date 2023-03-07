# Load functions and libraries ####
source("Functions/read_FRAM_data.R")
source("Functions/read_inriver_data.R")
source("Functions/calc_ER_HR.R")
source("Functions/generate_fishery_calendars.R")
library('gt')
library('tidyverse')
library('ggplot2')

# Set directories and file names for FRAM input data and inriver data ####
FRAM_directory <- "Data/FRAM/2022/"
FRAM_filename <- "Coho2229_STTNOF_041222.xlsx"

inriver_directory <- "Data/" 
inriver_filename <- "Coho_inriver_harvest_accounting_input_2022_post-season_7Mar2023.xlsx"

# Read FRAM Input file ####
# This runs slow because it has to parse non-tabular data from large FRAM files
FRAM_data<-read_FRAM_data(FRAM_directory = FRAM_directory,
                          FRAM_filename = FRAM_filename)

# Generate fishery sport and commercial fishery calendars for fishery inputs on template
sport_calendar<-generate_fishery_calendars(fishery_year="2022")$Sport_Calendar %>% 
  filter(Date >= "2022-08-01" & DayOfWeek == 'Mon')

commercial_calendar<-generate_fishery_calendars(fishery_year="2022")$Commercial_calendar %>% 
  filter(Date >= "2022-08-01" & DayOfWeek == 'Mon')



# Read in-river fishery handle data and join to look up tables ####
CR_data<-read_inriver_data(inriver_directory = inriver_directory, 
                           inriver_filename = inriver_filename,
                           fishery_year=2022)

# Calculate exploitation rates and harvest rates ####
calcs<-calc_ER_HR(FRAM_object = FRAM_data, 
                  CR_data_object = CR_data)


# Generate data summaries ####
# Handle summary table by fishery #####
handle_summary<-calcs %>% 
  group_by(Fishery_area, Gear_category, Gear_name) %>% 
  summarize(Kept_Clipped_Early = sum(Kept_clipped_adults_early, na.rm = T), 
            Kept_Clipped_Late = sum(Kept_clipped_adults_late, na.rm = T), 
            Kept_UnClipped_Early = sum(Kept_unclipped_adults_early, na.rm = T), 
            Kept_UnClipped_Late = sum(Kept_unclipped_adults_late, na.rm = T),
            Released_Early = sum(Released_adults_early, na.rm = T), 
            Released_Late = sum(Released_adults_late, na.rm = T), 
            Release_Morts_Early = sum(Released_mortality_adults_early, na.rm = T), 
            Release_Morts_Late = sum(Released_mortality_adults_late, na.rm = T)) %>% 
  mutate_if(is.numeric, round, 0)

# Print handle summary table for Rmarkdown
gt(handle_summary, rowname_col = "Fishery_area", groupname_col = c("Gear_category", "Gear_name")) %>% 
  summary_rows(groups = T, fns=list(Subtotal = "sum")) %>%
  grand_summary_rows(fns = list(Grand_Total = "sum"))


# Exploitation rate table by fishery area            
tab2<-calcs %>% 
  group_by(Fishery_area) %>% 
  summarize(ER_Early = sum(ER_early), ER_Late = sum(ER_late)) %>% 
  mutate_if(is.numeric, percent, accuracy = 0.01)


  #HR_Early = percent(sum(HR_marked_early), accuracy = 0.01), HR_Late = percent(sum(HR_marked_late), accuracy = 0.01), 
  gt(tab2, rowname_col = "Fishery_area") %>% 
    summary_rows(groups = T, fns=list(Subtotal = "sum")) %>%
    grand_summary_rows(fns = list(Grand_Total = "sum"))


# Generate summary plots ####
#colors<-c('red', 'blue', 'green', 'purple', 'grey', 'black')
calcs %>%
  ggplot(aes(x=Stat_week_commercial, y=ER_early, fill = Fishery_area)) +  
  geom_col() +
  labs(title = "Commerical ER by stat week")
  
    

