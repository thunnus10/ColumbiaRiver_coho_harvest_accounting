# This function generates calendars by stat weeks for sport and commercial fisheries.

generate_fishery_calendars<-function(fishery_year){ 
                                     #start_day_sport_week="Monday", 
                                     #start_day_commericial_week="Sunday"){
  if (!"tidyverse" %in% installed.packages()) install.packages("tidyverse")
  if (!"lubridate" %in% installed.packages()) install.packages("lubridate")
  library("tidyverse")
  library("lubridate")
  
  calendar_sport<-tibble(Date=seq(ymd(paste(fishery_year, "-01-01", sep="")), ymd(paste(fishery_year, "12-31", sep="")), by=1)) %>% 
    mutate(DayOfWeek = wday(Date, label=T, abbr=T), Stat_week_sport = isoweek(Date)+1)
  
  calendar_commercial<-tibble(Date=seq(ymd(paste(fishery_year, "-01-01", sep="")), ymd(paste(fishery_year, "12-31", sep="")), by=1)) %>% 
    mutate(DayOfWeek = wday(Date, label=T, abbr=T), Stat_week_commercial = isoweek(Date+1)+1)

  return(list(Sport_Calendar=calendar_sport, Commercial_calendar=calendar_commercial))
}