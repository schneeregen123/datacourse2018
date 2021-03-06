#------------------------------
# R95T Project
# author: Marc Dietz, Mirjam Scheller, David Mennekes
# Januar 2018
#------------------------------

#load and install

#require(tidyverse)
#require(lubridate)
#read your data as read_delim ein -> gives you a tuple!!

#------------------------------
#load your Data
#------------------------------

#load your data probably! first column should be a date format!
#data <- read_delim("Documents/UNI/MA_Hydrologie/04_datamanagement/topic04/BW_Climate_1977_2016.txt",  "\t", escape_double = FALSE, col_types = cols(date = col_date(format = "%Y-%m-%d")),  trim_ws = TRUE)


#data should look like this, important are the types per columns! col names have to be like in the example

#   date    id   RSK  RSKF SHK_TAG   TNK   TMK   TXK   UPM
#   <date> <int> <dbl> <int>   <int> <dbl> <dbl> <dbl> <int>
#   1 1977-01-01   257   4.9     1       0  -0.1   3.0   4.4    98
#   2 1977-01-02   257   0.3     1       0   1.5   4.6   6.7    81
#   3 1977-01-03   257   0.0     1       0   0.5   1.9   4.9    93
#   4 1977-01-04   257   0.0     0       0  -2.3  -0.2   3.0    94
#   5 1977-01-05   257   0.0     0       0  -0.6  -0.2   0.8    96
#   6 1977-01-06   257   0.0     7       0  -1.9  -0.4   1.7    87




#------------------------------
#the function r95t
#------------------------------


#time.start and time.end define the time interval for the 95 percentile reference. type a year! e. g. 1999 year should be part of the dataframe 
#if nothing is choosen time.start is the first year and time.end is 20 years later
R95T <- function(data=data, station.id = station.id, time.start = F, time.end = F){

    
    # Packages
    
    require(tidyverse)
    require(lubridate)
    
    
    #select data by your station
    
    d_station <- data %>% filter(id == station.id) %>% mutate(year = year(date))
    
    # cut off years where are more than 60 NA values
    #------------------------------
    
    #check where are NA in years
    del.years <- d_station  %>% #new col with year
        group_by(year) %>% 
        summarise(na.year = sum(is.na(RSK))) %>% 
        filter(na.year > 60) %>% 
        select(year) %>% unlist() %>% as.vector() #vector with years na < 60
    

    d_new <- d_station %>% filter( !year %in% del.years)
    
    #------------------------------
    # check data
    #------------------------------
    
    # check for at least 10 years for each time series
    err <- TRUE
    #1977-1996
    if (I(unlist(as.numeric(1997- d_new[1, "year"]))- I (0 + try(length(which(del.years >=1977 && del.years<=1996))))) < 10) {
        err <- FALSE
    }
    
    #1996-2016
    
    if (I(unlist(as.numeric(2017- 1997))- I (0 + try(length(which(del.years>=1997 && del.years<=2016))))) < 10) {
        err <- FALSE
    }
    
    
    #------------------------------
    # calculate
    #------------------------------
    # q95
    # Get data where rain event per day is >= 1mm
    d_1mm <- d_new %>% filter(RSK >= 1)
    
    #take the years as reference
    if(time.start == F){
        first.year <- as.numeric(unlist(d_new[1,"year"]))
    } else {
        first.year <- time.start
    }
    
    if(time.end == F){
        p.end <- I(first.year+20)
    } else {
        p.end <- time.end
    }
    
    q95 <- d_1mm %>% filter(year>= first.year & year < p.end) %>% summarise( quantile(RSK, 0.95, na.rm = T)) %>% 
        unlist() %>% as.numeric()
    
    
    # % days with more than q95 rain / for total days with rain (>1mm) 
    #per each year
    
    days.1mm <- d_1mm %>% group_by(year) %>% 
        summarise(count.1mm = length(RSK))
    
    days.q95 <- d_new %>%  filter(RSK >= q95) %>% group_by(year) %>% 
        summarise(count.q95 = length(RSK)) 
    
    days.percent <- data.frame(year = unique(d_station$year))
    days.percent <- merge(data.frame(year = unique(d_station$year)), days.1mm, by = "year", all.x = T)
    days.percent <- merge(days.percent, days.q95, by = "year", all.x = T) #for getting all years as reference
    days.percent2 <- days.percent %>% mutate(percent = count.q95 /count.1mm * 100) %>% 
        select(year, percent)
    
    
    #------------------------------
    #Climate change????
    #------------------------------
    
    #rate of the change
    mean.1997_2016 <- days.percent2 %>% filter(year>=1997, year <= 2016) %>%  summarise(mean(percent , na.rm = T))
    
    
    mean.1977_1996 <- days.percent2 %>% filter(year>=1977, year <= 1996) %>%  summarise(mean(percent, na.rm = T))
    
    rate.change <- mean.1997_2016 / mean.1977_1996 * 100 #thats the result :D
    
    
    mean.all <- mean(days.percent2$percent, na.rm = T) #thats the other result :)
    
    xxx <- "WARNING: for Index value 2. one of your periods has less than 10 years of data. The result might be not valid!!"
    if(err == F){
        errors <- xxx
        rate.change <- NA
    }  else {
        errors <- "ciao!"
    }
    if(is.nan(mean.all)) {errors <- c(xxx, "WARNING: there is no data!")}
    
    last.year <- d_new[nrow(d_new), "year"]
    
    return(c("mean.all" = mean.all, "rate.change" = rate.change, paste("the mean R95T from", first.year, "to", last.year, "is: ", round(mean.all,2), "% --- and the change of average from 1997 - 2016 related to average from 1977 - 1996 is", round(rate.change,2),"%"), errors))
    
}
