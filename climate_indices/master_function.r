ID <- c(257, 259, 755, 1197, 1224, 1346, 1602, 2074, 2638, 2712, 2814, 3257, 3402, 3761, 3927, 4094, 4175, 4300, 4703, 4887, 4928, 4931, 5229, 5275, 1443, 1468, 4169, 5664, 5731, 5906)
#problematic ID's: 1443, 1468, 4169, 5664, 5731, 5906

path <- "K:/Hydrologie/Data Collection, Data Storage, Data Management/ClimateChange/BW_Climate_1977_2016/BW_Climate_1977_2016.txt"
raw_data <- read_tsv(path)



complete_table <- do_it(ID = ID, path = path, raw_data = raw_data)

do_it <- function(ID, path, raw_data){
  require(tidyverse)
  require(tidyr)
  require(lubridate)
  require(dplyr)
  require(readr)
  require(zoo)
  require(ggplot2)
  
  # source("climate_indices/Fd.R")
  # Fd <- fun_frostdays
  # source("climate_indices/ETR.R")
  # source("climate_indices/GSL.R")
  # source("climate_indices/HWDI.R")
  # source("climate_indices/Tn90.R")
  # source("climate_indices/R10.R")
  # source("climate_indices/CDD.R")
  # source("climate_indices/R5d.R")
  # R5d <- calc_R5d
  # source("climate_indices/SDII.R")
  # source("climate_indices/R95T.R")
  # source("climate_indices/collection_of_all_functions.R")
  # R5d <- calc_R5d
  
  result_total <- tibble(ID = ID, 
                  average_Fd = NA, average_ETR = NA, average_GSL = NA, average_HWDI = NA, average_Tn90 = NA, average_R10 = NA, average_CDD = NA, average_R5d = NA, average_SDII = NA, average_R95T = NA,
                  change_Fd = NA, change_ETR = NA, change_GSL = NA, change_HWDI = NA, change_Tn90 = NA, change_R10 = NA, change_CDD = NA, change_R5d = NA, change_SDII = NA, change_R95T = NA)
    
    
  for (i in 1:length(ID)) {
    result_Fd <- Fd(bw = raw_data, station_id = ID[i])
      result_total$average_Fd <- result_Fd[[1]]
      result_total$change_Fd <- result_Fd[[2]]
      
    result_ETR <- ETR(x = raw_data, ID = 257)
      result_total$average_ETR[i] <- result_ETR$average_ETR
      result_total$change_ETR[i] <- result_ETR$ABC
    
    #result_GSL <- GSL()
      #requires single vectors date & temp
    
    #result_HWDI <- HWDI(raw_data, 257)
      #arguments imply differing number of rows: 0, 14610 
      # result_total$average_HWDI[i] <- result_HWDI[[1]]
      # result_total$change_HWDI[i] <- result_HWDI[[2]]
    
    #result_Tn90 <- Tn90()
      #requires single vectors date and tmin
    
    result_R10 <- R10(bw = raw_data, id_station = ID[i])
      result_total$average_R10[i] <- result_R10[[2]]
      result_total$change_R10[i] <- result_R10[[3]]
    
    result_CDD <- CDD(x = raw_data, ID = ID[i])
      result_total$average_CDD[i] <- result_CDD$average
      result_total$change_CDD[i] <- result_CDD$rate.of.change
    
    result_R5d <- R5d(bw = raw_data, idstation = ID[i])
      result_total$average_R5d[i] <- result_R5d[[3]]
      result_total$change_R5d[i] <- result_R5d[[2]]
    
    result_SDII <- SDII(data = raw_data, ID = ID[i])
      result_total$average_SDII[i] <- result_SDII$total_mean
      result_total$change_SDII[i] <- result_SDII$rate_change
    
    result_R95T <- R95T(data = raw_data, station.id = ID[i])
      result_total$average_R95T[i] <- result_R95T$mean.all
      result_total$change_R95T[i] <- result_R95T$`rate.change.mean(percent, na.rm = T)`
  }
  
  return(result_total)
}
