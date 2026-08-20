# Inspect the YSI profiles, summarize the data, and export clean rounded csv files

# 00 Set Up R Environment and write functions ---------------------------------------------
# Mira was here!

# Source libraries and global functions 
source(here::here("functions/00_libraries.R"))
# source("functions/00_helper_functions.R")
source(here::here("functions/01_ysi_profile.R"))

# Save the parameters you need later on 
loc_lat <- 40.29231
loc_long <- -105.6562
loc_alt <- 3105
gl4_lat <- 40.0551844
gl4_long <- -105.620445
gl4_alt <- 3560

# # Write plotting function 
Round_Plot_YSI_FUNC <- function(ysi_profile, round_to_nearest ){
  ysi_profile %>%
    mutate(depth_m=round(depth_m/ round_to_nearest )* round_to_nearest ) %>% #round to the nearest 0.5
    group_by(depth_m, parameter, lake) %>%
    mutate(value = median(value, na.rm=TRUE)) %>%
    mutate(month=month(date_time)) %>%
    filter(!parameter %in% c("barometer_mmHg","cond_spec_uScm")) %>%
    ggplot(aes(x=value, y=depth_m, color=parameter))+
    geom_point()+
    scale_y_reverse()+
    facet_wrap(parameter~., scales="free_x", nrow = 2)+
    labs(title=paste(unique(ysi_profile$lake),unique(ysi_profile$date)))
}

# Write a function that rounds, summarizes by depth, and pivots the data to wide format 
OTI_YSI_FUNC <- function(ysi_profile, round_to_nearest){

  # Round to the nearest depth (based on what you decided looking at the plots )
  ysi_profile_rounded <- ysi_profile %>%
    mutate(depth_m=round(depth_m/ round_to_nearest )* round_to_nearest )

  # Save the date time that the profile was collected as date
  ysi_profile_rounded$date <- ysi_profile_rounded$date_time[1] # funky because we want to keep the date-time that the profile was taken but we don't want to summarize by time bc it would replicate for each second or minute and some of our profiles cover a lot of time

  # Summarize: take median parameter value for each unique combination of lake, site, date, depth
  ysi_profile_summarized <- ysi_profile_rounded %>% #round to the nearest 0.5
    group_by(lake, site, date, depth_m, parameter) %>% # gather everythinf into groups correspond to a unique combination of lake, date, depth
    summarise(value = median(value, na.rm = TRUE), .groups = "drop") # take the median of each group

  # Pivot the resulting table from long format to wide format
  ysi_wide <- ysi_profile_summarized %>%
    select(lake, site, date, depth_m, parameter, value) %>%  # keep relevant columns
    pivot_wider(
      names_from = parameter,   # each unique parameter becomes its own column
      values_from = value       # fill those columns with the 'value' data
    )

  # Format columns and column names

  # some columns just need to be renamed
  names(ysi_wide)[names(ysi_wide) == "lake"] <- "lakeID"
  names(ysi_wide)[names(ysi_wide) == "temp_C"] <- "temp_degC"
  names(ysi_wide)[names(ysi_wide) == "do_mgL"] <- "doConcentration_mgpL"
  names(ysi_wide)[names(ysi_wide) == "do_percent"] <- "doSaturation_percent"
  names(ysi_wide)[names(ysi_wide) == "cond_spec_uScm"] <- "specificConductivity_uSpcm"

  # Format dates to be compatable
  ysi_wide$date_yyyy.mm.dd <- as.Date(ysi_wide$date)
  ysi_wide$time_hhmmss <- format(ysi_wide$date, "%H:%M:%S")

  # Convert the units of barometric pressure to tbe same as the rest of the OTI Team
  ysi_wide$waterPressure_barA <- ysi_wide$barometer_mmHg * 0.0013322 # we measure barometric pressure as barometer_mmHg, for "water pressure" (under water rather than in air handheld) Dave wanrs barA as the units

  # Some parameters we don't collect on our instrument so give them a column with explicit NAs
  ysi_wide$turbidity_FNU <- NA # explicit column of NAs for data that we do not have
  ysi_wide$salinity_psu <- NA # explicit column of NAs for data that we do not have
  ysi_wide$tds_mgpL <- NA # explicit column of NAs for data that we do not have
  ysi_wide$barometerAirHandheld_mbars <- NA # explicit column of NAs for data that we do not have

  # Set lat long and altitude based on lake
  ysi_wide$latitude <- ifelse(ysi_wide$lakeID == "GL4", gl4_lat,
                              ifelse(ysi_wide$lakeID == "LOC", loc_lat, NA))
  ysi_wide$longitude <- ifelse(ysi_wide$lakeID == "GL4", gl4_long,
                               ifelse(ysi_wide$lakeID == "LOC", loc_long, NA))
  ysi_wide$altitude_m <- ifelse(ysi_wide$lakeID == "GL4", gl4_alt,
                                ifelse(ysi_wide$lakeID == "LOC", loc_alt, NA))

  # We have some timepoints for the loch where we have CHLA and PHYC but other time points when we don't. Set it up so that if we have data it populates and if not the column gets explocot NAs
  ysi_wide <- ysi_wide %>% mutate(chlorophyll_RFU  = if ("chla_RFU" %in% names(.)) chla_RFU else NA) # take ysi_wide and make a new column called "chlorophyll_RFU" (what Dave wants this called), if the data frame includes a column named "chla_RFU" (what we name that column), then use the data from that column. If there is no column with that name (if we don't have that data) then fill the column with NAs
  ysi_wide <- ysi_wide %>% mutate(phycocyaninBGA_RFU  = if ("phycoC_RFUU" %in% names(.)) phycoC_RFU else NA)
  ysi_wide <- ysi_wide %>% mutate(pH  = if ("pH" %in% names(.)) pH else NA) # also for some reason some timepoints with no pH and no orp
  ysi_wide <- ysi_wide %>% mutate(orp_mV = if ("orp_mV" %in% names(.)) orp_mV else NA)


  # Put all together into one nice formatted dataframe
  ysi_clean <- subset(ysi_wide, select = c("lakeID" , "date_yyyy.mm.dd", "time_hhmmss", "depth_m", "temp_degC", "doConcentration_mgpL",
                                           "doSaturation_percent", "chlorophyll_RFU", "phycocyaninBGA_RFU", "turbidity_FNU", "pH", "orp_mV",
                                           "specificConductivity_uSpcm", "salinity_psu", "tds_mgpL", "waterPressure_barA", "latitude",
                                           "longitude", "altitude_m", "barometerAirHandheld_mbars" ))

  return(ysi_clean)
}

# 01 Load and Process profiles ---------------------------------------------

# Load in Data 

# GL4 
GL4_dir <- here("~/OneDrive - UCB-O365/Research/Data/R/sensor_db/data/Sensors/YSI Pro DSS/GL4/raw")
GL4files <- dir_ls(GL4_dir, regexp = "\\.csv$", recurse = TRUE)     # Get all text files in the main directory and its subdirectories
# KAG 20250815 -- I downloaded the YSI profiles to my machine for easy access locally. For days with multiple YSI profiles from different ice holes I took only the deepest profile 
GL4files <- GL4files[str_detect(GL4files, "Zmax")]  # Only look at the "Zmax" files
length(GL4files) #check how many files you have 

# LOC 
LOC_dir <- here("~/OneDrive - UCB-O365/Research/Data/R/sensor_db/data/Sensors/YSI Pro DSS/LOC/raw/zmax")
LOCfiles <- dir_ls(LOC_dir, regexp = "\\.csv$", recurse = TRUE) # Get all text files in the main directory and its subdirectories
LOCfiles <- LOCfiles[str_detect(LOCfiles, "Zmax")]     # Only look at the "Zmax" files
length(LOCfiles) #check how many files you have 


# Process  YSI profiles 

GL4files_processed <- lapply(GL4files, process_ysi)
LOCfiles_processed <- lapply(LOCfiles, process_ysi)

# 02 Plot, format, and export  ---------------------------------------------

#GL4 -- 18 profiles 
setwd("~/OneDrive - UCB-O365/Research/Data/R/sensor_db/data/Sensors/YSI Pro DSS/GL4/export")

Round_Plot_YSI_FUNC(GL4files_processed[[1]], 0.5) # Check the plot 
GL4_2024_06_27 <- OTI_YSI_FUNC(GL4files_processed[[1]], 0.5) #round, summarize, pivot, and format the data 
write_csv(GL4_2024_06_27, "GL4_2024_06_27_profile.csv") # save the output 

Round_Plot_YSI_FUNC(GL4files_processed[[2]], 0.5)
GL4_2024_07_23 <- OTI_YSI_FUNC(GL4files_processed[[2]], 0.5)
write_csv( GL4_2024_07_23, "GL4_2024_07_23_profile.csv")

Round_Plot_YSI_FUNC(GL4files_processed[[3]], 0.25) # good at 0.25
GL4_2024_08_22 <- OTI_YSI_FUNC(GL4files_processed[[3]], 0.5)
write_csv( GL4_2024_08_22, "GL4_2024_08_22_profile.csv")

Round_Plot_YSI_FUNC(GL4files_processed[[4]], 0.25) # good at 0.25
GL4_2024_09_26 <- OTI_YSI_FUNC(GL4files_processed[[4]], 0.25)
write_csv( GL4_2024_09_26, "GL4_2024_09_26_profile.csv")

Round_Plot_YSI_FUNC(GL4files_processed[[5]], 0.25) # only one data point at 0.25 but I think still worth saving 
GL4_2024_10_22 <-  OTI_YSI_FUNC(GL4files_processed[[5]], 0.25) 
write_csv(GL4_2024_10_22, "GL4_2024_10_22_profile.csv")

Round_Plot_YSI_FUNC(GL4files_processed[[6]], 0.25) 
GL4_2024_12_03 <-  OTI_YSI_FUNC(GL4files_processed[[6]], 0.25) 
write_csv(GL4_2024_12_03, "GL4_2024_12_03_profile.csv")

Round_Plot_YSI_FUNC(GL4files_processed[[7]], 0.25)
GL4_2025_01_28 <- OTI_YSI_FUNC(GL4files_processed[[7]], 0.25)
write_csv( GL4_2025_01_28, "GL4_2025_01_28_profile.csv")

Round_Plot_YSI_FUNC(GL4files_processed[[8]], 0.25)+ggtitle("GL4 2024-02-27")
GL4_2025_02_27 <- OTI_YSI_FUNC(GL4files_processed[[8]], 0.25)
write_csv(GL4_2025_02_27, "GL4_2025_02_27_profile.csv")

Round_Plot_YSI_FUNC(GL4files_processed[[9]], 0.25)+ggtitle("GL4 2024-03-20")
GL4_2025_03_20 <- OTI_YSI_FUNC(GL4files_processed[[9]], 0.25)
write_csv(GL4_2025_03_20, "GL4_2025_03_20_profile.csv")

Round_Plot_YSI_FUNC(GL4files_processed[[10]], 0.25)
GL4_2025_04_22 <- OTI_YSI_FUNC(GL4files_processed[[10]], 0.25)
write_csv(GL4_2025_04_22, "GL4_2025_04_22_profile.csv" )

Round_Plot_YSI_FUNC(GL4files_processed[[11]], 0.25)
GL4_2025_07_01 <- OTI_YSI_FUNC(GL4files_processed[[11]], 0.25)
write_csv(GL4_2025_07_01,"GL4_2025_07_01_profile.csv" )

Round_Plot_YSI_FUNC(GL4files_processed[[12]] %>% filter(depth_m<13), 0.25)+ggtitle("GL4 2025-07-23")
GL4_2025_07_23 <- OTI_YSI_FUNC(GL4files_processed[[12]], 0.25)
write_csv(GL4_2025_07_23, "GL4_2025_07_23_profile.csv")

Round_Plot_YSI_FUNC(GL4files_processed[[13]], 0.5)
GL4_2025_08_12 <- OTI_YSI_FUNC(GL4files_processed[[13]], 0.5)
write_csv(GL4_2025_08_12, "GL4_2025_08_12_profile.csv")

Round_Plot_YSI_FUNC(GL4files_processed[[14]], 0.5)
GL4_2025_09_04 <- OTI_YSI_FUNC(GL4files_processed[[14]], 0.5)
write_csv(GL4_2025_09_04, "GL4_2025_09_04_profile.csv")

Round_Plot_YSI_FUNC(GL4files_processed[[15]]%>% filter(depth_m<12.5), 0.25)
GL4_2025_12_16 <- OTI_YSI_FUNC(GL4files_processed[[15]], 0.25)
write_csv(GL4_2025_12_16, "GL4_2025_12_16_profile.csv")

Round_Plot_YSI_FUNC(GL4files_processed[[16]], 0.25)
GL4_2026_01_15 <- OTI_YSI_FUNC(GL4files_processed[[16]], 0.25) %>%
  filter(depth_m < 13.25) #IAO - I think the sensor may have hit sediment here, but NA for sampling zMax on field sheet
write_csv(GL4_2026_01_15, "GL4_2026_01_15_profile.csv")

Round_Plot_YSI_FUNC(GL4files_processed[[17]], 0.25)
GL4_2026_02_10 <- OTI_YSI_FUNC(GL4files_processed[[17]], 0.25) %>%
  filter(depth_m < 12.6) #max depth at sampling location = 13.3 and ice depth = 0.7
write_csv(GL4_2026_02_10, "GL4_2026_02_10_profile.csv")

Round_Plot_YSI_FUNC(GL4files_processed[[18]] %>% filter(depth_m < 12.25), 1)
GL4_2026_03_19 <- OTI_YSI_FUNC(GL4files_processed[[18]], 0.25) %>%
  filter(depth_m < 12.25) #max depth at sampling location = 13.0m and ice depth = 0.75m
write_csv(GL4_2026_03_19, "GL4_2026_03_19_profile.csv") #This profile is a little wobbly.

## April and May 2026 were shore samples


# LOC --- 30 files
setwd("~/OneDrive - UCB-O365/Research/Data/R/sensor_db/data/Sensors/YSI Pro DSS/LOC/export")


# Round_Plot_YSI_FUNC(LOCfiles_processed[[1]], 0.25)
# LOC_2023_12_05 <-  OTI_YSI_FUNC(LOCfiles_processed[[1]], 0.25)
# write_csv(LOC_2023_12_05, "LOC_2023_12_05_profile.csv")
# 
# Round_Plot_YSI_FUNC(LOCfiles_processed[[2]], 0.1) 
# LOC_2024_01_30 <-  OTI_YSI_FUNC(LOCfiles_processed[[2]], 0.1)
# write_csv(LOC_2024_01_30, "LOC_2024_01_30_profile.csv" )
# 
# Round_Plot_YSI_FUNC(LOCfiles_processed[[3]], 0.25)
# LOC_2024_02_06 <- OTI_YSI_FUNC(LOCfiles_processed[[3]], 0.25)
# write_csv(LOC_2024_02_06, "LOC_2024_02_06_profile.csv" )

Round_Plot_YSI_FUNC(LOCfiles_processed[[1]], 0.1)
LOC_2024_03_05 <- OTI_YSI_FUNC(LOCfiles_processed[[4]], 0.1)
write_csv(LOC_2024_03_05, "LOC_2024_03_05_profile.csv")

Round_Plot_YSI_FUNC(LOCfiles_processed[[2]], 0.25)
LOC_2024_04_23 <- OTI_YSI_FUNC(LOCfiles_processed[[2]], 0.25)
write_csv(LOC_2024_04_23, "LOC_2024_04_23_profile.csv")

Round_Plot_YSI_FUNC(LOCfiles_processed[[3]], 0.1)
LOC_2024_05_13 <-  OTI_YSI_FUNC(LOCfiles_processed[[3]], 0.1)
write_csv(LOC_2024_05_13, "LOC_2024_05_13_profile.csv")

Round_Plot_YSI_FUNC(LOCfiles_processed[[4]], 0.25)
LOC_2024_06_13 <-  OTI_YSI_FUNC(LOCfiles_processed[[4]], 0.25)
write_csv(LOC_2024_06_13, "LOC_2024_06_13_profile.csv")

Round_Plot_YSI_FUNC(LOCfiles_processed[[5]], 0.25)
LOC_2024_07_09 <- OTI_YSI_FUNC(LOCfiles_processed[[5]], 0.25)
write_csv(LOC_2024_07_09, "LOC_2024_07_09_profile.csv")

Round_Plot_YSI_FUNC(LOCfiles_processed[[6]], 0.25)
LOC_2024_09_24 <- OTI_YSI_FUNC(LOCfiles_processed[[6]], 0.25)
write_csv(LOC_2024_09_24, "LOC_2024_09_24_profile.csv")

# Round_Plot_YSI_FUNC(LOCfiles_processed[[7]], 0.1) 
# LOC_2024_10_25 <- OTI_YSI_FUNC(LOCfiles_processed[[7]], 0.25)
# write_csv(LOC_2024_10_25, "LOC_2024_10_25_profile.csv")

# Round_Plot_YSI_FUNC(LOCfiles_processed[[8]], 0.1)
# LOC_2024_11_21 <- OTI_YSI_FUNC(LOCfiles_processed[[11]], 0.25)
# write_csv(LOC_2024_11_21, "LOC_2024_11_21_profile.csv")

Round_Plot_YSI_FUNC(LOCfiles_processed[[7]], 0.1)
LOC_2024_12_12 <-  OTI_YSI_FUNC(LOCfiles_processed[[7]], 0.1)
write_csv(LOC_2024_12_12,"LOC_2024_12_12_profile.csv" )

Round_Plot_YSI_FUNC(LOCfiles_processed[[8]], 0.1) 
LOC_2025_01_16 <- OTI_YSI_FUNC(LOCfiles_processed[[8]], 0.1)
write_csv(LOC_2025_01_16, "LOC_2025_01_16_profile.csv")

Round_Plot_YSI_FUNC(LOCfiles_processed[[9]], 0.1) + ggtitle("LOC 2025-01-24")
LOC_2025_01_24 <- OTI_YSI_FUNC(LOCfiles_processed[[9]], 0.1)
write_csv(LOC_2025_01_24, "LOC_2025_01_24_profile.csv")

Round_Plot_YSI_FUNC(LOCfiles_processed[[10]], 0.1)
LOC_2025_02_11 <- OTI_YSI_FUNC(LOCfiles_processed[[10]], 0.1)
write_csv(LOC_2025_02_11, "LOC_2025_02_11_profile.csv" )

Round_Plot_YSI_FUNC(LOCfiles_processed[[11]], 0.25)
LOC_2025_02_25 <- OTI_YSI_FUNC(LOCfiles_processed[[11]], 0.25)
write_csv(LOC_2025_02_25, "LOC_2025_02_25_profile.csv")

Round_Plot_YSI_FUNC(LOCfiles_processed[[12]], 0.25) 
LOC_2025_03_11 <- OTI_YSI_FUNC(LOCfiles_processed[[12]], 0.25)
write_csv(LOC_2025_03_11,"LOC_2025_03_11_profile.csv" )

Round_Plot_YSI_FUNC(LOCfiles_processed[[13]], 0.25)  
LOC_2025_04_15 <- OTI_YSI_FUNC(LOCfiles_processed[[13]], 0.25)
write_csv(LOC_2025_04_15, "LOC_2025_04_15_profile.csv")

Round_Plot_YSI_FUNC(LOCfiles_processed[[14]], 0.25) 
LOC_2025_05_22 <- OTI_YSI_FUNC(LOCfiles_processed[[14]], 0.25)
write_csv(LOC_2025_05_22, "LOC_2025_05_22_profile.csv")

Round_Plot_YSI_FUNC(LOCfiles_processed[[15]], 0.25) 
LOC_2025_06_19 <-  OTI_YSI_FUNC(LOCfiles_processed[[15]], 0.25)
write_csv(LOC_2025_06_19, "LOC_2025_06_19_profile.csv")

Round_Plot_YSI_FUNC(LOCfiles_processed[[16]], 0.25)
LOC_2025_07_29 <- OTI_YSI_FUNC(LOCfiles_processed[[16]], 0.25)
write_csv(LOC_2025_07_29, "LOC_2025_07_29_profile.csv")

Round_Plot_YSI_FUNC(LOCfiles_processed[[17]], 0.25) + ggtitle("LOC 2025-08-20")
LOC_2025_08_20 <- OTI_YSI_FUNC(LOCfiles_processed[[17]], 0.25)
write_csv(LOC_2025_08_20, "LOC_2025_08_20_profile.csv")    

# COME BACK TO THIS IAO -- this was a synoptic day, not OTI
# Round_Plot_YSI_FUNC(LOCfiles_processed[[18]], 0.25)+ ggtitle("LOC 2025-08-22")
# LOC_2025_08_22 <- OTI_YSI_FUNC(LOCfiles_processed[[18]], 0.25)
# write_csv(LOC_2025_08_22, "LOC_2025_08_20_profile.csv")      

Round_Plot_YSI_FUNC(LOCfiles_processed[[21]] %>% filter(depth_m < 4.6), 0.25) + ggtitle("LOC 2025-09-12")
LOC_2025_09_12 <- OTI_YSI_FUNC(LOCfiles_processed[[17]], 0.25) %>% filter(depth_m < 4.6)
write_csv(LOC_2025_09_12, "LOC_2025_09_12_profile.csv")    

# 9/30 and November samples were shore samples

Round_Plot_YSI_FUNC(LOCfiles_processed[[22]] %>% filter(depth_m < 4.25), 0.25) + ggtitle("LOC 2025-09-12")
LOC_2025_12_10 <- OTI_YSI_FUNC(LOCfiles_processed[[17]], 0.25) %>% filter(depth_m < 4.25) #4.5m depth and 0.25 ice
write_csv(LOC_2025_12_10, "LOC_2025_12_10_profile.csv")    

Round_Plot_YSI_FUNC(LOCfiles_processed[[21]] %>% filter(depth_m<3.6), 0.25)
LOC_2026_01_08 <- OTI_YSI_FUNC(LOCfiles_processed[[21]]%>% filter(depth_m<3.6), 0.25)
write_csv(LOC_2026_01_08, "LOC_2026_01_08_profile.csv") #4m depth and 0.4m ice depth

Round_Plot_YSI_FUNC(LOCfiles_processed[[22]] %>% filter(depth_m<3), 0.25)
LOC_2026_02_05 <- OTI_YSI_FUNC(LOCfiles_processed[[22]]%>% filter(depth_m<3), 0.25)
write_csv(LOC_2026_02_05, "LOC_2026_02_05_profile.csv") #3.43 depth and 0.53m ice depth

Round_Plot_YSI_FUNC(LOCfiles_processed[[23]] %>% filter(depth_m<3.8), 0.25)
LOC_2026_03_05 <- OTI_YSI_FUNC(LOCfiles_processed[[23]]%>% filter(depth_m<3.8), 0.25)
write_csv(LOC_2026_03_05, "LOC_2026_03_05_profile.csv") #3.8 depth

# OPEN WATER
Round_Plot_YSI_FUNC(LOCfiles_processed[[24]] %>% filter(depth_m<3.75), 0.25)
LOC_2026_04_14 <- OTI_YSI_FUNC(LOCfiles_processed[[23]]%>% filter(depth_m<3.75), 0.25)
write_csv(LOC_2026_04_14, "LOC_2026_04_14_profile.csv") #3.8 depth

Round_Plot_YSI_FUNC(LOCfiles_processed[[25]] %>% filter(depth_m<4.7), 0.25)
LOC_2026_05_21 <- OTI_YSI_FUNC(LOCfiles_processed[[23]]%>% filter(depth_m<4.7), 0.25)
write_csv(LOC_2026_05_21, "LOC_2026_05_21_profile.csv") #No zmax noted, assuming max depth
