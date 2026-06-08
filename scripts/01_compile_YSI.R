# Inspect the YSI profiles, summarize the data, and export clean rounded csv files

# 00 Set Up R Environment and write functions ---------------------------------------------
# Mira was here!

    # Source libraries and global functions 
    source("functions/00_libraries.R")
    # source("functions/00_helper_functions.R")
    source("functions/01_ysi_profile.R")

    # Save the parameters you need later on 
    loc_lat <- 40.29231
    loc_long <- -105.6562
    loc_alt <- 3105
    gl4_lat <- 40.0551844
    gl4_long <- -105.620445
    gl4_alt <- 3560

    # Write plotting function 
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
    GL4_dir <- here("/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/04_Mountain_Limno_Lab/01_Data/Sensor_Data/YSI_DSSPro/raw/GL4")
    GL4files <- dir_ls(GL4_dir, regexp = "\\.csv$", recurse = TRUE)     # Get all text files in the main directory and its subdirectories
    # KAG 20250815 -- I downloaded the YSI profiles to my machine for easy access locally. For days with multiple YSI profiles from different ice holes I took only the deepest profile 
    GL4files <- GL4files[str_detect(GL4files, "Zmax")]  # Only look at the "Zmax" files
    length(GL4files) #check how many files you have 
    
    # LOC 
    LOC_dir <- here("/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/04_Mountain_Limno_Lab/01_Data/Sensor_Data/YSI_DSSPro/raw/LOC")
    LOCfiles <- dir_ls(LOC_dir, regexp = "\\.csv$", recurse = TRUE) # Get all text files in the main directory and its subdirectories
    LOCfiles <- LOCfiles[str_detect(LOCfiles, "Zmax")]     # Only look at the "Zmax" files
    length(LOCfiles) #check how many files you have 
    
    
  # Process  YSI profiles 

    GL4files_processed <- lapply(GL4files, process_ysi)
    LOCfiles_processed <- lapply(LOCfiles, process_ysi)
    

# 02 Plot, format, and export  ---------------------------------------------

  #GL4 -- 13 profiles 
  setwd("/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/04_Mountain_Limno_Lab/01_Data/Sensor_Data/YSI_DSSPro/cleaned/GL4/")

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

      Round_Plot_YSI_FUNC(GL4files_processed[[6]], 0.25) # only one data point at 0.25 but I think still worth saving 
      GL4_2024_12_03 <-  OTI_YSI_FUNC(GL4files_processed[[6]], 0.25) 
      write_csv(GL4_2024_12_03, "GL4_2024_12_03_profile.csv")
      
      Round_Plot_YSI_FUNC(GL4files_processed[[7]], 0.25)
      GL4_2025_01_28 <- OTI_YSI_FUNC(GL4files_processed[[7]], 0.25)
      write_csv( GL4_2025_01_28, "GL4_2025_01_28_profile.csv")
      
      Round_Plot_YSI_FUNC(GL4files_processed[[8]], 0.25)
      GL4_2025_02_27 <- OTI_YSI_FUNC(GL4files_processed[[8]], 0.25)
      write_csv(GL4_2025_02_27, "GL4_2025_02_27_profile.csv")
      
      Round_Plot_YSI_FUNC(GL4files_processed[[9]], 0.25)
      GL4_2025_03_20 <- OTI_YSI_FUNC(GL4files_processed[[9]], 0.25)
      write_csv(GL4_2025_03_20, "GL4_2025_03_20_profile.csv")
      
      Round_Plot_YSI_FUNC(GL4files_processed[[10]], 0.25)
      GL4_2025_04_22 <- OTI_YSI_FUNC(GL4files_processed[[10]], 0.25)
      write_csv(GL4_2025_04_22, "GL4_2025_04_22_profile.csv" )
      
      Round_Plot_YSI_FUNC(GL4files_processed[[11]], 0.25)
      GL4_2025_07_01 <- OTI_YSI_FUNC(GL4files_processed[[11]], 0.25)
      write_csv(GL4_2025_07_01,"GL4_2025_07_01_profile.csv" )
      
      Round_Plot_YSI_FUNC(GL4files_processed[[12]], 0.25)
      GL4_2025_07_23 <- OTI_YSI_FUNC(GL4files_processed[[12]], 0.25)
      write_csv(GL4_2025_07_23, "GL4_2025_07_23_profile.csv")
      
      Round_Plot_YSI_FUNC(GL4files_processed[[13]], 0.5)
      GL4_2025_08_12 <- OTI_YSI_FUNC(GL4files_processed[[13]], 0.5)
      write_csv(GL4_2025_08_12, "GL4_2025_08_12_profile.csv")

      Round_Plot_YSI_FUNC(GL4files_processed[[14]], 0.5)
      GL4_2025_09_04 <- OTI_YSI_FUNC(GL4files_processed[[14]], 0.5)
      write_csv(GL4_2025_09_04, "GL4_2025_09_04_profile.csv")
      
  
  # LOC --- 24 files
    setwd("/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/04_Mountain_Limno_Lab/01_Data/Sensor_Data/YSI_DSSPro/cleaned/LOC/")
 
      Round_Plot_YSI_FUNC(LOCfiles_processed[[1]], 0.25)
      LOC_2023_12_05 <-  OTI_YSI_FUNC(LOCfiles_processed[[1]], 0.25)
      write_csv(LOC_2023_12_05, "LOC_2023_12_05_profile.csv")
      
      Round_Plot_YSI_FUNC(LOCfiles_processed[[2]], 0.1) 
      LOC_2024_01_30 <-  OTI_YSI_FUNC(LOCfiles_processed[[2]], 0.1)
      write_csv(LOC_2024_01_30, "LOC_2024_01_30_profile.csv" )
      
      Round_Plot_YSI_FUNC(LOCfiles_processed[[3]], 0.25)
      LOC_2024_02_06 <- OTI_YSI_FUNC(LOCfiles_processed[[3]], 0.25)
      write_csv(LOC_2024_02_06, "LOC_2024_02_06_profile.csv" )
      
      Round_Plot_YSI_FUNC(LOCfiles_processed[[4]], 0.1)
      LOC_2024_03_05 <- OTI_YSI_FUNC(LOCfiles_processed[[4]], 0.1)
      write_csv(LOC_2024_03_05, "LOC_2024_03_05_profile.csv")
      
      Round_Plot_YSI_FUNC(LOCfiles_processed[[5]], 0.25)
      LOC_2024_04_23 <- OTI_YSI_FUNC(LOCfiles_processed[[5]], 0.25)
      write_csv(LOC_2024_04_23, "LOC_2024_04_23_profile.csv")
      
      Round_Plot_YSI_FUNC(LOCfiles_processed[[6]], 0.1)
      LOC_2024_05_13 <-  OTI_YSI_FUNC(LOCfiles_processed[[6]], 0.1)
      write_csv(LOC_2024_05_13, "LOC_2024_05_13_profile.csv")
      
      Round_Plot_YSI_FUNC(LOCfiles_processed[[7]], 0.25)
      LOC_2024_06_13 <-  OTI_YSI_FUNC(LOCfiles_processed[[7]], 0.25)
      write_csv(LOC_2024_06_13, "LOC_2024_06_13_profile.csv")
      
      Round_Plot_YSI_FUNC(LOCfiles_processed[[8]], 0.25)
      LOC_2024_07_09 <- OTI_YSI_FUNC(LOCfiles_processed[[8]], 0.25)
      write_csv(LOC_2024_07_09, "LOC_2024_07_09_profile.csv")
      
      Round_Plot_YSI_FUNC(LOCfiles_processed[[9]], 0.25)
      LOC_2024_09_24 <- OTI_YSI_FUNC(LOCfiles_processed[[9]], 0.25)
      write_csv(LOC_2024_09_24, "LOC_2024_09_24_profile.csv")
      
      Round_Plot_YSI_FUNC(LOCfiles_processed[[10]], 0.1) 
      LOC_2024_10_25 <- OTI_YSI_FUNC(LOCfiles_processed[[10]], 0.25)
      write_csv(LOC_2024_10_25, "LOC_2024_10_25_profile.csv")

      Round_Plot_YSI_FUNC(LOCfiles_processed[[11]], 0.1)
      LOC_2024_11_21 <- OTI_YSI_FUNC(LOCfiles_processed[[11]], 0.25)
      write_csv(LOC_2024_11_21, "LOC_2024_11_21_profile.csv")
      
      Round_Plot_YSI_FUNC(LOCfiles_processed[[12]], 0.1)
      LOC_2024_12_12 <-  OTI_YSI_FUNC(LOCfiles_processed[[12]], 0.1)
      write_csv(LOC_2024_12_12,"LOC_2024_12_12_profile.csv" )
      
      Round_Plot_YSI_FUNC(LOCfiles_processed[[13]], 0.1) 
      LOC_2025_01_16 <- OTI_YSI_FUNC(LOCfiles_processed[[13]], 0.1)
      write_csv(LOC_2025_01_16, "LOC_2025_01_16_profile.csv")
      
      Round_Plot_YSI_FUNC(LOCfiles_processed[[14]], 0.1)
      LOC_2025_01_24 <- OTI_YSI_FUNC(LOCfiles_processed[[14]], 0.1)
      write_csv(LOC_2025_01_24, "LOC_2025_01_24_profile.csv")
      
      Round_Plot_YSI_FUNC(LOCfiles_processed[[15]], 0.1)
      LOC_2025_02_11 <- OTI_YSI_FUNC(LOCfiles_processed[[15]], 0.1)
      write_csv(LOC_2025_02_11, "LOC_2025_02_11_profile.csv" )
      
      Round_Plot_YSI_FUNC(LOCfiles_processed[[16]], 0.25)
      LOC_2025_02_25 <- OTI_YSI_FUNC(LOCfiles_processed[[16]], 0.25)
      write_csv(LOC_2025_02_25, "LOC_2025_02_25_profile.csv")
      
      Round_Plot_YSI_FUNC(LOCfiles_processed[[17]], 0.25) 
      LOC_2025_03_11 <- OTI_YSI_FUNC(LOCfiles_processed[[17]], 0.25)
      write_csv(LOC_2025_03_11,"LOC_2025_03_11_profile.csv" )
      
      Round_Plot_YSI_FUNC(LOCfiles_processed[[18]], 0.25)  
      LOC_2025_04_15 <- OTI_YSI_FUNC(LOCfiles_processed[[18]], 0.25)
      write_csv(LOC_2025_04_15, "LOC_2025_04_15_profile.csv")
      
      Round_Plot_YSI_FUNC(LOCfiles_processed[[19]], 0.25) 
      LOC_2025_05_22 <- OTI_YSI_FUNC(LOCfiles_processed[[19]], 0.25)
      write_csv(LOC_2025_05_22, "LOC_2025_05_22_profile.csv")
      
      Round_Plot_YSI_FUNC(LOCfiles_processed[[20]], 0.25) 
      LOC_2025_06_19 <-  OTI_YSI_FUNC(LOCfiles_processed[[20]], 0.25)
      write_csv(LOC_2025_06_19, "LOC_2025_06_19_profile.csv")
      
      Round_Plot_YSI_FUNC(LOCfiles_processed[[21]], 0.25)
      LOC_2025_07_29 <- OTI_YSI_FUNC(LOCfiles_processed[[21]], 0.25)
      write_csv(LOC_2025_07_29, "LOC_2025_07_29_profile.csv")

      
      
      
  
# # Profiles for The Loch (LOC) 
# 
# # Inspect the profiles, summarize the data, and export inot "LOC > export" folder
# 
# 
# # ... 2024-03-05
# 
# LOC1 <- process_ysi(LOCfiles[1])
# head(LOC1)
# 
# LOC1 %>%
#   mutate(depth_m=round(depth_m/0.25)*0.25) %>% #round to the nearest 0.5
#   group_by(depth_m, parameter, lake) %>%
#   mutate(value = median(value, na.rm=TRUE)) %>%
#   mutate(month=month(date_time)) %>%
#   filter(!parameter %in% c("barometer_mmHg","cond_spec_uScm")) %>%
#   ggplot(aes(x=value, y=depth_m, color=parameter))+
#   geom_point()+
#   scale_y_reverse()+
#   facet_wrap(parameter~., scales="free_x", nrow = 2)+
#   labs(title=paste(unique(LOC1$lake),unique(LOC1$date)))
# 
# 
# # ... 2024-04-23 
# 
# LOC2 <- process_ysi(LOCfiles[2])
# head(LOC2)
# 
# LOC2 %>%
#   mutate(depth_m=round(depth_m/0.25)*0.25) %>% #round to the nearest 0.5
#   group_by(depth_m, parameter, lake) %>%
#   mutate(value = median(value, na.rm=TRUE)) %>%
#   mutate(month=month(date_time)) %>%
#   filter(!parameter %in% c("barometer_mmHg","cond_spec_uScm")) %>%
#   ggplot(aes(x=value, y=depth_m, color=parameter))+
#   geom_point()+
#   scale_y_reverse()+
#   facet_wrap(parameter~., scales="free_x", nrow = 2)+
#   labs(title=paste(unique(LOC2$lake),unique(LOC2$date)))
# 
# 
# # ... 2024-05-13 
# 
# LOC3 <- process_ysi(LOCfiles[3])
# head(LOC3)
# 
# LOC3 %>%
#   mutate(depth_m=round(depth_m/0.25)*0.25) %>% #round to the nearest 0.5
#   group_by(depth_m, parameter, lake) %>%
#   mutate(value = median(value, na.rm=TRUE)) %>%
#   mutate(month=month(date_time)) %>%
#   filter(!parameter %in% c("barometer_mmHg","cond_spec_uScm")) %>%
#   ggplot(aes(x=value, y=depth_m, color=parameter))+
#   geom_point()+
#   scale_y_reverse()+
#   facet_wrap(parameter~., scales="free_x", nrow = 2)+
#   labs(title=paste(unique(LOC3$lake),unique(LOC3$date)))
# 
# 
# # ... For dates with multiple holes, pick the best looking one and export that 
# LOC14 <- process_ysi(LOCfiles[14])
# head(LOC14)
# 
# LOC14 %>%
#   mutate(depth_m=round(depth_m/0.25)*0.25) %>% #round to the nearest 0.5
#   group_by(depth_m, parameter, lake) %>%
#   mutate(value = median(value, na.rm=TRUE)) %>%
#   mutate(month=month(date_time)) %>%
#   filter(!parameter %in% c("barometer_mmHg","cond_spec_uScm")) %>%
#   ggplot(aes(x=value, y=depth_m, color=parameter))+
#   geom_point()+
#   scale_y_reverse()+
#   facet_wrap(parameter~., scales="free_x", nrow = 2)+
#   labs(title=paste(unique(LOC14$lake),unique(LOC14$date)))
# 
# LOC16 <- process_ysi(LOCfiles[16])
# head(LOC16)
# 
# LOC16 %>%
#   mutate(depth_m=round(depth_m/0.25)*0.25) %>% #round to the nearest 0.5
#   group_by(depth_m, parameter, lake) %>%
#   mutate(value = median(value, na.rm=TRUE)) %>%
#   mutate(month=month(date_time)) %>%
#   filter(!parameter %in% c("barometer_mmHg","cond_spec_uScm")) %>%
#   ggplot(aes(x=value, y=depth_m, color=parameter))+
#   geom_point()+
#   scale_y_reverse()+
#   facet_wrap(parameter~., scales="free_x", nrow = 2)+
#   labs(title=paste(unique(LOC16$lake),unique(LOC16$date)))


# OTHER LAKES 


#Compile all San Juans profiles 
# LFM_july <- process_ysi("Data/USFS San Juans/01_YSI/LOWER 4MILE/raw/LowerFourmile_Lake_20240718.csv")
# LFM_sept <- process_ysi("Data/USFS San Juans/01_YSI/LOWER 4MILE/raw/LowerFourmile_Lake_20240906.csv")
# UFM_sept <- process_ysi("Data/USFS San Juans/01_YSI/UPPER 4MILE/raw/UpperFourmile_Lake_20240907.csv")
# TKY_july <- process_ysi("Data/USFS San Juans/01_YSI/TKY CREEK/raw/TurkeyCreek_Lake_20240716.csv")
# TKY_sept <- process_ysi("Data/USFS San Juans/01_YSI/TKY CREEK/raw/TurkeyCreek_Lake_20240906.csv")
# LOC_march <- process_ysi("Data/On Thin Ice/01_YSI/LOC/raw/Loch_Zmax_20250225.csv")
# GL4_apr <- process_ysi("Data/On Thin Ice/01_YSI/GL4/raw/GL4_20250422.csv")
# 
# SJ_all <- bind_rows(LFM_july,
#                     LFM_sept,
#                     UFM_sept,
#                     TKY_july,
#                     TKY_sept)
# 
# SJ_all %>%
#   mutate(month=month(date_time)) %>%
#   filter(parameter=="temp") %>%
#   ggplot(aes(x=value, y=depth_m, color=factor(month)))+
#   geom_point()+
#   scale_y_reverse()+
#   facet_wrap(lake~.)
# 
# 
# #Can we round the depth_m values to smooth out the profiles?
# SJ_all %>%
#   mutate(depth_m=round(depth_m/0.25)*0.25) %>% #round to the nearest 0.25
#   group_by(depth_m, parameter, lake) %>%
#   mutate(value = median(value, na.rm=TRUE)) %>%
#   mutate(month=month(date_time)) %>%
#   filter(parameter=="do_mgL") %>%
#   ggplot(aes(x=value, y=depth_m, color=factor(month)))+
#   geom_point()+
#   scale_y_reverse()+
#   facet_wrap(lake~.)
# #Maybe?
# 
# #Can we round the time values to smooth out the profiles?
# SJ_all %>%
#   mutate(rounded_timestamp = as.POSIXct(round(as.numeric(date_time) / 5) * 5, origin = "1970-01-01")) %>%
#   #round to nearest 5 seconds
#   group_by(rounded_timestamp, lake, parameter) %>%
#   mutate(value = median(value, na.rm=TRUE)) %>%
#   mutate(month=month(date_time)) %>%
#   filter(parameter=="do_mgL") %>%
#   ggplot(aes(x=value, y=depth_m, color=factor(month)))+
#   geom_point()+
#   facet_wrap(lake~.)
# #Maybe?
# 
# # Do the more recent profiles look better?
# LOC_march  %>%
#   mutate(month=month(date_time)) %>%
#   filter(parameter=="temp") %>%
#   ggplot(aes(x=value, y=depth_m, color=factor(month)))+
#   geom_point()+
#   scale_y_reverse()+
#   facet_wrap(lake~.)
# # Yikes.
# 
# #Round to smooth out profiles?
# LOC_march %>%
#   mutate(depth_m=round(depth_m/0.25)*0.25) %>% #round to the nearest 0.1
#   group_by(depth_m, parameter, lake) %>%
#   mutate(value = median(value, na.rm=TRUE)) %>%
#   mutate(month=month(date_time)) %>%
#   filter(parameter=="temp") %>%
#   ggplot(aes(x=value, y=depth_m, color=factor(month)))+
#   geom_point()+
#   scale_y_reverse()+
#   facet_wrap(lake~.)
# # Maybe the inverse strat is just easy to miss?
# 
# # Do the more recent profiles look better?
# GL4_apr  %>%
#   mutate(month=month(date_time)) %>%
#   filter(parameter=="temp") %>%
#   ggplot(aes(x=value, y=depth_m, color=factor(month)))+
#   geom_point()+
#   scale_y_reverse()+
#   facet_wrap(lake~.)
# # A little better than The Loch
# 
# #Round to smooth out profiles?
# GL4_apr %>%
#   mutate(depth_m=round(depth_m/0.5)*0.5) %>% #round to the nearest 0.5
#   group_by(depth_m, parameter, lake) %>%
#   mutate(value = median(value, na.rm=TRUE)) %>%
#   mutate(month=month(date_time)) %>%
#   filter(parameter=="temp") %>%
#   ggplot(aes(x=value, y=depth_m, color=factor(month)))+
#   geom_point()+
#   scale_y_reverse()+
#   facet_wrap(lake~.)+
#   labs(x="Temp (deg C)")
# # This looks very reasonable.
# 
