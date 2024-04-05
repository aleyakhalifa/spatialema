# Load packages----
library(readxl)
library(haven)
library(data.table)
library(lubridate)
library(tidyverse) #for data cleaning and formatting
library(sf) #for spatial data management and functions
library(ggmap) #for visualization
library(epitab)
library(gtsummary)
library(leaflet)
library(leaflegend)
library(sp)
library(spdep) # Spatial Dependence: Weighting Schemes, Statistics and Models
library(spatstat)
library(splancs) # K-function
library(smacpod) # Spatial scanning statistic 
library(mapview)


# Load data----

#RCCS survey data including R19 data + R18-17 for missing participants
survey <- read_excel("survey_230125.xlsx")
survey <- survey %>% filter(emai_id!="EM17")
saveRDS(survey, file = "survey.rds")

#SES quartiles estimated separately, by round
SES_data <- rbind(read_dta("SES_data.dta"),read_dta("SES_data_R17.dta"))
saveRDS(SES_data, file = "SES_data.rds")

#Trial outcome data
emai_outcome <- read.csv(file="EMAI_Outcome_18March2021v2.csv")
emai_outcome <- emai_outcome %>% 
  rename(EMAI_ID = patient_code) 
emai_group <- emai_outcome %>% 
  select(EMAI_ID, Group)
emai_group <- emai_group %>% 
  distinct(EMAI_ID, Group)

#Store start and end dates for each participant
emai_dates <- emai_outcome %>% select(EMAI_ID,Start_date)
emai_dates$start <- as.POSIXlt(emai_dates$Start_date, format = "%m / %d / %y", tz="Africa/Kampala")
emai_dates$end <- emai_dates$start + days(90)
#de-duplicate
emai_dates <- distinct(emai_dates)

#Reported behaviors
emai_behavior <- read.csv(file="eventcontingent.csv")
emai_behavior <- emai_behavior %>% 
  rename(EMAI_ID = patient_code) 
emai_behavior$EMAI_ID <- gsub("AI-0","",as.character(emai_behavior$EMAI_ID))
emai_behavior$EMAI_ID <- gsub("-","",as.character(emai_behavior$EMAI_ID))
emai_behavior$EMAI_ID <- gsub("_","",as.character(emai_behavior$EMAI_ID))
emai_behavior$EMAI_ID <- gsub("EM10A","EM10",as.character(emai_behavior$EMAI_ID))
emai_behavior <- filter(emai_behavior, grepl('EM',EMAI_ID))
  #Truncate to start and end dates by participant
  emai_behavior <- emai_behavior %>% mutate(timestamp = str_replace(meta.end,"T"," ")) 
  emai_behavior$timestamp <- as.POSIXlt(emai_behavior$timestamp, tz="Africa/Kampala")
  ### Merge with emai_dates
  emai_behavior <- inner_join(emai_dates,emai_behavior,by="EMAI_ID")
  ### Keep all rows where timestamp > (start - days(1)) & timestamp < (end + days(1))
  emai_behavior <- emai_behavior %>%
    filter((timestamp > (start)) & (timestamp < (end + days(1))))
saveRDS(emai_behavior, file = "emai_behavior.rds")

#-------------------------------------------------------------------------#

# Part I. Describe mobility----
## Set up data----
# Load data
emai_gps <- read.csv(file="emai_gps.csv") %>% filter(EMAI_ID!="EM17")


## Exclusions/data cleaning
### Drop locations without coordinates or timestamps
emai_gps <- emai_gps[!(is.na(emai_gps$gps_lat)), ]
emai_gps <- emai_gps[!(is.na(emai_gps$timestamp)), ]
### Remove duplicate points
emai_gps <- distinct(emai_gps,EMAI_ID,timestamp,.keep_all=TRUE)
### Study area boundary
## Remove implausible distances (where at least one GPS point is outside of the plausible boundary)
#left=29.5, bottom=-1.4 , right=34.0 , top=0.7
emai_gps <- emai_gps %>% filter(gps_lat>-1.4 & gps_lat<0.7 & gps_long>29.5 & gps_long<34.0)
#remove IDs without RCCS data
emai_gps <- emai_gps %>% filter(EMAI_ID!="EM25" & EMAI_ID!="EM53"& EMAI_ID!="EM17")

## Convert time
### Merge with emai_dates
emai_gps <- inner_join(emai_dates,emai_gps,by="EMAI_ID")
### Format timestamps & start/end dates
emai_gps$timestamp <- as.POSIXlt(emai_gps$timestamp, format = "%d / %m / %Y %H:%M", tz="Africa/Kampala")
### Keep all rows where timestamp > (start - days(1)) & timestamp < (end + days(1))
emai_gps <- emai_gps %>%
  filter((timestamp > (start)) & (timestamp < (end + days(1))))
### Create weeks
emai_gps <- emai_gps %>%
  group_by(EMAI_ID) %>% 
  mutate(week = week(timestamp))

saveRDS(emai_gps, file = "emai_gps.rds")

## Create spatial objects
#Convert lat and long to simple feature (sf) points
gps <- st_as_sf(emai_gps, coords=c('gps_long','gps_lat'),
                crs=4326)
#check
gps$geometry



## 1. Weekly distance----
#sort by timestamp
gps_week <- gps %>% arrange(EMAI_ID, timestamp)
  #remove elapsed time >6 hours
  
#calculate distance
empty <- st_as_sfc("POINT(EMPTY)", crs = 4326)

gps_week <- gps_week %>% 
  group_by(RCCS_ID) %>%
  st_set_crs(4326) %>%
  mutate(
    elapsed_time = lead(timestamp) - timestamp,
    distance_to_next = sf::st_distance(geometry, lead(geometry, default=NA), 
                                       by_element = TRUE),
    km_traveled = distance_to_next/1000)

gps_week <- gps_week %>%
  group_by(EMAI_ID, RCCS_ID, week) %>%
  summarize(Distance_week=sum(km_traveled, na.rm=TRUE)) %>%
  arrange(desc(Distance_week))
summary(gps_week$Distance_week)

### Distribution of weekly distance by participant
ggplot(gps_week) +
  geom_boxplot(aes(x=reorder(EMAI_ID,-Distance_week,median), y=as.numeric(Distance_week))) + 
  coord_cartesian( xlim = NULL, ylim = c(0,6000) ) + 
  theme(legend.position="none") + 
  xlab("Participant ID") + 
  ylab("Kilometers traveled per week") + 
  theme(axis.text.x = element_text(angle = 90))

### Describe weekly distances
gps_weekly <- gps_week %>%
  group_by(EMAI_ID, RCCS_ID) %>%
  summarize(mean_distance=mean(Distance_week), median_distance=median(Distance_week)) %>%
  arrange(desc(mean_distance))
summary(gps_weekly$mean_distance)


## 2. Number unique locations visited over the study period----
gps_africa <- gps %>% st_set_crs(4326) %>% st_transform(crs="ESRI:102011") #transform to Africa to identify clusters
#cluster function - use min points per radius of 2 because just want to identify duplicate points like the home
db2 <- function(x) {
  gps_africa <- x %>% st_coordinates()
  cluster_25 = dbscan::dbscan(gps_africa, eps = 25, minPts = 2)$cluster
  return (data.frame(cluster_25m=cluster_25))
}
# Apply function to cluster points
gps_africa_cluster_df <- gps_africa %>%
  group_by(EMAI_ID) %>%
  group_modify(~db2(.x)) %>% 
  ungroup()

#number of unique locations visited
# Merge this with the main data frame
gps_clusters <- bind_cols(gps_africa, gps_africa_cluster_df %>% 
                                dplyr::select(-EMAI_ID))

# De-duplicate clusters, keeping all unique non-clustered points (cluster_25m==0)
gps_clusters_0 <- gps_clusters %>% filter(cluster_25m==0)
gps_clusters_1 <- gps_clusters %>% filter(cluster_25m>0) %>% distinct(EMAI_ID, cluster_25m)
gps_clusters <- bind_rows(gps_clusters_0, gps_clusters_1)

# Number of unique clusters
unique_locations <- gps_clusters %>%
  group_by(EMAI_ID) %>%
  summarize(unique_locations = n())
summary(unique_locations$unique_locations)

# Total number of GPS points
total_gps_points <- gps %>% group_by(EMAI_ID) %>% summarize(total=n())
summary(total_gps_points$total)


## 3. % GPS points in 100m, 400m, 800m home buffer region----
#load home coordinates
home <- survey %>% select(EMAI_ID, latitude, longitude) %>%
  st_as_sf(coords=c('longitude','latitude'),
           crs=4326)
home$buffer_100 <- st_buffer(home$geometry,100)
home$buffer_400 <- st_buffer(home$geometry,400)
home$buffer_800 <- st_buffer(home$geometry,800)
mapview(home$buffer_800)

# create a vector of unique EMAI_ID values
id_vec <- unique(home$EMAI_ID)

# create an empty dataframe to store the results
buffer_points <- data.frame(EMAI_ID = character(),
                         contains_na_100 = numeric(),
                         contains_not_na_100 = numeric(),
                         contains_na_400 = numeric(),
                         contains_not_na_400 = numeric(),
                         contains_na_800 = numeric(),
                         contains_not_na_800 = numeric(),
                         stringsAsFactors = FALSE)

# loop through each EMAI_ID value
for (id in id_vec) {
  # filter home and gps dataframes by current EMAI_ID
  home_id <- home %>% filter(EMAI_ID == id)
  gps_id <- gps %>% filter(EMAI_ID == id)
  
  # compute contains variable for gps data using home buffer
  gps_id <- gps_id %>% mutate(contains_100 = st_intersects(gps_id$geometry, home_id$buffer_100))
  gps_id$contains_100 <- unlist(lapply(gps_id$contains_100, function (x) ifelse(length(x) > 0, x, NA)))
  gps_id <- gps_id %>% mutate(contains_400 = st_intersects(gps_id$geometry, home_id$buffer_400))
  gps_id$contains_400 <- unlist(lapply(gps_id$contains_400, function (x) ifelse(length(x) > 0, x, NA)))
  gps_id <- gps_id %>% mutate(contains_800 = st_intersects(gps_id$geometry, home_id$buffer_800))
  gps_id$contains_800 <- unlist(lapply(gps_id$contains_800, function (x) ifelse(length(x) > 0, x, NA)))
  
  # count number of rows in gps_id where contains is not NA
  contains_na_100 <- sum(is.na(gps_id$contains_100))
  contains_not_na_100 <- sum(!is.na(gps_id$contains_100))
  contains_na_400 <- sum(is.na(gps_id$contains_400))
  contains_not_na_400 <- sum(!is.na(gps_id$contains_400))
  contains_na_800 <- sum(is.na(gps_id$contains_800))
  contains_not_na_800 <- sum(!is.na(gps_id$contains_800))
  
  # add current EMAI_ID and contains_not_na to results_df
  buffer_points <- rbind(buffer_points, data.frame(EMAI_ID = id,
                                             contains_na_100 = contains_na_100,
                                             contains_not_na_100 = contains_not_na_100,
                                             contains_na_400 = contains_na_400,
                                             contains_not_na_400 = contains_not_na_400,
                                             contains_na_800 = contains_na_800,
                                             contains_not_na_800 = contains_not_na_800,
                                             stringsAsFactors = FALSE))
}

#calculate % outside each home buffer
buffer_points <- buffer_points %>% mutate(outside_100 = contains_na_100/(contains_na_100+contains_not_na_100),
                                          outside_400 = contains_na_400/(contains_na_400+contains_not_na_400),
                                          outside_800 = contains_na_800/(contains_na_800+contains_not_na_800))
summary(buffer_points$outside_100)
summary(buffer_points$outside_400)
summary(buffer_points$outside_800)

summary(buffer_points$contains_na_100)
summary(buffer_points$contains_na_400)
summary(buffer_points$contains_na_800)

#-------------------------------------------------------------------------#
# Part II. Describe mobility by survey measures----
#### Merge data----
#Merge with survey data
df_list <- list(survey, gps_weekly)
rccs_dist <- df_list %>% reduce(full_join,by='RCCS_ID')
#filter out EM17 because they don't have gps data
#also note, three participants with EMAI_ID.y's were not matched to a survey.
rccs_dist <- rccs_dist %>% rename(EMAI_ID=EMAI_ID.x) %>% filter(EMAI_ID!="EM17")

#Merge with SES data
SES_data$curr_id <- as.character(SES_data$curr_id)
df_list <- list(rccs_dist, SES_data)
rccs_ses_dist <- df_list %>% reduce(left_join,by='curr_id')

#Merge with emai trial group
df_list <- list(rccs_ses_dist, emai_group)
rccs_ses_dist_ema <- df_list %>% reduce(full_join,by='EMAI_ID')

data <- rccs_ses_dist_ema %>% filter_at(vars(emai_id,EMAI_ID),all_vars(!is.na(.)))

#### Format vars----
#Format transportation variables
#access to car or motorcycle
data <- data %>% mutate(motor = ifelse((car==1) | (motorcyc==1),1,
                                       ifelse((car==2) & (motorcyc==2),2,
                                              8)))
data$motor <- factor(data$motor,
                     levels=c(1,2),
                     labels=c("Yes","No"))
attr(data$motor,"label") <- "Access to car or motorcycle"   
table(data$motor)


#access to any transportation
data <- data %>% mutate(transport = ifelse((motor==1) | (bicycle==1),1,
                                           ifelse((motorcyc==2) & (bicycle==2),2,
                                                  8)))
data$transport <- factor(data$transport,
                         levels=c(1,2),
                         labels=c("Yes","No"))
attr(data$transport,"label") <- "Access to any transportation (car, motorcycle, bicycle)"   
table(data$transport)


#access to transportation: car
data$car <- factor(data$car,
                   levels=c(1,2),
                   labels=c("Yes","No"))
attr(data$car,"label") <- "Access to a car" 
#access to transportation: motorcycle
data$motorcyc <- factor(data$motorcyc,
                        levels=c(1,2),
                        labels=c("Yes","No"))
attr(data$motorcyc,"label") <- "Access to a motorcycle" 
#access to transportation: bicycle
data$bicycle <- factor(data$bicycle,
                       levels=c(1,2),
                       labels=c("Yes","No"))
attr(data$bicycle,"label") <- "Access to a bicycle" 
#Format distance traveled
#continuous  
attr(data$mean_distance,"label") <- "Average weekly distance traveled (km)"
#quartiles
data <- data %>% mutate(distance_quart=ntile(mean_distance,4))
data$distance_quart <- factor(data$distance_quart,
                              levels=c(1,2,3,4),
                              labels=c("Least mobile", "Less mobile", "More mobile", "Most mobile"))
attr(data$distance_quart,"label") <- "Quartiles of average weekly distance traveled (km)" 

#Format sociodemographic variables
#gender
data <- data %>%
  mutate(sex = ifelse(sex=="M", "Men",
                      ifelse(sex=="F", "Women",
                             NA)))
attr(data$sex,"label") <- "Gender"

#age
data <- data %>% 
  mutate(age_group = ifelse(ageyrs<25,"20-24",
                            ifelse(ageyrs>24 & ageyrs<35,"25-34",
                                   ifelse(ageyrs>24 & ageyrs<46,"35-45",
                                          NA))))
attr(data$age_group,"label") <- "Age group"

#SES
data$rSEScat <- factor(data$rSEScat,levels=c(1,2,3,4),labels=c("Lowest","Low-middle","High-middle","Highest"))
attr(data$rSEScat,"label") <- "Socioeconomic status quartile"

#education: primary and secondary/tertiary (all had gone to school)
data <- data %>%
  mutate(education = ifelse(educyrs==3 | educyrs==4,1,
                            ifelse(educyrs==5 | educyrs==7,2,
                                   NA)))
data$education <- factor(data$education,
                         levels=c(1,2),
                         labels=c("Secondary","Tertiary"))
attr(data$education,"label") <- "Highest level of education"

#resident
data$resident <- factor(data$resident,
                        levels=c(0,1,2),
                        labels=c("Former resident (out-migrated)","Permanent","Transient"))
attr(data$resident,"label") <- "Resident status"

#migrant
data$mobility <- factor(data$mobility,
                        levels=c(1,3,4),
                        labels=c("No migration","In-migrated","Out-migrated"))
attr(data$mobility,"label") <- "Reported migration" 

#community type
data$comm_type <- factor(data$comm_type,
                         levels=c(0,1,2),
                         labels=c("Agrarian","Fishing","Trading"))
attr(data$comm_type,"label") <- "Type of community"

#occupation
data <- data %>%
  mutate(occupation = ifelse(occup1==1 | occup1==2, 1,
                             ifelse(occup1==10 | occup1==11,2,
                                    ifelse(occup1==20 | occup1==22,3,
                                           ifelse(occup1==6,4,
                                                  ifelse(occup1==8,5,
                                                         ifelse(occup1==15,6,
                                                                NA)))))))
data$occupation <- factor(data$occupation,
                          levels=c(1,2,3,4,5,6),
                          labels=c("Agriculture","Retail/trading/vending","Construction/transportation","Government/clerical/teaching","Student","Other"))


attr(data$occupation,"label") <- "Primary occupation"

#HIV status
data <- data %>%
  mutate(hiv = ifelse(finalhiv=="P","HIV positive",
                      ifelse(finalhiv=="N","HIV negative",
                             ifelse(finalhiv=="","Unknown",
                                    NA))))
attr(data$hiv,"label") <- "HIV Status"

attr(data$Group,"label") <- "Trial arm"


#-------------------------------------------------------------------------#

# Part III. Describe behaviors----
## Merge data----
#behavior timestamps
  filters <- c("SMK","ALC","SEX")
  emai_behavior <- emai_behavior %>% filter(grepl(paste(filters,collapse="|"),PAST_HOUR))
  #convert both timestamps to numeric for matching
  emai_behavior$time <- as.numeric(as.POSIXct(emai_behavior$timestamp))
  gps$time <- as.numeric(as.POSIXct(gps$timestamp))                             
  #use base r to merge  - include all=TRUE in merge() if want to classify all the GPS coordinates without behavior reports as controls as well
  z <- lapply(intersect(emai_behavior$EMAI_ID,gps$EMAI_ID),function(id) {
  d1 <- subset(emai_behavior,EMAI_ID==id)
  d2 <- subset(gps,EMAI_ID==id)
  
  d1$indices <- sapply(d1$time,function(d) which.min(abs(d2$time - d)))
  d2$indices <- 1:nrow(d2)
  
  merge(d1,d2,by=c('EMAI_ID','indices'),all=TRUE)
  })
gps_behavior <- do.call(rbind,z)
gps_behavior$indices <- NULL

## Format behavioral outcomes----
gps_behavior <- gps_behavior %>%
  mutate(smoked = ifelse(SMK_EC_AMT>0,1,
                         NA)) %>%
  mutate(smoked = ifelse(is.na(smoked),0,1))
attr(gps_behavior$smoked,"label") <- "Smoked"

gps_behavior <- gps_behavior %>%
  mutate(drank = str_detect(PAST_HOUR,"ALC_EC")) %>%
  mutate(drank = ifelse(drank==TRUE,1,NA)) %>%
  mutate(drank = ifelse(is.na(drank),0,1))
attr(gps_behavior$drank,"label") <- "Drank alcohol"

gps_behavior <- gps_behavior %>%
  mutate(condomless_sex =  ifelse(CONDOM_EC==0,1,
                                  NA)) %>%
  mutate(condomless_sex = ifelse(is.na(condomless_sex),0,1))
attr(gps_behavior$condomless_sex,"label") <- "Had condomless sex"

## 1. Number of reports----
num_reports <- gps_behavior %>% 
  group_by(EMAI_ID) %>% 
  summarize(condom=sum(condomless_sex), drank=sum(drank), smoked=sum(smoked)) %>%
  mutate(ever_condom = ifelse(condom>0,1,0),
         ever_drank = ifelse(drank>0,1,0),
         ever_smoked = ifelse(smoked>0,1,0))

table(num_reports$ever_drank)
num_reports %>% filter(ever_drank==1) %>% summary(drank)

table(num_reports$ever_smoked)
num_reports %>% filter(ever_smoked==1) %>% summary(smoked)

table(num_reports$ever_condom)
num_reports %>% filter(ever_condom==1) %>% summary(condom)

## 2. Number unique locations where behavior occurred----
#### Alcohol----
gps_drank <- gps_behavior %>% 
  filter(drank==1) 
gps_drank <- gps_drank %>%
  st_sf(gps_drank$geometry) %>%
  st_set_crs(4326) %>% st_transform(crs="ESRI:102011") 

# Initialize an empty list to store results
results_list <- list()

# Loop through unique participant IDs
unique_ids <- unique(gps_drank$EMAI_ID)

for (id in unique_ids) {
  df <- gps_drank %>% filter(EMAI_ID == id) %>% st_coordinates()
  results <- dbscan::dbscan(df, eps = 25, minPts = 2)$cluster
  
  # Create a data frame with participant_ID and results
  result_df <- data.frame(EMAI_ID = id, cluster = results)
  
  # Append the result_df to the results_list
  results_list[[id]] <- result_df
}

# Combine all the result data frames into a single dataframe
drank_clusters <- bind_rows(results_list)

# Number of unique clusters
unique_locations_drank <- drank_clusters %>%
  group_by(EMAI_ID) %>%
  summarize(num_clusters = max(cluster), num_pts_alone = length(which(cluster==0))) %>%
  mutate(num_unique = num_clusters + num_pts_alone)
summary(unique_locations_drank$num_unique)



#### Smoking----
gps_smoked <- gps_behavior %>% 
  filter(smoked==1) 
gps_smoked <- gps_smoked %>%
  st_sf(gps_smoked$geometry) %>%
  st_set_crs(4326) %>% st_transform(crs="ESRI:102011") 

# Initialize an empty list to store results
results_list <- list()

# Loop through unique participant IDs
unique_ids <- unique(gps_smoked$EMAI_ID)

for (id in unique_ids) {
  df <- gps_smoked %>% filter(EMAI_ID == id) %>% st_coordinates()
  results <- dbscan::dbscan(df, eps = 25, minPts = 2)$cluster
  
  # Create a data frame with participant_ID and results
  result_df <- data.frame(EMAI_ID = id, cluster = results)
  
  # Append the result_df to the results_list
  results_list[[id]] <- result_df
}

# Combine all the result data frames into a single dataframe
smoked_clusters <- bind_rows(results_list)

# Number of unique clusters
unique_locations_smoked <- smoked_clusters %>%
  group_by(EMAI_ID) %>%
  summarize(num_clusters = max(cluster), num_pts_alone = length(which(cluster==0))) %>%
  mutate(num_unique = num_clusters + num_pts_alone)
summary(unique_locations_smoked$num_unique)


#### Condoms----
gps_condom <- gps_behavior %>% 
  filter(condomless_sex==1) 
gps_condom <- gps_condom %>%
  st_sf(gps_condom$geometry) %>%
  st_set_crs(4326) %>% st_transform(crs="ESRI:102011") 

# Initialize an empty list to store results
results_list <- list()

# Loop through unique participant IDs
unique_ids <- unique(gps_condom$EMAI_ID)

for (id in unique_ids) {
  df <- gps_condom %>% filter(EMAI_ID == id) %>% st_coordinates()
  results <- dbscan::dbscan(df, eps = 25, minPts = 2)$cluster
  
  # Create a data frame with participant_ID and results
  result_df <- data.frame(EMAI_ID = id, cluster = results)
  
  # Append the result_df to the results_list
  results_list[[id]] <- result_df
}

# Combine all the result data frames into a single dataframe
condom_clusters <- bind_rows(results_list)

# Number of unique clusters
unique_locations_condom <- condom_clusters %>%
  group_by(EMAI_ID) %>%
  summarize(num_clusters = max(cluster), num_pts_alone = length(which(cluster==0))) %>%
  mutate(num_unique = num_clusters + num_pts_alone)
summary(unique_locations_condom$num_unique)



## 3. % GPS points in 100m, 400m, 800m home buffer region----
#### Alcohol----
# create an empty dataframe to store the results
buffer_points_drank <- data.frame(EMAI_ID = character(),
                            contains_na_100 = numeric(),
                            contains_not_na_100 = numeric(),
                            contains_na_400 = numeric(),
                            contains_not_na_400 = numeric(),
                            contains_na_800 = numeric(),
                            contains_not_na_800 = numeric(),
                            stringsAsFactors = FALSE)

# loop through each EMAI_ID value
for (id in id_vec) {
  # filter home and gps dataframes by current EMAI_ID
  home_id <- home %>% filter(EMAI_ID == id)
  gps_id <- gps_drank %>% filter(EMAI_ID == id)
  
  # compute contains variable for gps data using home buffer
  gps_id <- gps_id %>% mutate(contains_100 = st_intersects(gps_id$geometry, home_id$buffer_100))
  gps_id$contains_100 <- unlist(lapply(gps_id$contains_100, function (x) ifelse(length(x) > 0, x, NA)))
  gps_id <- gps_id %>% mutate(contains_400 = st_intersects(gps_id$geometry, home_id$buffer_400))
  gps_id$contains_400 <- unlist(lapply(gps_id$contains_400, function (x) ifelse(length(x) > 0, x, NA)))
  gps_id <- gps_id %>% mutate(contains_800 = st_intersects(gps_id$geometry, home_id$buffer_800))
  gps_id$contains_800 <- unlist(lapply(gps_id$contains_800, function (x) ifelse(length(x) > 0, x, NA)))
  
  # count number of rows in gps_id where contains is not NA
  contains_na_100 <- sum(is.na(gps_id$contains_100))
  contains_not_na_100 <- sum(!is.na(gps_id$contains_100))
  contains_na_400 <- sum(is.na(gps_id$contains_400))
  contains_not_na_400 <- sum(!is.na(gps_id$contains_400))
  contains_na_800 <- sum(is.na(gps_id$contains_800))
  contains_not_na_800 <- sum(!is.na(gps_id$contains_800))
  
  # add current EMAI_ID and contains_not_na to results_df
  buffer_points_drank <- rbind(buffer_points_drank, data.frame(EMAI_ID = id,
                                                   contains_na_100 = contains_na_100,
                                                   contains_not_na_100 = contains_not_na_100,
                                                   contains_na_400 = contains_na_400,
                                                   contains_not_na_400 = contains_not_na_400,
                                                   contains_na_800 = contains_na_800,
                                                   contains_not_na_800 = contains_not_na_800,
                                                   stringsAsFactors = FALSE))
}

#calculate % outside each home buffer
buffer_points_drank <- buffer_points_drank %>% mutate(outside_100 = contains_na_100/(contains_na_100+contains_not_na_100),
                                          outside_400 = contains_na_400/(contains_na_400+contains_not_na_400),
                                          outside_800 = contains_na_800/(contains_na_800+contains_not_na_800))

buffer_points_drank <- buffer_points_drank %>% mutate(outside_100_yes = ifelse(outside_100>0,1,ifelse(outside_100==0,0,NA)),
                                                      outside_400_yes = ifelse(outside_400>0,1,ifelse(outside_400==0,0,NA)),
                                                      outside_800_yes = ifelse(outside_800>0,1,ifelse(outside_800==0,0,NA)),)

table(buffer_points_drank$outside_100_yes)
table(buffer_points_drank$outside_400_yes)
table(buffer_points_drank$outside_800_yes)

buffer_points_drank <- buffer_points_drank %>% filter(!is.na(outside_100_yes))
summary(buffer_points_drank$contains_na_100)
summary(buffer_points_drank$contains_na_400)
summary(buffer_points_drank$contains_na_800)

#### Smoking----
# create an empty dataframe to store the results
buffer_points_smoked <- data.frame(EMAI_ID = character(),
                                  contains_na_100 = numeric(),
                                  contains_not_na_100 = numeric(),
                                  contains_na_400 = numeric(),
                                  contains_not_na_400 = numeric(),
                                  contains_na_800 = numeric(),
                                  contains_not_na_800 = numeric(),
                                  stringsAsFactors = FALSE)

# loop through each EMAI_ID value
for (id in id_vec) {
  # filter home and gps dataframes by current EMAI_ID
  home_id <- home %>% filter(EMAI_ID == id)
  gps_id <- gps_smoked %>% filter(EMAI_ID == id)
  
  # compute contains variable for gps data using home buffer
  gps_id <- gps_id %>% mutate(contains_100 = st_intersects(gps_id$geometry, home_id$buffer_100))
  gps_id$contains_100 <- unlist(lapply(gps_id$contains_100, function (x) ifelse(length(x) > 0, x, NA)))
  gps_id <- gps_id %>% mutate(contains_400 = st_intersects(gps_id$geometry, home_id$buffer_400))
  gps_id$contains_400 <- unlist(lapply(gps_id$contains_400, function (x) ifelse(length(x) > 0, x, NA)))
  gps_id <- gps_id %>% mutate(contains_800 = st_intersects(gps_id$geometry, home_id$buffer_800))
  gps_id$contains_800 <- unlist(lapply(gps_id$contains_800, function (x) ifelse(length(x) > 0, x, NA)))
  
  # count number of rows in gps_id where contains is not NA
  contains_na_100 <- sum(is.na(gps_id$contains_100))
  contains_not_na_100 <- sum(!is.na(gps_id$contains_100))
  contains_na_400 <- sum(is.na(gps_id$contains_400))
  contains_not_na_400 <- sum(!is.na(gps_id$contains_400))
  contains_na_800 <- sum(is.na(gps_id$contains_800))
  contains_not_na_800 <- sum(!is.na(gps_id$contains_800))
  
  # add current EMAI_ID and contains_not_na to results_df
  buffer_points_smoked <- rbind(buffer_points_smoked, data.frame(EMAI_ID = id,
                                                               contains_na_100 = contains_na_100,
                                                               contains_not_na_100 = contains_not_na_100,
                                                               contains_na_400 = contains_na_400,
                                                               contains_not_na_400 = contains_not_na_400,
                                                               contains_na_800 = contains_na_800,
                                                               contains_not_na_800 = contains_not_na_800,
                                                               stringsAsFactors = FALSE))
}

#calculate % outside each home buffer
buffer_points_smoked <- buffer_points_smoked %>% mutate(outside_100 = contains_na_100/(contains_na_100+contains_not_na_100),
                                                      outside_400 = contains_na_400/(contains_na_400+contains_not_na_400),
                                                      outside_800 = contains_na_800/(contains_na_800+contains_not_na_800))
summary(buffer_points_smoked$contains_na_100)
summary(buffer_points_smoked$outside_100)
summary(buffer_points_smoked$contains_na_400)
summary(buffer_points_smoked$outside_400)
summary(buffer_points_smoked$contains_na_800)
summary(buffer_points_smoked$outside_800)

buffer_points_smoked <- buffer_points_smoked %>% mutate(outside_100_yes = ifelse(outside_100>0,1,ifelse(outside_100==0,0,NA)),
                                                      outside_400_yes = ifelse(outside_400>0,1,ifelse(outside_400==0,0,NA)),
                                                      outside_800_yes = ifelse(outside_800>0,1,ifelse(outside_800==0,0,NA)),)
table(buffer_points_smoked$outside_100_yes)
table(buffer_points_smoked$outside_400_yes)
table(buffer_points_smoked$outside_800_yes)

buffer_points_smoked <- buffer_points_smoked %>% filter(!is.na(outside_100_yes))
summary(buffer_points_smoked$contains_na_100)
summary(buffer_points_smoked$contains_na_400)
summary(buffer_points_smoked$contains_na_800)

#### Condoms----
# create an empty dataframe to store the results
buffer_points_condom <- data.frame(EMAI_ID = character(),
                                   contains_na_100 = numeric(),
                                   contains_not_na_100 = numeric(),
                                   contains_na_400 = numeric(),
                                   contains_not_na_400 = numeric(),
                                   contains_na_800 = numeric(),
                                   contains_not_na_800 = numeric(),
                                   stringsAsFactors = FALSE)

# loop through each EMAI_ID value
for (id in id_vec) {
  # filter home and gps dataframes by current EMAI_ID
  home_id <- home %>% filter(EMAI_ID == id)
  gps_id <- gps_condom %>% filter(EMAI_ID == id)
  
  # compute contains variable for gps data using home buffer
  gps_id <- gps_id %>% mutate(contains_100 = st_intersects(gps_id$geometry, home_id$buffer_100))
  gps_id$contains_100 <- unlist(lapply(gps_id$contains_100, function (x) ifelse(length(x) > 0, x, NA)))
  gps_id <- gps_id %>% mutate(contains_400 = st_intersects(gps_id$geometry, home_id$buffer_400))
  gps_id$contains_400 <- unlist(lapply(gps_id$contains_400, function (x) ifelse(length(x) > 0, x, NA)))
  gps_id <- gps_id %>% mutate(contains_800 = st_intersects(gps_id$geometry, home_id$buffer_800))
  gps_id$contains_800 <- unlist(lapply(gps_id$contains_800, function (x) ifelse(length(x) > 0, x, NA)))
  
  # count number of rows in gps_id where contains is not NA
  contains_na_100 <- sum(is.na(gps_id$contains_100))
  contains_not_na_100 <- sum(!is.na(gps_id$contains_100))
  contains_na_400 <- sum(is.na(gps_id$contains_400))
  contains_not_na_400 <- sum(!is.na(gps_id$contains_400))
  contains_na_800 <- sum(is.na(gps_id$contains_800))
  contains_not_na_800 <- sum(!is.na(gps_id$contains_800))
  
  # add current EMAI_ID and contains_not_na to results_df
  buffer_points_condom <- rbind(buffer_points_condom, data.frame(EMAI_ID = id,
                                                                 contains_na_100 = contains_na_100,
                                                                 contains_not_na_100 = contains_not_na_100,
                                                                 contains_na_400 = contains_na_400,
                                                                 contains_not_na_400 = contains_not_na_400,
                                                                 contains_na_800 = contains_na_800,
                                                                 contains_not_na_800 = contains_not_na_800,
                                                                 stringsAsFactors = FALSE))
}

#calculate % outside each home buffer
buffer_points_condom <- buffer_points_condom %>% mutate(outside_100 = contains_na_100/(contains_na_100+contains_not_na_100),
                                                        outside_400 = contains_na_400/(contains_na_400+contains_not_na_400),
                                                        outside_800 = contains_na_800/(contains_na_800+contains_not_na_800))

buffer_points_condom <- buffer_points_condom %>% mutate(outside_100_yes = ifelse(outside_100>0,1,ifelse(outside_100==0,0,NA)),
                                                      outside_400_yes = ifelse(outside_400>0,1,ifelse(outside_400==0,0,NA)),
                                                      outside_800_yes = ifelse(outside_800>0,1,ifelse(outside_800==0,0,NA)),)

table(buffer_points_condom$outside_100_yes)
table(buffer_points_condom$outside_400_yes)
table(buffer_points_condom$outside_800_yes)

buffer_points_condom <- buffer_points_condom %>% filter(!is.na(outside_100_yes))
summary(buffer_points_condom$contains_na_100)
summary(buffer_points_condom$contains_na_400)
summary(buffer_points_condom$contains_na_800)

# Figures and tables----
#### Figure 1. Travel paths over the study period, all participants----
pal <- colorFactor(palette="Paired",domain=emai_gps$EMAI_ID)
fig1 <- leaflet(data=emai_gps,options = leafletOptions(zoomControl = FALSE)) %>%
  setView(lng = 31.2, lat = -.38, zoom = 8.6) %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addCircleMarkers(lng= ~gps_long, lat= ~gps_lat,
                   radius=1.8, color=~pal(EMAI_ID), fillOpacity=0.2) %>%
  addScaleBar(position = "bottomleft", options=scaleBarOptions(metric=TRUE,imperial=FALSE)) 
fig1
savePlot(fig1,filename="fig1.png",type="png")

#### Table 1. Sociodemographic characteristics----
table <- data %>% select(sex,age_group,rSEScat,education,resident,mobility,comm_type,occupation,hiv,motor,bicycle)
table1 <- table %>% tbl_summary()
# Write to xlsx
table1 %>%
  as_tibble() %>% 
  write.xlsx("Table1.xlsx")


#### Description of the data----
## Number observation days
emai_gps %>% 
  select(EMAI_ID, timestamp) %>% 
  mutate(date = as.Date(timestamp)) %>%
  select(-timestamp) %>% 
  unique() %>% 
  group_by(EMAI_ID) %>% 
  summarize(n = n()) %>% 
  summary(n)

## Number points per day
emai_gps %>% 
  select(EMAI_ID, timestamp) %>% 
  mutate(date = as.Date(timestamp)) %>%
  group_by(EMAI_ID, date) %>% 
  summarize(n = n()) %>% 
  group_by(EMAI_ID) %>% 
  summarize(pts_per_day = mean(n)) %>%
  summary(pts_per_day)


