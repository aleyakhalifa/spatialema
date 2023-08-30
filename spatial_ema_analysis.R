# Load packages----
library(readxl) # Importing/exporting excel
library(haven) # Importing stata
library(data.table) # Formatting data tables
library(lubridate) # Formatting time variables
library(tidyverse) #for data cleaning and formatting
library(sf) #for spatial data management and functions
library(ggmap) # Visualizing on a mapn
library(epitab) # Descriptive statistics
library(gtsummary) # Descriptive statistics
library(leaflet) # Visualizing on a map
library(leaflegend) # Visualizing on a map
library(sp) # Spatial data
library(spdep) # Spatial Dependence: Weighting Schemes, Statistics and Models
library(spatstat) # Spatial statistics
library(splancs) # K-function
library(smacpod) # Spatial scanning statistic 
library(mapview) # Visualizing on a map

# Load data----
survey <- readRDS("survey.rds") # RCCS survey data
emai_behavior <- readRDS(file="emai_behavior.rds") # EMAI trial reported behaviors
emai_gps <- readRDS("emai_gps.rds") # EMAI trial GPS data

#-------------------------------------------------------------------------#

# Part I. Describe mobility----
## Create spatial objects----
gps <- st_as_sf(emai_gps, coords=c('gps_long','gps_lat'),
                crs=4326) # convert lat and long to simple feature (sf) points
gps$geometry # check
 
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
rccs_dist <- df_list %>% reduce(full_join,by='RCCS_ID') %>% rename(EMAI_ID=EMAI_ID.x)

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

#### Table 2. Mobility measures----
#merge gps_weekly, total_gps_points, clustered_gps_points, locations_weekly, buffer_points, num_reports, num_reports_week
table <- data

#### Figure 2. Mobility by sociodemographics----
#Create the boxplot data (filter out outlier for visualization purposes)
iqr <- data %>% select(mean_distance,sex,age_group,rSEScat,education,resident,mobility,comm_type,occupation,hiv,motor,bicycle)
iqr$mean_distance <- as.numeric(iqr$mean_distance)

#iqr and p-value by category
iqr %>% tbl_continuous(variable=mean_distance,
                       include=c(sex,age_group,rSEScat,education,resident,mobility,comm_type,occupation,hiv,motor,bicycle)) %>%
  add_p(everything() ~ "kruskal.test", pvalue_fun = ~style_pvalue(.x, digits = 2)) 

#n's by category
iqr %>% tbl_summary()

#pivot longer
iqr_long <- iqr %>%  pivot_longer(-mean_distance)
write.xlsx(iqr_long, "iqr_long.xlsx")

#Plot the boxplot
#load data
boxplots <- read_excel("boxplots_230206.xlsx", sheet=2)

boxplots <- boxplots %>% filter(!is.na(value)) 
boxplots <- boxplots %>% filter(value!="N/A")

#order variables to match Table 1
boxplots$Variables <- factor(boxplots$Variables,
                             levels=c("Total","Gender (P = 0.37)",
                                      "Age group (P = 0.23)","Socioeconomic status quartile (P = 0.17)",
                                      "Highest level of education (P = 0.55)","Primary occupation (P = 0.81)",
                                      "HIV Status (P = 0.66)","Resident status (P = 0.39)","Reported migration (P = 0.93)",
                                      "Type of community (P = 0.15)",
                                      "Access to car or motorcycle (P = 0.15)","Access to a bicycle (P = 0.41)"),
                             ordered=TRUE)
boxplots$Values2 <- factor(boxplots$Values2,
                           levels=c("No (n = 26), IQR: 208 (111, 580)", "Yes (n = 19), IQR: 355 (182, 499)", 
                                    "No (n = 28), IQR: 215 (118, 452)", "Yes (n = 17), IQR: 355 (192, 588)", 
                                    "Trading (n = 19), IQR: 385 (202, 576)", "Fishing (n = 5), IQR: 203 (151, 230)", "Agrarian (n = 21), IQR: 172 (92, 553)", 
                                    "In-migrated (n = 2), IQR: 369 (230, 507)", "Out-migrated (n = 7), IQR: 230 (170, 400)", "No migration (n = 36), IQR: 302 (145, 571)", 
                                    "Transient (n = 1), IQR: 92 (92, 92)", "Former resident (out-migrated) (n = 6), IQR: 208 (162, 381)", "Permanent (n = 38), IQR: 338 (150, 580)", 
                                    "HIV positive (n = 5), IQR: 230 (203, 513)", "HIV negative (n = 40), IQR: 302 (127, 556)", 
                                    "Other (n = 5), IQR: 151 (121, 203)", "Student (n = 1), IQR: 274 (274, 274)", "Retail/trading/vending (n = 7), IQR: 485 (153, 871)", "Government/clerical/teaching (n = 12), IQR: 261 (167, 417)", "Construction/transportation (n = 2), IQR: 387 (258, 517)", "Agriculture (n = 14), IQR: 370 (96, 553)", 
                                    "Tertiary (n = 12), IQR: 189 (153, 400)", "Secondary (n = 33), IQR: 355 (129, 553)", 
                                    "Highest (n = 15), IQR: 212 (172, 519)", "High-middle (n = 17), IQR: 345 (151, 588)", "Low-middle (n = 6), IQR: 539 (334, 616)", "Lowest (n = 7), IQR: 129 (57, 300)", 
                                    "35-45 (n = 15), IQR: 331 (198, 549)", "25-34 (n = 16), IQR: 408 (127, 578)", "20-24 (n = 14), IQR: 199 (81, 366)", 
                                    "Women (n = 21), IQR: 369 (172, 588)", "Men (n = 24), IQR: 221 (145, 415)", 
                                    "N = 45, IQR: 274 (150, 533)"),
                           ordered=TRUE)

ggplot(data=boxplots, aes(x=Values2, y=mean_distance, color=as.character(Variables))) + 
  geom_boxplot() + 
  coord_flip() +
  ylab("Distribution of weekly distance (km) traveled")  + 
  scale_y_continuous(breaks=c(0,250,500,750,1000,1250,1500),limits=c(0,1500)) +
  facet_wrap(~Variables,labeller=labeller(Variables=label_wrap_gen(width=20)),strip.position="left",nrow=12,ncol=1,scales = "free_y") +
  theme_linedraw() +
  theme_grey() + 
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        strip.text.y.left = element_text(angle=0),
        strip.placement = "outside")

#### Supplement, Figure 1. Travel paths over the study period, by participants----
emai_gps_supfig <- emai_gps
emai_gps_supfig$EMAI_ID <- as.numeric(as.factor(emai_gps_supfig$EMAI_ID))

base <- get_stamenmap(bbox=c(left=29.5, bottom=-1.4 , right=34.0 , top=0.7), zoom=8, "terrain-lines")

supfig1 <- ggmap(base) + 
  geom_point(data=emai_gps_supfig, aes(x = gps_long, y = gps_lat, colour="red"), size=1) +
  facet_wrap(~EMAI_ID) + 
  xlab("") + 
  ylab("") +
  theme(legend.position="none",
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(),  
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.text.x = element_text(size = 20))
supfig1
ggsave("maps_by_id.pdf", plot = supfig1, width = 24, height = 24, units = "in")

