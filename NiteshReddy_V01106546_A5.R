# Set the working directory and verify it
setwd("/Users/niteshreddy/Downloads/USA /BootCamp/Stat Modeling")
getwd()
#install.packages(dplyr)
# Function to install and load libraries
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Load required libraries
libraries <- c("dplyr", "readr", "readxl", "tidyr", "ggplot2", "BSDA")
lapply(libraries, install_and_load)

# Reading the file into R
data <- read.csv("NSSO68.csv")

# Filtering for Haryana
df <- data %>%
  filter(state_1 == "HR")

# Display dataset info
cat("Dataset Information:\n")
print(names(df))
print(head(df))
print(dim(df))

# Finding missing values
missing_info <- colSums(is.na(df))
cat("Missing Values Information:\n")
print(missing_info)

# Subsetting the data
hrnew <- df %>%
  select(state_1, District, Region, Sector, State_Region, Meals_At_Home, ricepds_v, Wheatpds_q, chicken_q, pulsep_q, wheatos_q, No_of_Meals_per_day)

# Impute missing values with mean for specific columns
impute_with_mean <- function(column) {
  if (any(is.na(column))) {
    column[is.na(column)] <- mean(column, na.rm = TRUE)
  }
  return(column)
}
hrnew$Meals_At_Home <- impute_with_mean(hrnew$Meals_At_Home)

# Finding outliers and removing them
remove_outliers <- function(df, column_name) {
  Q1 <- quantile(df[[column_name]], 0.25)
  Q3 <- quantile(df[[column_name]], 0.75)
  IQR <- Q3 - Q1
  lower_threshold <- Q1 - (1.5 * IQR)
  upper_threshold <- Q3 + (1.5 * IQR)
  df <- subset(df, df[[column_name]] >= lower_threshold & df[[column_name]] <= upper_threshold)
  return(df)
}

outlier_columns <- c("ricepds_v", "chicken_q")
for (col in outlier_columns) {
  hrnew <- remove_outliers(hrnew, col)
}

# Summarize consumption
hrnew$total_consumption <- rowSums(hrnew[, c("ricepds_v", "Wheatpds_q", "chicken_q", "pulsep_q", "wheatos_q")], na.rm = TRUE)

# Summarize and display top consuming districts and regions
summarize_consumption <- function(group_col) {
  summary <- hrnew %>%
    group_by(across(all_of(group_col))) %>%
    summarise(total = sum(total_consumption)) %>%
    arrange(desc(total))
  return(summary)
}

district_summary <- summarize_consumption("District")
region_summary <- summarize_consumption("Region")

cat("Top Consuming Districts:\n")
print(head(district_summary, 4))
cat("Region Consumption Summary:\n")
print(region_summary)

# Rename districts and sectors
district_mapping <- c("1" = "Panchkula","2" = "Ambala","3" = "Yamunanagar","4" = "Kurukshetra","5" = "Kaithal","6" = "Karnal","7" = "Panipat","8" = "Sonipat","9" = "Jind","10" = "Fatehabad","11" = "Sirsa","12" = "Hisar","13" = "Bhiwani","14" = "Rohtak","15" = "Jhajjar","16" = "Mahendragarh","17" = "Rewari","18" = "Gurgaon","19" = "Faridabad","20" = "Mewat")
sector_mapping <- c("2" = "URBAN", "1" = "RURAL")

hrnew$District <- as.character(hrnew$District)
hrnew$Sector <- as.character(hrnew$Sector)
hrnew$District <- ifelse(hrnew$District %in% names(district_mapping), district_mapping[hrnew$District], hrnew$District)
hrnew$Sector <- ifelse(hrnew$Sector %in% names(sector_mapping), sector_mapping[hrnew$Sector], hrnew$Sector)

View(hrnew)

hist(hrnew$total_consumption, breaks = 10, col = 'blue', border = 'black', 
     xlab = "Consumption", ylab = "Frequency", main = "Consumption Distribution in Haryana State")

HR_consumption <- aggregate(total_consumption ~ District, data = hrnew, sum) 
View(HR_consumption)
??barplot
barplot(HR_consumption$total_consumption, 
        names.arg = HR_consumption$District, 
        las = 2, # Makes the district names vertical
        col = 'blue', 
        border = 'black', 
        xlab = "District", 
        ylab = "Total Consumption", 
        main = "Total Consumption per District",
        cex.names = 0.6) # Adjust the size of district names if needed


# b) Plot {'any variable of your choice'} on the Haryana state map using NSSO68.csv data

library(ggplot2) 
library(sf) # mapping
library(dplyr) 
Sys.setenv("SHAPE_RESTORE_SHX" = "YES") 

data_map <- st_read("/Users/niteshreddy/Downloads/HARYANA_DISTRICTS.geojson") 
View(data_map)

data_map <- data_map %>% 
  rename(District = dtname) 
colnames(data_map) 
data_map_data <- merge(HR_consumption,data_map,by = "District") 
View(data_map_data)
ggplot(data_map_data) + 
  geom_sf(aes(fill =total_consumption, geometry = geometry)) + 
  scale_fill_gradient(low = "yellow", high = "red") + 
  ggtitle("Total Consumption_by_District") 

ggplot(data_map_data) + 
  geom_sf(aes(fill = total_consumption, geometry = geometry)) + 
  scale_fill_gradient(low = "yellow", high = "red") + 
  ggtitle("Total Consumption by District") +
  geom_sf_text(aes(label = District, geometry = geometry), size = 3, color = "black")
