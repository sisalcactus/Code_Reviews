#A4
#Author: Owen Treleaven
#-------------------------------
#Make sure the original is never changed!

getwd()
setwd("~/Google Drive/UoT - MBiotech/DHT_R_2023/Assignments/A4_MARTIANS")
library(tidyverse)
og_ufo_data <- read.delim("ufo_subset.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
#creating copy of data to work with
ufo_data <- og_ufo_data 
print(head(ufo_data))
class(ufo_data)#ensuring data is loaded as a dataframe
#' Data Dictionary
#' datetime: Contains date and time of sighting
#' city: City in which UFO was sighted
#' state: State code in which UFO was sighted
#' country: Country code of sighting
#' shape: Shape of the UFO
#' duration seconds: Duration of the sighting in seconds
#' duration hours min: Duration of the sighting in hours and min
#' comments: Sighting description
#' date _posted: Posted date of the sighting
#' latitude: Latitude coordinate of the sighting
#' longitude: Longitude coordinate of the sighting

#Trimming potential spaces in column names
column_names <- colnames(ufo_data) #Checking exisiting column names
clean_ufo_data <- ufo_data #Creating a new subset with "cleaned" col names
colnames(clean_ufo_data) <- trimws(colnames(clean_ufo_data), which = "both")
clean_col_names <- colnames(clean_ufo_data) #checking result

#Finding and replacing missing shape data
missing_shape <- filter(clean_ufo_data, shape == "")
rplcd_shapes_ufo <- mutate(clean_ufo_data, shape = replace(shape, shape == "", "unknown"))

#Finding those city values that contain country information and incorporating the country into the country column
# Finding those city values that contain country information and incorporating the country into the country column
country_updates_ufo <- mutate(rplcd_shapes_ufo, #creating new column
                              country = ifelse(country == "", #Extracting blank field
                                               str_extract(city, "\\(.*\\)"), #Accounting for special characters
                                               country))
country_updates_ufo$country <- str_replace_all(country_updates_ufo$country, "[()]", "") #removing () around country name
missing_country <- filter(country_updates_ufo, country == "") #creating a new df of only the missing countries
country_updates_ufo$country <- trimws(country_updates_ufo$country) #cleaning fields
country_updates_ufo <- filter(country_updates_ufo, country != "") #removing empty country fields


# Converting datetime and date_posted to appropriate formats
# These were originally strings and now are converted to R recognized date-time formats. 
# This will allow for operations like subtraction and and comparison
# Create new subsets with the date conversion
date_updated_ufo <- country_updates_ufo
date_updated_ufo$datetime <- as.POSIXct(date_updated_ufo$datetime, format = "%Y-%m-%d %H:%M")#converting based on format in datetime
date_updated_ufo$date_posted <- as.Date(date_updated_ufo$date_posted, format = "%d-%m-%Y")#converting based on format in date_poste

#Filtering the comments column for "fake" and "hoax"
hoax_ufo <- date_updated_ufo %>%
  filter((str_detect(tolower(comments), "hoax") | str_detect(tolower(comments), "fake"))) %>% 
  mutate(is_hoax = str_detect(tolower(comments), "hoax") | str_detect(tolower(comments), "fake"))

# Create a table reporting the percentage of hoax sightings per country
hoax_percentage_table <- hoax_ufo %>%
  group_by(country) %>% #grouping the data by country
  summarise(hoax_percentage = mean(is_hoax, na.rm = TRUE) * 100) %>% #calculating the mean of "is_hoax" because it is boolean mean = proportion
  arrange(desc(hoax_percentage)) # arranging the values in a descending fashion

print(hoax_percentage_table)

report_delay <- date_updated_ufo %>% #if one accounts for time some delays will be negative due to no time in datetime
  mutate(report_delay = difftime(date_posted, datetime, units = "days" )) %>%   #mutated a new col report_delay, from the output of calculating the difference in time between date_posted and datetime, making output in days
  mutate(report_delay = round(report_delay, 2))
#removing rows where observation was reported before it happened
report_delay <- report_delay %>%
  filter(report_delay >= 0)

# Creating a table reporting the average report_delay per country
average_report_delay <- report_delay %>%
  group_by(country) %>% #grouping the data by country
  summarise(avg_report_delay = mean(report_delay, na.rm = TRUE)) %>% #calculating the mean of report_delay
  arrange(desc(avg_report_delay)) # arranging the values in a descending fashion
average_report_delay #viewing the table

#Checking data quality of duration seconds
# Filtering out rows with missing or non-numeric values in the "duration seconds" column
filtered_dursec <- clean_ufo_data %>%
  filter(!is.na(`duration.seconds`), is.numeric(`duration.seconds`))
summary(filtered_dursec$`duration.seconds`) #huge range
boxplot(filtered_dursec$`duration.seconds`, main = "Boxplot of Duration Seconds", ylab = "Seconds")
# The distribution of the data appears to have significant right skew! 
# Create a new subset of ufo_data
log_dursec <- clean_ufo_data %>%
  mutate(log.duration.seconds = log(`duration.seconds` + 1)) # Adding a new column with log-transformed values to adjust for the skew
#Creating two histograms to compare the difference in distribution
library(ggplot2)

# Create a histogram of the log-transformed "duration.seconds" data

# Create a histogram of the "duration.seconds" data
hist(clean_ufo_data$duration.seconds, xlab= "Duration in seconds", ylab = "Frequency", main = "Hisogram of UFO Observation Durations") # This is not very informative, a log transformation will help. 

ggplot(log_dursec, aes(x = log.duration.seconds)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  theme_minimal() +
  labs(x = "Log(Duration in seconds)", y = "Frequency", title = "Histogram of Log-transformed UFO Observation Durations (s)")

#' Peer Reviewer Shawn's Comments ####

#' Required Functionality:
#' The dataset is correctly read into a data frame, with the headers correctly shown, the class confirmed, and no whitespace evident
#' Good use of class() to prove that the format is correct
#' Clever use of trimws with "both" specified; this checks for whitespace before and after the column names
#' Rows missing values for "Shape" are shown in a dataset; this meets the requirement of finding rows where said values are missing
#' "unknown" is correctly imputed into cells missing "shape" information; good use of replace() within mutate() 
#' Great conversion of datetime and date_posted using as.POSIXct() and as.Date(); the output appears correct
#' Clever use of str_detect(), which returns Boolean values, to identify hoaxes in the comments and populate the is_hoax column with "TRUE"s
#' At the same time, the is_hoax column actually also has to display the "FALSE"s (per the assignment requirements)
#' I don't seem to be getting the hoax_percentage_table intended: the hoax_percentage column shows "100"s
#' The report_delay column is correctly added with properly calculated differences (in days);
#' good use of difftime() with "days" specified and rounding to 2 decimal places 
#' also great use of mean() and arrange() to find the average of the data and organize it, respectively
#' 

#' Bonus Functionality:
#' Superior use of extract functions to fill the "country" column, Owen; the process is logical and the comments help justify each related line of code
#' Very creative and thoughtful strategy of extracting based on the parentheses, placing the text (country name) within those into the "country" column, then removing the brackets
#' I see that most of the country names were correctly inserted into the "country" column, with some exceptions;
#' these include cases where there were multiple parenthesized texts or when the fetched text is not a country name but a series of random characters (e.g., 49.07xx&#176;n&#44 1.39xx&#176;e)
#' as a way to improve this, we could use regex to grab only letters, e.g. introducing [A-za-z])
#' Overall, you did a great job, Owen! This is no small challenge and the code you have is a terrific achievement


#' Style and Organization:
#' 
#' 
#' 
#' 

#' Recommendations:
#' Line 8: code can be shortened as some specifications are the default ("header = TRUE") so can be taken out;
#' as an alternative, you could have used read.csv() for concision (no need to specify the "sep" part)
#' Line 10: to simplify the code, we could avoid creating a copy (new variable) of "og_ufo_data" and instead use "og_ufo_data" for the entirety of the code
#' Line 11: maybe add a justification for this line of code; the "print()" part may not be needed as without it, we still see the desired head rows
#' Lines 27 and 30: we don't have to define a new variable to visualize results; just having the code you want visualized will do (e.g., just having "colnames(ufo_data)" in one line will yield the output in Console, and that would suffice and prevent crowding the "Environment")
#' Line 33: to numerically show the rows where "Shape" values are missing, we could use: which(clean_ufo_data$shape == "")
#' General:
#' To streamline and shorten the code, we could add more pipes in the first few sections of your code
#' Some comments can be shorter or taken out (e.g., lines 36 and 37 are duplicates)

#' Final Thoughts:
#'  
#' 
#' 
#' 