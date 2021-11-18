###Raam Sivakumar-BINF*6210-Assignment 1 Code

###Attribution: Raam Sivakumar chose the question and organism group. He decided appropriate data filtering and quality control steps. He created all graphs under the section Plotting.  
  ##Amy edited the Treemap plot, changed the order of some data filtering commands, and added a statistical test. 
  ##How this breaks down in the script: Lines 5 to 123 (exlcuding lines 86 to 98) were created by Raam. Lines 124 to 153 and lines 86 to 98 were created by Amy. 
  ##Amy edited line 11, lines 59 to 61, lines 78 to 80, and lines 102 to 109. 

###Loading Packages----

library(vroom)
library(tidyverse)
library(treemap)
library(ggplot2)
library(RColorBrewer) #treemap automatically accepts colourbrewer colours!

###Reading and Checking Data----

#This code uses the BOLD database API to call the Salamandridae tab-separated value records into the global environment, write to the user's local directory and then calls the local .tsv file.
#Vroom was used as it is faster than read_tsv in my cases but it works the same way
raw_data <- vroom("http://www.boldsystems.org/index.php/API_Public/combined?taxon=Salamandridae&format=tsv")
write_tsv(raw_data, "Raw_Salamandridae_Data.tsv")
raw_data <- vroom("Raw_Salamandridae_Data.tsv")

#General checking of the .tsv raw data. Involves checking the class and length (number of columns) of the data frame as well as showing all the column headers using names() and descriptive statistics for each one with summary()
class(raw_data)
length(raw_data)
dim(raw_data)
names(raw_data)
summary(raw_data)

###General Statistics and Filtering of Raw Data----

#The raw data is filtered for NA values in the BIN, country and latitude columns before being piped into a new data frame called trimmed_data. These columns will be of importance downstream.
trimmed_data <- raw_data %>%
  filter(!is.na(bin_uri)) %>%
  filter(!is.na(country)) %>%
  filter(!is.na(lat)) %>%
  select(bin_uri,species_name,lat,country)

#Another general check of the new data frame to make sure none of the columns are missing and the row values are unchanged. Also includes summary statistics of filtered data.
class(trimmed_data)
dim(trimmed_data)
summary(trimmed_data)

#Preliminary results from the new data frame by looking at specific missing values. This was particularly troubling as many of the specimen records lacked at least one "lat", "bin_uri" or "country" value.
bin_richness <- length(unique(trimmed_data$bin_uri))
no_bin_specimens <- sum(is.na(raw_data$bin_uri))
no_country_specimens <- sum(is.na(raw_data$country))
no_lat_specimens <- sum(is.na(raw_data$lat))

#Printing to the user clearly that the dataset has records missing important BIN, lat and country values.
cat("Total bins:", bin_richness,",",
           "Unassigned bins:", no_bin_specimens, ",",
           "Specimens with unassigned countries:",no_country_specimens, ",",
           "Specimens with unassigned latitudes:", no_lat_specimens)

###Data Processing----

#Generating new data frame for specimens obtained from all countries and grouping by country to see bin richness per country. General checking of data frame generation is also seen below
all_countries <- trimmed_data %>%
  group_by(country) %>%
  count(bin_uri) %>% #bin_uri has been grouped by country, count() counts the number of times xyz bin occurs in xyz country (but does not include 0 occurrences)
  group_by(country) %>%
  count(country) #counts the number of times each country appears in the dataframe (which equals the number of bins that occur in that country)
class(all_countries)
dim(all_countries)
attributes(all_countries)

#Making and printing a table showing the number of BINS per country as well as general checking to make sure the table was generated correctly with the appropriate number of variables (countries)
specimens_by_country <- table(trimmed_data$country)
class(specimens_by_country)
dim(specimens_by_country)
attributes(specimens_by_country)
specimens_by_country

#Generating new data frame for specimens obtained from Mediterranean latitude (between 30 and 40 degrees north of Equator) and grouping by country to see bin richness per country. General checking of data frame generation is also seen below
mediterranean_bins <- trimmed_data %>%
  filter(between(lat,30,40)) %>%
  group_by(country) %>%
  count(bin_uri) %>%
  group_by(country) %>%
  count(country)
class(mediterranean_bins)
dim(mediterranean_bins)
attributes(mediterranean_bins)

###Plotting----

#Add a new column to all_countries that has the Country name, Bin Richness value (i.e. "China, 34"), for treemap
#https://stackoverflow.com/questions/29253844/r-treemap-how-to-add-multiple-labels/29254734
all_countries$labels <- paste(all_countries$country, all_countries$n, #"combine" both columns
                              sep = "\n") #separator = newline character

#Add a new column to all_countries that indicates whether the country is from a mediterranean latitude (based on if the country name is also found in mediterranean_bins)--for treemap
for (i in 1:nrow(all_countries)) { #iterate through all rows of all_countries
  if (all_countries$country[i] %in% mediterranean_bins$country){ #if country name is found in mediterranean_bins
    all_countries$mediterranean[i] = "Yes" #add Yes
  } else {
    all_countries$mediterranean[i] = "No" #otherwise add No
  }
}

#Tree map visualizing the distribution of bin richness across all the available countries within the dataset
Treemap <- treemap(all_countries,index="labels", #changed labels from all_countries$country to all_countries$labels
                   vSize="n",type="categorical", #able to colour boxes based on categorical variables
                   vColor = "mediterranean", #colours based on whether the country is mediterranean or not (lat 30-40)
                   palette = "Set2", #colourblind friendly colourbrewer palette
                   fontsize.labels = 10, #labels are longer now, so I made the fontsize a bit smaller
                   title.legend = "Does the country contain Mediterranean latitudes?", #change legend name
                   position.legend = "bottom", #sticks the legend at the bottom
                   title="Visualization of Salamandridae Bin Richness Distribution", aspRatio = 1.5)

#Boxplot showing the distribution of bin richness across different latitudes among various countries
Boxplot <- ggplot(trimmed_data, aes(x=country, y=lat)) +
  geom_boxplot(width = 0.8, color = "black") + coord_flip() +
  theme_minimal() + theme(text=element_text(size=18))
Boxplot + labs(title="Distribution of Salamandridae Specimen Latitudes Across Countries",
               y="Latitude", x="Country")

#Barplot showing the distribution of bin richness across different countries within the Mediterranean latitude
Barplot <- ggplot(mediterranean_bins, aes(x=country, y=n)) +
  geom_bar(stat = "identity",width = 0.8, color = "black",fill = "black") + coord_flip() + 
  theme_minimal() + theme(text=element_text(size=18))
Barplot + labs(title="Distribution of Salamandridae Bin Richness Among Countries Across Mediterranean Cimate Latitude",
  y="Bin Richness", x="Country")

###Statistical Test: Mediterranean vs Non-Mediterranean----

#Are the data normally distributed? (Shapiro-Wilk)
shapiro.test(all_countries$n[all_countries$mediterranean == "Yes"]) #No
shapiro.test(all_countries$n[all_countries$mediterranean == "No"]) #Also no

#Transform values to try to make them normally distributed
all_countries$log <- log(all_countries$n)

#Are these new data normally distributed? 
shapiro.test(all_countries$log[all_countries$mediterranean == "Yes"]) #No
shapiro.test(all_countries$log[all_countries$mediterranean == "No"]) #Also no
#Ok, non-parametric it is!

#Test if the two distributions are the same with Kolmogorov–Smirnov test
ks.test(all_countries$n[all_countries$mediterranean == "Yes"], all_countries$n[all_countries$mediterranean == "No"])
#p = 0.9... I think this means that their distributions are similar enough to satisfy the assumptions of a Kruskal-Wallis test

#Perform Kruskal-Wallis test comparing Mediterranean and non-Mediterranean
kruskal.test(all_countries$log~all_countries$mediterranean)
#Results are not statistically significant (X2 = 0.14, p = 0.70), so do not need to do a Dunn test

#Graph the results in a boxplot (even though they are not significantly different)
ggplot(all_countries, aes(mediterranean, n)) +
  geom_boxplot(aes(fill = mediterranean), show.legend = FALSE) +
  scale_fill_brewer(palette="Set2") +
  labs(title = "Distribution of bin richness between countries with and without Mediterranean latitudes",
       y = "Bin Richness",
       x = "Does the country have a Mediterranean latitude?") +
  theme_bw()
