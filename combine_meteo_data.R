# Install packages once and load them using library everytime you reopen the project

install.packages("janitor")          # Easy data cleaning, especially for column names and tables
install.packages("tidyverse")        # Core collection of R packages for data science (ggplot2, dplyr, tidyr, etc.)
install.packages("knitr")            # Dynamic report generation with R Markdown and code chunks
install.packages("recipes")          # Preprocessing pipeline for modeling: normalization, encoding, etc.
install.packages("bookdown")         # Create books and long reports with R Markdown
install.packages("patchwork")        # Combine multiple ggplot2 plots into one figure/layout
install.packages("lubridate")        # Make it easy to work with dates and times
install.packages("skimr")            # Summarizes dataframes with pretty, compact stats
install.packages("naniar")           # Visualizes and handles missing data easily
install.packages("visdat")           # Visualize structure and missingness in a dataset
install.packages("mice")             # Multiple imputation of missing values using chained equations
install.packages("zoo")              # Infrastructure for time series data (especially irregular series)
install.packages("ggridges")         # Create ridgeline (joy) plots to compare distributions across groups
install.packages("sf")               # Modern spatial data format for handling geospatial vector data
install.packages("rnaturalearth")    # Country/region maps for plotting and spatial analysis
install.packages("rnaturalearthdata")# Companion data package with the actual map files for rnaturalearth
install.packages("gstat")            # Geostatistics: kriging, IDW interpolation, variograms, etc.
install.packages("raster")           # Read, write, and process raster (grid) spatial data
install.packages("viridis")          # Color palettes optimized for colorblind accessibility
install.packages("gganimate")        # Animate ggplot2 graphics (e.g., time series animations)
install.packages("transformr")       # Helper for transitions in gganimate animations
install.packages("sp")               # Legacy spatial package, required by older geospatial packages like gstat
install.packages("scales")           # Format numbers, axis labels, colors in plots
install.packages("car")              # Companion to Applied Regression: includes VIF, ANOVA, diagnostics
install.packages("lmtest")           # Diagnostic tests for linear regression models (e.g., heteroskedasticity)
install.packages("broom")            # Convert model outputs into tidy dataframes for analysis/reporting
install.packages("psych")            # Tools for psychological research, EFA, descriptive stats, reliability
install.packages("forecast")         # Time series forecasting models (ARIMA, ETS, etc.)
install.packages("splines")          # Fit regression models using spline (nonlinear) basis functions
install.packages("ggrepel")
install.packages("e1071")
install.packages("moments")
install.packages("ggthemes")
install.packages("randomForest")
install.packages("caret")
install.packages("boot")
install.packages("sandwich")


# Loading all necessary libraries

# For data cleaning & wrangling
library(janitor)        # Data cleaning tools
library(tidyverse)      # Core data science packages (dplyr, ggplot2, etc.)
library(lubridate)      # Dates and times
library(skimr)          # Summary statistics
library(naniar)         # Missing value visualization
library(visdat)         # Data structure & missing data visualization
library(zoo)            # Irregular time series support
library(recipes)        # Feature engineering
library(moments)
library(knitr)          # Dynamic reporting
library(bookdown)       # Book writing with R Markdown
library(ggplot2)        # Core plotting system
library(patchwork)      # Combine ggplots
library(ggridges)       # Ridgeline plots
library(gganimate)      # Animation for ggplot
library(transformr)     # Transitions for animations
library(viridis)        # Colorblind-friendly palettes
library(scales)         # Formatting plot axes, legends
library(ggrepel)
library(e1071)
library(ggthemes)
library(sf)             # Modern geospatial vector handling
library(sp)             # Legacy spatial support
library(rnaturalearth)  # Country/region vector maps
library(rnaturalearthdata) # Map data files
library(raster)         # Raster spatial data
library(gstat)          # Spatial interpolation, Kriging
library(car)            # Regression diagnostics, VIF
library(lmtest)         # Hypothesis testing in linear models
library(broom)          # Convert models to tidy data frames
library(psych)          # Descriptive stats, EFA, psychometrics
library(forecast)       # Time series models like ARIMA
library(splines)        # Spline-based regression models
library(mice)
library(forecast)
library(randomForest)
library(caret)
library(dplyr)
library(lmtest)
library(sandwich)
library(car)       # for VIF
library(ggplot2)
library(boot) 
library(sandwich)
search()



# Setting my working directory (adjust this path)
setwd("/Users/johnrohith/Library/CloudStorage/OneDrive-TUSMM/Applied Business Analytics ANLY 09006/R Project/R Project/First Final Research Project")
getwd()


# Listing all CSV files in the folder
csv_files <- list.files(pattern = "*.csv")
print(csv_files)


#Uploading the dataset
prec_data <- read.csv("MTM05.20250520T230553.csv") #Precipitation Dataset
temp_data <- read.csv("MTM06.20250520T230507.csv") #Temparature Dataset
rain_data <- read.csv("MTM01.20250520T230519.csv") #Rainfall Dataset
emis_data <- read.csv("EAA19.20250815T130801.csv") #Sectoral based Air Pollution Emissions Dataset
hosp_data <- read.csv("DHA89.20250520T080997.csv") #Hospital Dataset



#Summarising our dataframe
summary(prec_data)
summary(temp_data)
summary(rain_data)
summary(emis_data)
summary(hosp_data)

#Skimming through our dataframe
skim(prec_data)
skim(temp_data)
skim(rain_data)
skim(emis_data)
skim(hosp_data)

#Just looking at the dataset
head(prec_data)
head(temp_data)
head(rain_data)
head(emis_data)
head(hosp_data)

colnames(emis_data)
lapply(emis_data[,c("Statistic.Label","Year","NACE.Rev..2.Sector","UNIT")],unique)



#Clean Column Names. This is to ensure column access consistent and easier.
prec_data  <- clean_names(prec_data)
temp_data  <- clean_names(temp_data)
rain_data  <- clean_names(rain_data)
emis_data  <- clean_names(emis_data)
hosp_data  <- clean_names(hosp_data)




# Part 1 : Exploratory Data Analysis of our dataset
# 1.1 Plotting box plots to find the spread of our precipitation

# Step 1: Clean and prepare the data
prec_cleaned <- prec_data %>%
    filter(!is.na(value), value > 0)  # remove missing and non-positive values

# Step 2: Plot the boxplot
ggplot(prec_cleaned, aes(x = reorder(meteorological_weather_station, value, FUN = median), y = value)) +
    geom_boxplot(outlier.color = "red", fill = "lightblue") +
    labs(
        title = "Spread of Precipitation Across Weather Stations",
        x = "Weather Station",
        y = "Precipitation (mm)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", hjust = 0.5),
        panel.border = element_rect(color = "black", fill = NA),
        panel.background = element_blank()
    ) +
    scale_y_continuous(labels = scales::comma_format())



# 1.2. Visualizing the trend of precipitation data
# Step 1: Extract year and clean
prec_clean <- prec_data %>%
    mutate(year = as.numeric(str_extract(month, "^\\d{4}"))) %>%
    filter(!is.na(year), !is.na(value))


# Step 2: Aggregating by station and year
prec_yearly <- prec_clean %>%
    group_by(meteorological_weather_station, year) %>%
    summarise(avg_precip_mm = mean(value, na.rm = TRUE), .groups = "drop")

# Step 3: Plotting with one panel per station
ggplot(prec_yearly, aes(x = year, y = avg_precip_mm)) +
    geom_line(color = "steelblue", linewidth = 0.4) +
    facet_wrap(~ meteorological_weather_station, scales = "free_y") +
    labs(
        title = "Yearly Avg.Trends in Precipitation by Station",
        subtitle = "Phoenix Park, Roches Point, and Markee have a lot of missing data.",
        x = "Year",
        y = "Average Precipitation (mm)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
        strip.text = element_text(face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
    )


# 1.3 Detection of Outliers in and plotting our temparature dataset 
# Step 1: Detect outliers in temp_data
outliers_temp <- temp_data %>%
    filter(!is.na(value)) %>%
    group_by(statistic_label) %>%
    mutate(
        Q1 = quantile(value, 0.25),
        Q3 = quantile(value, 0.75),
        IQR = Q3 - Q1,
        is_outlier = value < (Q1 - 1.5 * IQR) | value > (Q3 + 1.5 * IQR)
    ) %>%
    filter(is_outlier) %>%
    ungroup()


# Step 2: Plot with geom_text_repel() for labeling outliers
ggplot(temp_data, aes(x = statistic_label, y = value)) +
    geom_boxplot(outlier.color = "red", outlier.shape = 16, fill = "#9ecae1") +
    geom_text_repel(
        data = outliers_temp,
        aes(label = meteorological_weather_station),
        size = 2.3,
        color = "black",
        max.overlaps = 100,
        box.padding = 0.3,
        segment.color = "grey50"
    ) +
    labs(
        title = "Boxplot of Temperature Values by Statistic Label",
        subtitle = "Outliers labeled using Station Names using (ggrepel)",
        x = "Statistic Label",
        y = "Temperature (°C)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
    )

# 1.4 Visualizing different segments of our temp_data. - Mean maximum temparature

# Step 1 : For the line graph
mean_max_data <- temp_data %>%
    filter(statistic_label == "Mean Maximum Temperature") %>%
    mutate(year = as.numeric(str_extract(month, "^\\d{4}"))) %>%
    filter(!is.na(year))

# Step 2 : Missingness Data
ggplot(mean_max_data, aes(x = year, y = value)) +
    geom_line(color = "steelblue", linewidth = 0.4) +
    facet_wrap(~ meteorological_weather_station, scales = "free_y") +
    labs(
        title = "Yearly Mean Maximum Temperature by Station",
        subtitle = "Phoenix Park, Roches Point, and Markee have a lot of missing data.",
        x = "Year", y = "Temperature (°C)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
        strip.text = element_text(face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
    )

# Step 3 : Missingness Heatmap
mean_max_miss <- temp_data %>%
    filter(statistic_label == "Mean Maximum Temperature") %>%
    separate(month, into = c("year", "month_name"), sep = " ", convert = TRUE) %>%
    mutate(
        month_name = trimws(month_name),
        is_missing = is.na(value),
        year = as.character(year),
        meteorological_weather_station = as.character(meteorological_weather_station),
        month = factor(month_name, levels = month.name, labels = month.abb)
    )

# Step 4 : Visualizing Missingness
ggplot(mean_max_miss, aes(x = month, y = factor(year), fill = is_missing)) +
    geom_tile(color = "grey80") +
    facet_wrap(~ meteorological_weather_station, ncol = 4) +
    scale_fill_manual(values = c("FALSE" = "white", "TRUE" = "red"),
                      labels = c("Present", "Missing"),
                      name = "Data Status",
                      guide = guide_legend(override.aes = list(color = "black"))) +
    labs(
        title = "Monthly Missing Data: Heatmap for Mean Maximum Temperature",
        subtitle = "Markee, Phoenix Park, and Roches Point have significant missing data",
        x = "Month", y = "Year"
    ) +
    theme_minimal(base_size = 10) +
    theme(
        axis.text.y = element_text(size = 5.5),
        axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        legend.key = element_rect(color = "black"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
    )


# 1.5 Visualising the Mean minimum temparature 

# Step 1 : For the line graph
mean_min_data <- temp_data %>%
    filter(statistic_label == "Mean Minimum Temperature") %>%
    mutate(year = as.numeric(str_extract(month, "^\\d{4}"))) %>%
    filter(!is.na(year))

# Step 2 : Missingness Data
ggplot(mean_min_data, aes(x = year, y = value)) +
    geom_line(color = "steelblue", linewidth = 0.4) +
    facet_wrap(~ meteorological_weather_station, scales = "free_y") +
    labs(
        title = "Yearly Mean Minimum Temperature by Station",
        subtitle = "Phoenix Park, Roches Point, and Markee have a lot of missing data.",
        x = "Year", y = "Temperature (°C)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
        strip.text = element_text(face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
    )

# Step 3 : Missingness Heatmap
mean_min_miss <- temp_data %>%
    filter(statistic_label == "Mean Minimum Temperature") %>%
    separate(month, into = c("year", "month_name"), sep = " ", convert = TRUE) %>%
    mutate(
        month_name = trimws(month_name),
        is_missing = is.na(value),
        year = as.character(year),
        meteorological_weather_station = as.character(meteorological_weather_station),
        month = factor(month_name, levels = month.name, labels = month.abb)
    )

# Step 4 : Visualizing Missingness
ggplot(mean_min_miss, aes(x = month, y = factor(year), fill = is_missing)) +
    geom_tile(color = "grey80") +
    facet_wrap(~ meteorological_weather_station, ncol = 4) +
    scale_fill_manual(values = c("FALSE" = "white", "TRUE" = "red"),
                      labels = c("Present", "Missing"),
                      name = "Data Status",
                      guide = guide_legend(override.aes = list(color = "black"))) +
    labs(
        title = "Monthly Missing Data: Mean Minimum Temperature",
        subtitle = "Markee, Phoenix Park, and Roches Point have significant missing data",
        x = "Month", y = "Year"
    ) +
    theme_minimal(base_size = 10) +
    theme(
        axis.text.y = element_text(size = 5.5),
        axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        legend.key = element_rect(color = "black"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
    )


# ----- 3. Mean Air Temperature -----

# Step 1 : For the line graph
mean_air_data <- temp_data %>%
    filter(statistic_label == "Mean Air Temperature") %>%
    mutate(year = as.numeric(str_extract(month, "^\\d{4}"))) %>%
    filter(!is.na(year))

# Step 2 : Missingness Data
ggplot(mean_air_data, aes(x = year, y = value)) +
    geom_line(color = "steelblue", linewidth = 0.4) +
    facet_wrap(~ meteorological_weather_station, scales = "free_y") +
    labs(
        title = "Yearly Mean Air Temperature by Station",
        subtitle = "Phoenix Park, Roches Point, and Markee have a lot of missing data.",
        x = "Year", y = "Temperature (°C)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
        strip.text = element_text(face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
    )

# Step 3 : Missingness Heatmap
mean_air_miss <- temp_data %>%
    filter(statistic_label == "Mean Air Temperature") %>%
    separate(month, into = c("year", "month_name"), sep = " ", convert = TRUE) %>%
    mutate(
        month_name = trimws(month_name),
        is_missing = is.na(value),
        year = as.character(year),
        meteorological_weather_station = as.character(meteorological_weather_station),
        month = factor(month_name, levels = month.name, labels = month.abb)
    )

# Step 4 : Visualizing Missingness

ggplot(mean_air_miss, aes(x = month, y = factor(year), fill = is_missing)) +
    geom_tile(color = "grey80") +
    facet_wrap(~ meteorological_weather_station, ncol = 4) +
    scale_fill_manual(values = c("FALSE" = "white", "TRUE" = "red"),
                      labels = c("Present", "Missing"),
                      name = "Data Status",
                      guide = guide_legend(override.aes = list(color = "black"))) +
    labs(
        title = "Monthly Missing Data: Mean Air Temperature",
        subtitle = "Markee, Phoenix Park, and Roches Point have significant missing data",
        x = "Month", y = "Year"
    ) +
    theme_minimal(base_size = 10) +
    theme(
        axis.text.y = element_text(size = 5.5),
        axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        legend.key = element_rect(color = "black"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
    )


# ----- 4. Maximum Air Temperature -----
max_air_data <- temp_data %>%
    filter(statistic_label == "Maximum Air Temperature") %>%
    mutate(year = as.numeric(str_extract(month, "^\\d{4}"))) %>%
    filter(!is.na(year))

ggplot(max_air_data, aes(x = year, y = value)) +
    geom_line(color = "steelblue", linewidth = 0.4) +
    facet_wrap(~ meteorological_weather_station, scales = "free_y") +
    labs(
        title = "Yearly Maximum Air Temperature by Station",
        subtitle = "Phoenix Park, Roches Point, and Markee have a lot of missing data.",
        x = "Year", y = "Temperature (°C)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
        strip.text = element_text(face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
    )


# Repeat similar pattern for missingness plot…

# ----- 5. Minimum Air Temperature -----
# Repeat both yearly trend and missingness plots

# ----- 6. Grass Minimum Temperature -----
# Repeat both yearly trend and missingness plots



# 2.0 EDA of our rain_data dataset. 
# Separating our rain_data into years and month for future use
rain_data <- rain_data %>%
    mutate(
        year = as.numeric(substr(month, 1, 4)),
        month_number = as.numeric(substr(month, 6, 7)),
        month = factor(month_number, levels = 1:12, labels = month.abb),
        meteorological_weather_station = as.character(meteorological_weather_station)
    )

# Outliers detections
# Calculating IQR, Q1, Q3, and mark outliers
rain_with_iqr <- rain_data %>%
    filter(!is.na(value)) %>%
    group_by(statistic_label) %>%
    mutate(
        Q1 = quantile(value, 0.25),
        Q3 = quantile(value, 0.75),
        IQR = Q3 - Q1,
        is_outlier = value < (Q1 - 1.5 * IQR) | value > (Q3 + 1.5 * IQR)
    ) %>%
    ungroup()



ggplot(rain_data, aes(x = statistic_label, y = value)) +
    geom_boxplot(
        fill = "#a1d99b",
        outlier.colour = "red",
        outlier.shape = 16,
        outlier.size = 1.5,
        width = 0.6
    ) +
    labs(
        title = "Boxplot of Rainfall Statistics (Outliers Highlighted)",
        subtitle = "Red dots represent outliers detected using IQR",
        x = "Rainfall Statistic",
        y = "Value (mm/days)"
    ) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 30, hjust = 1),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7)
    )


    


# 2.1 ----- 1. Total Rainfall -----
total_rainfall_data <- rain_data %>%
    filter(statistic_label == "Total Rainfall", !is.na(year), is.finite(value))

ggplot(total_rainfall_data, aes(x = year, y = value)) +
    geom_line(color = "steelblue", linewidth = 0.4) +
    facet_wrap(~ meteorological_weather_station, scales = "free_y") +
    labs(
        title = "Yearly Total Rainfall by Station",
        subtitle = "Galway misses the most whereas Clones, Kilkenny,Rosslare are missing a significant amount.",
        x = "Year", y = "Rainfall (mm)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
        strip.text = element_text(face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
    )

total_rainfall_miss <- rain_data %>%
    filter(statistic_label == "Total Rainfall") %>%
    mutate(is_missing = is.na(value), year = as.character(year))

ggplot(total_rainfall_miss, aes(x = month, y = factor(year), fill = is_missing)) +
    geom_tile(color = "grey80") +
    facet_wrap(~ meteorological_weather_station, ncol = 4) +
    scale_fill_manual(values = c("FALSE" = "white", "TRUE" = "red"),
                      labels = c("Present", "Missing"),
                      name = "Data Status",
                      guide = guide_legend(override.aes = list(color = "black"))) +
    labs(
        title = "Monthly Missing Data: Total Rainfall",
        subtitle = "Galway misses the most whereas Clones, Kilkenny,Rosslare are missing a significant amount",
        x = "Month", y = "Year"
    ) +
    theme_minimal(base_size = 10) +
    theme(
        axis.text.y = element_text(size = 5.5),
        axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        legend.key = element_rect(color = "black")
    )

# 2.2 ----- 2. Most Rainfall in a Day -----
most_rainfall_data <- rain_data %>%
    filter(statistic_label == "Most Rainfall in a Day", !is.na(year), is.finite(value))

ggplot(most_rainfall_data, aes(x = year, y = value)) +
    geom_line(color = "steelblue", linewidth = 0.4) +
    facet_wrap(~ meteorological_weather_station, scales = "free_y") +
    labs(
        title = "Yearly Most Rainfall in a Day by Station",
        subtitle = "Phoenix Park, Roches Point, and Markee have a lot of missing data.",
        x = "Year", y = "Rainfall (mm)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
        strip.text = element_text(face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
    )

most_rainfall_miss <- rain_data %>%
    filter(statistic_label == "Most Rainfall in a Day") %>%
    mutate(is_missing = is.na(value), year = as.character(year))

ggplot(most_rainfall_miss, aes(x = month, y = factor(year), fill = is_missing)) +
    geom_tile(color = "grey80") +
    facet_wrap(~ meteorological_weather_station, ncol = 4) +
    scale_fill_manual(values = c("FALSE" = "white", "TRUE" = "red"),
                      labels = c("Present", "Missing"),
                      name = "Data Status",
                      guide = guide_legend(override.aes = list(color = "black"))) +
    labs(
        title = "Monthly Missing Data: Most Rainfall in a Day",
        subtitle = "Most stations have a significant amount of data missing",
        x = "Month", y = "Year"
    ) +
    theme_minimal(base_size = 10) +
    theme(
        axis.text.y = element_text(size = 5.5),
        axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        legend.key = element_rect(color = "black"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
    )

# 2.3 ----- 3. Raindays (0.2mm or More) -----
raindays_data <- rain_data %>%
    filter(statistic_label == "Raindays (0.2mm or More)", !is.na(year), is.finite(value))

ggplot(raindays_data, aes(x = year, y = value)) +
    geom_line(color = "steelblue", linewidth = 0.4) +
    facet_wrap(~ meteorological_weather_station, scales = "free_y") +
    labs(
        title = "Yearly Raindays (0.2mm or More) by Station",
        subtitle = "Phoenix Park, Roches Point, and Markee have a lot of missing data.",
        x = "Year", y = "Number of Raindays"
    ) +
    theme_minimal(base_size = 13) +
    theme(
        strip.text = element_text(face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
    )

raindays_miss <- rain_data %>%
    filter(statistic_label == "Raindays (0.2mm or More)") %>%
    mutate(is_missing = is.na(value), year = as.character(year))

ggplot(raindays_miss, aes(x = month, y = factor(year), fill = is_missing)) +
    geom_tile(color = "grey80") +
    facet_wrap(~ meteorological_weather_station, ncol = 4) +
    scale_fill_manual(values = c("FALSE" = "white", "TRUE" = "red"),
                      labels = c("Present", "Missing"),
                      name = "Data Status",
                      guide = guide_legend(override.aes = list(color = "black"))) +
    labs(
        title = "Monthly Missing Data: Raindays (0.2mm or More)",
        subtitle = "Significant missing data observed",
        x = "Month", y = "Year"
    ) +
    theme_minimal(base_size = 10) +
    theme(
        axis.text.y = element_text(size = 5.5),
        axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        legend.key = element_rect(color = "black"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
    )

# Visualizing Decades with the highest rainfall
# Step 1: Filter Total Rainfall and create decade column
rain_total <- rain_data %>%
    filter(statistic_label == "Total Rainfall", !is.na(value)) %>%
    mutate(decade = paste0(floor(year / 10) * 10, "s"))

# Step 2: Get the max rainfall per decade
rain_max_decade <- rain_total %>%
    group_by(decade) %>%
    slice_max(value, n = 1, with_ties = FALSE) %>%
    ungroup()

# Step 3: Arrange in descending order of rainfall and fix factor levels for plotting
rain_max_decade <- rain_max_decade %>%
    arrange(desc(value)) %>%
    mutate(decade = factor(decade, levels = unique(decade))) # lock the order

# Step 4: Plot
ggplot(rain_max_decade, aes(x = decade, y = value)) +
    geom_col(fill = "steelblue", color = "black", width = 0.7) +
    labs(
        title = "Decade-wise Maximum Total Rainfall",
        x = "Decade",
        y = "Maximum Rainfall (mm)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        legend.position = "none"
    )

# Visualizing the missing_data by decades to help our future analysis
# Step 0 : Total amount of missing values
sum(is.na(rain_data$value))

# Step 1: Add decade column
rain_decade_data <- rain_data %>%
    mutate(decade = paste0(floor(year / 10) * 10, "s"))

# Step 2: Count missing values per decade
missing_by_decade <- rain_decade_data %>%
    group_by(decade) %>%
    summarise(missing_count = sum(is.na(value))) %>%
    arrange(desc(missing_count))

# View the table
print(missing_by_decade)

# Step 3: Visualize missing values by decade
ggplot(missing_by_decade, aes(x = reorder(decade, -missing_count), y = missing_count)) +
    geom_col(fill = "grey", color = "black", width = 0.7) +
    labs(
        title = "Missing Rainfall Records by Decade",
        x = "Decade",
        y = "Number of Missing Values"
    ) +
    theme_minimal(base_size = 13) +
    theme(
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
    )


# Spread of rainfall statistics over the years
# Total Rainfall spread
rain_data %>%
    filter(statistic_label == "Total Rainfall") %>%
    ggplot(aes(x = year, y = value)) +
    geom_point(color = "steelblue", alpha = 0.7, size = 2) +
    labs(
        title = "Total Rainfall Over the Years",
        x = "Year",
        y = "Rainfall (mm)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        plot.title = element_text(face = "bold", hjust = 0.5)
    )

# Most Rainfall in a day 
rain_data %>%
    filter(statistic_label == "Most Rainfall in a Day") %>%
    ggplot(aes(x = year, y = value)) +
    geom_point(color = "royalblue", alpha = 0.7, size = 2) +
    labs(
        title = "Total Rainfall Over the Years",
        x = "Year",
        y = "Rainfall (mm)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        plot.title = element_text(face = "bold", hjust = 0.5)
    )

# Raindays (0.2mm or More)
rain_data %>%
    filter(statistic_label == "Raindays (0.2mm or More)") %>%
    ggplot(aes(x = year, y = value)) +
    geom_point(color = "darkblue", alpha = 0.7, size = 2) +
    labs(
        title = "Raindays (0.2mm or More) Over the Years",
        x = "Year",
        y = "Number of Rain Days"
    ) +
    theme_minimal(base_size = 13) +
    theme(
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        plot.title = element_text(face = "bold", hjust = 0.5)
    )







# EDA of hosp_data. 
# Exploring our spread of data

ggplot(hosp_data, aes(x = age_group, y = value)) +
    geom_boxplot(outlier.color = "red", fill = "#c6dbef") +
    labs(
        title = "Distribution of Hospital Discharges by Age Group",
        subtitle = "Lot of missing data is observed",
        x = "Age Group", y = "Discharge Count"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
                                     )

# Ensure 'year' is numeric
hosp_data$year <- as.numeric(hosp_data$year)

# Step 1: Filter only "Both sexes", "Total Hospital Discharges", exclude unwanted areas and "All ages"
clean_hosp_data <- hosp_data %>%
    filter(
        statistic_label == "Total Hospital Discharges",
        sex == "Both sexes",
        age_group != "All ages",
        !area %in% c("Ireland", "Ireland plus non-residents", "Non-residents")
    )

# Step 2: Total hospital discharges by year (all diseases)
total_all <- clean_hosp_data %>%
    group_by(year) %>%
    summarise(Total_All_Discharges = sum(value, na.rm = TRUE), .groups = "drop")

# Step 3: Filter and calculate respiratory discharges by year
total_resp <- clean_hosp_data %>%
    filter(high_level_ishmt == "Diseases of the respiratory system (J00-J99)") %>%
    group_by(year) %>%
    summarise(Respiratory_Discharges = sum(value, na.rm = TRUE), .groups = "drop")

# Step 4: Join and compute percentage
comparison_table <- left_join(total_all, total_resp, by = "year") %>%
    mutate(
        Respiratory_Percentage = round((Respiratory_Discharges / Total_All_Discharges) * 100, 2)
    )

# Step 5: Print result
print(comparison_table)


# Verify total discharges in 2017 across counties
hosp_data %>%
    filter(
        year == 2017,
        statistic_label == "Total Hospital Discharges",
        sex == "Both sexes",
        age_group != "All ages",
        !area %in% c("Ireland", "Ireland plus non-residents", "Non-residents")
    ) %>%
    summarise(Verified_Total = sum(value, na.rm = TRUE))



# 2. County with most respiratory cases (Bar Plot for Top County Only) for both men & women. 
# ==================== Respiratory Disease Cases by County (Bar Plot) ====================

hosp_data %>%
    filter(
        age_group == "All ages",
        high_level_ishmt == "Diseases of the respiratory system (J00-J99)",
        sex %in% c("Male", "Female"),
        !area %in% c("Ireland", "Ireland plus non-residents", "Non-residents")
    ) %>%
    group_by(area) %>%
    summarise(total_cases = sum(value, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_cases)) %>%
    ggplot(aes(x = reorder(area, total_cases), y = total_cases)) +
    geom_col(fill = "#2b8cbe", width = 0.7) +
    coord_flip() +
    scale_y_continuous(labels = scales::comma) +
    labs(
        title = "Respiratory Cases by County",
        subtitle = "Before cleaning -Dublin, Cork, and Galway top the list of respiratory disease cases",
        x = "County",
        y = "Total Cases"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)
    )


# ========================================================================================



# 3. ==================== Gender-wise Respiratory Disease Cases by Year and County ====================

# Step 1: Pre-aggregate and sort counties by total cases descending
top_ordered_areas <- hosp_data %>%
    filter(
        sex != "Both sexes",
        high_level_ishmt == "Diseases of the respiratory system (J00-J99)",
        !area %in% c("Ireland", "Ireland plus non-residents", "Non-residents")
    ) %>%
    group_by(area) %>%
    summarise(total_cases = sum(value, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_cases)) %>%
    pull(area)

# Step 2: Plot with reordered factor levels
hosp_data %>%
    filter(
        sex != "Both sexes",
        high_level_ishmt == "Diseases of the respiratory system (J00-J99)",
        !area %in% c("Ireland", "Ireland plus non-residents", "Non-residents")
    ) %>%
    mutate(area = factor(area, levels = top_ordered_areas)) %>%
    group_by(sex, year, area) %>%
    summarise(total_cases = sum(value, na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(x = year, y = total_cases, fill = sex)) +
    geom_col(position = "dodge") +
    facet_wrap(~ area, scales = "free_y") +
    labs(
        title = "Respiratory Disease Cases by Gender, Year, and County",
        x = "Year", y = "Total Cases"
    ) +
    theme_minimal(base_size = 11) +
    theme(
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold", size = 9),
        plot.title = element_text(hjust = 0.5, face = "bold")
    )


# 4. Top 5 age groups most affected by respiratory diseases based on total hospital discharges. 
# First do the filtering and then summarise


top_age_groups <- hosp_data %>%
    filter(
        statistic_label == "Total Hospital Discharges",
        high_level_ishmt == "Diseases of the respiratory system (J00-J99)",
        !area %in% c("Ireland", "Ireland plus non-residents", "Non-residents"),
        sex %in% c("Male", "Female"),
        age_group != "All ages",
        !is.na(value)
    ) %>%
    group_by(age_group) %>%
    summarise(total_cases = sum(value, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_cases)) %>%
    slice_head(n = 5)

# Plot
ggplot(top_age_groups, aes(x = reorder(age_group, total_cases), y = total_cases)) +
    geom_col(fill = "#3182bd", width = 0.7) +
    coord_flip() +
    labs(
        title = "Top 5 Age Groups with Respiratory Disease Discharges",
        subtitle = "Excludes All Ages, Non-residents, and Both Sexes",
        x = "Age Group",
        y = "Total Cases"
    ) +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal(base_size = 12) +
    theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        axis.text = element_text(size = 10),
        plot.subtitle = element_text(hjust = 0.5)
    )


#5.Top 5 most affected age groups by respiratory diseases for each year, 
# Filter and summarise top 5 age groups by year
top_age_groups <- hosp_data %>%
    filter(
        high_level_ishmt == "Diseases of the respiratory system (J00-J99)",
        statistic_label == "Total Hospital Discharges",
        age_group != "All ages",
        sex %in% c("Male", "Female"),
        !area %in% c("Ireland", "Ireland plus non-residents", "Non-residents")
    ) %>%
    group_by(year, age_group) %>%
    summarise(total_cases = sum(value, na.rm = TRUE), .groups = "drop") %>%
    group_by(year) %>%
    slice_max(order_by = total_cases, n = 5) %>%
    mutate(age_group = fct_reorder(age_group, total_cases)) %>%
    ungroup()

# Plot with descending order within each year facet
ggplot(top_age_groups, aes(x = total_cases, y = age_group, fill = age_group)) +
    geom_col(show.legend = FALSE, width = 0.6) +
    facet_wrap(~ year, scales = "free_x", ncol = 3) +
    labs(
        title = "Top 5 Age Groups by Respiratory Disease Cases (Each Year)",
        x = "Total Cases", y = "Age Group"
    ) +
    scale_fill_brewer(palette = "Set2") +
    theme_minimal(base_size = 12) +
    theme(
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10)
    )




#Exploring our emissions dataset


# Ensure year is treated as a factor
emis_boxdata <- emis_data %>%
    mutate(year = factor(year))

# Create the boxplot
ggplot(emis_boxdata, aes(x = year, y = value)) +
    geom_boxplot(outlier.color = "red", fill = "#fdd0a2") +
    labs(
        title = "Boxplot of Emission Values by Year",
        subtitle = "Outliers in red indicate skewed sectors",
        x = "Year",
        y = "Emission Value (Tonnes)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
    )



#Total Emissions based on gases
# Step 1: Aggregate by statistic_label (gas type)
emis_summary <- emis_data %>%
    group_by(statistic_label) %>%
    summarise(total_emissions_tonnes = sum(value, na.rm = TRUE)) %>%
    arrange(desc(total_emissions_tonnes))

print(emis_summary)


# Step 2: Plotting
ggplot(emis_summary, aes(x = reorder(statistic_label, total_emissions_tonnes), 
                         y = total_emissions_tonnes)) +
    geom_col(fill = "darkgreen", color = "black") +
    coord_flip() +
    scale_y_continuous(
        breaks = scales::pretty_breaks(n = 10),
        labels = scales::comma_format(accuracy = 1)  # Show full numbers with commas
    ) +
    labs(
        title = "Total Emissions by Pollutant Type",
        x = "Pollutant",
        y = "Total Emissions (Tonnes)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.y = element_text(size = 10)
    )



# Let's check how the pollutants change year on year. 
# Step 1: Aggregate and order year as factor
emis_yearly <- emis_data %>%
    group_by(statistic_label, year) %>%
    summarise(
        total_emissions_tonnes = sum(value, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    mutate(year = factor(year, levels = sort(unique(year)))) # Maintain year order

# Step 2: Plotting the year-wise emissions trend
ggplot(emis_yearly, aes(x = year, y = total_emissions_tonnes,
                        color = statistic_label, group = statistic_label)) +
    geom_line(size = 1.1) +
    geom_point(size = 2) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_discrete(drop = FALSE) +  # Display all year levels
    scale_color_brewer(palette = "Set2") +
    labs(
        title = "Yearly Emissions Trend by Pollutant",
        x = "Year",
        y = "Total Emissions (Tonnes)",
        color = "Pollutant"
    ) +
    theme_minimal(base_size = 13) +
    theme(
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        plot.title = element_text(face = "bold", hjust = 0.5)
    )


# Convert values to tonnes
# --------------------------------------
# 📦 Step 1: Clean the emissions dataset
# --------------------------------------
# We exclude rows with 'total' in the sector name (case-insensitive)
emis_data_clean <- emis_data %>%
    filter(!grepl("total", nace_rev_2_sector, ignore.case = TRUE))

# ---------------------------------------------------------
# 📊 Graph: Top 10 Emitting Sectors (by total emissions)
# ---------------------------------------------------------
# Step 2: Summarise emissions by sector and get top 10
top_sectors_overall <- emis_data_clean %>%
    group_by(nace_rev_2_sector) %>%
    summarise(total_emissions = sum(value, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_emissions)) %>%
    slice_max(total_emissions, n = 10)

# Step 3: Plot
ggplot(top_sectors_overall, aes(x = fct_reorder(nace_rev_2_sector, total_emissions),
                                y = total_emissions)) +
    geom_bar(stat = "identity", fill = "#2c7fb8") +
    coord_flip() +
    scale_y_continuous(labels = scales::comma) +
    labs(
        title = "Top 10 Emitting Sectors (Overall)",
        x = "Sector",
        y = "Emissions (Tonnes)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        plot.title = element_text(face = "bold", hjust = 0.5)
    )



# -----------------------------------------------
# Step 1: Filter only manufacturing & printing sectors in 2010
# -----------------------------------------------
emis_2010 <- emis_data %>%
    filter(year == 2010) %>%
    filter(str_detect(nace_rev_2_sector, regex("manufacturing|printing|manufacture", ignore_case = TRUE)))

# ------------------------------------------------
# Step 2: Summarise emissions and select Top 5
# ------------------------------------------------
emis_2010_summary <- emis_2010 %>%
    group_by(nace_rev_2_sector) %>%
    summarise(total_emissions = sum(value, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_emissions)) %>%
    slice_max(total_emissions, n = 5) %>%
    mutate(sector_wrapped = stringr::str_wrap(nace_rev_2_sector, width = 25)) # Wrap sector names for readability

# --------------------------------------------
#  Step 3: Plot Top 5 Manufacturing Sectors (2010)
# --------------------------------------------
ggplot(emis_2010_summary, aes(
    x = fct_reorder(sector_wrapped, total_emissions),
    y = total_emissions,
    fill = sector_wrapped  # Categorical fill for visual separation
)) +
    geom_col(width = 0.5, show.legend = FALSE) +
    coord_flip() +
    geom_text(aes(label = scales::comma(round(total_emissions))), 
              hjust = -0.1, size = 3.2) +
    scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.1))) +
    labs(
        title = "Top Manufacturing & Printing Sectors by Emissions (2010)",
        x = "Sector",
        y = "Emissions (Tonnes)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(size = 9)
    )




# ------------------------------------------
# Step 1: Filter for manufacturing sectors
# ------------------------------------------
emis_manufacture <- emis_data %>%
    filter(str_detect(nace_rev_2_sector, regex("manufacturing|printing|manufacture", ignore_case = TRUE)))

# ------------------------------------------------------------
# 📊 Step 2: Summarize emissions per sector per year, top 5
# ------------------------------------------------------------
top_manu_each_year <- emis_manufacture %>%
    group_by(year, nace_rev_2_sector) %>%
    summarise(total_emissions = sum(value, na.rm = TRUE), .groups = "drop") %>%
    group_by(year) %>%
    slice_max(total_emissions, n = 5, with_ties = FALSE) %>%
    mutate(sector_wrapped = str_wrap(nace_rev_2_sector, width = 30))


#As there are too many stations that have missing data, we're choosing to collate the data all the weather conditions. 

#Pivoting wider and saving it to a dataframe, that could be collated later
prec_wide <- prec_data %>%
    dplyr::select(month, meteorological_weather_station, statistic_label, value) %>%
    tidyr::pivot_wider(names_from = statistic_label, values_from = value)

temp_wide <- temp_data %>%
    dplyr::select(month, meteorological_weather_station, statistic_label, value) %>%
    tidyr::pivot_wider(names_from = statistic_label, values_from = value)


#Merginging all the dataframes created above.
climate_data <- prec_wide %>%
    full_join(temp_wide, by = c("month", "meteorological_weather_station"))


# Renaming all columns in climate_data
climate_data <- climate_data %>%
    rename(
        month = month,
        station = meteorological_weather_station,
        precipitation_mm = `Precipitation Amount`,
        mean_max_temp_c = `Mean Maximum Temperature`,
        mean_min_temp_c = `Mean Minimum Temperature`,
        mean_air_temp_c = `Mean Air Temperature`,
        max_air_temp_c = `Maximum Air Temperature`,
        min_air_temp_c = `Minimum Air Temperature`,
        grass_min_temp_c = `Grass Minimum Temperature`
    )



#Separating month from year
climate_data <- climate_data %>%
    separate(month, into = c("year", "month"), sep = " ")


#Methods of imputation for climate data.
# 1. Filter data for years ≥ 1990, as we have chosen to work with the data from the year 1990 onwards
climate_sub <- climate_data %>%
    filter(as.numeric(year) >= 1990)

# Count missing values per column
na_counts <- sapply(climate_sub, function(col) sum(is.na(col)))

# Convert to a readable data frame
na_summary <- data.frame(
    Column = names(na_counts),
    Missing_Values = na_counts
)

# View result
print(na_summary)



# 2. Explore missing data patterns,provides a matrix view of missingness
md.pattern(climate_sub)

# 3. Run MICE with predictive mean matching (PMM)
set.seed(123)  # ensures reproducibility
imputed_mice <- mice(
    climate_sub,
    m = 5,                # number of imputed datasets
    method = 'pmm',       # predictive mean matching, robust for continuous data :contentReference[oaicite:5]{index=5}
    maxit = 10,           # number of iterations
    seed = 123            # reproducibility
)

# 4. Extract one completed dataset (e.g., first)
climate_imputed <- complete(imputed_mice, 1)


# 5. Final check.
# Total number of NA values in the entire imputed dataset
total_missing <- sum(is.na(climate_imputed))

# Print result
cat("Total number of missing values in climate_imputed:", total_missing, "\n")


# Pivot only the numeric climate variables to long format
climate_long <- climate_imputed %>%
    pivot_longer(
        cols = c(
            "precipitation_mm", "mean_max_temp_c", "mean_min_temp_c",
            "mean_air_temp_c", "max_air_temp_c", "min_air_temp_c", "grass_min_temp_c"
        ),
        names_to = "variable",
        values_to = "value"
    )

# Creating a boxplot for imputed data
ggplot(climate_long, aes(x = variable, y = value)) +
    geom_boxplot(fill = "#a1d99b", outlier.color = "red") +
    labs(
        title = "Boxplot of Climate Variables (Post MICE Imputation)",
        x = "Climate Variable",
        y = "Value"
    ) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        plot.title = element_text(face = "bold", hjust = 0.5)
    )

#Climate imputed is the cleaned dataset saved under the name - "climate_imputed" using MICE




# Visualizing the cleaned climate dataset post cleaning
# ----------------------------
# 0) Prep (do NOT overwrite climate_imputed)
# ----------------------------
cli_df <- climate_imputed %>%
    mutate(
        year  = suppressWarnings(as.integer(year)),
        month = factor(month, levels = month.name, ordered = TRUE)
    ) %>%
    filter(!is.na(year), !is.na(month))

# A neat minimal theme with borders
theme_bordered <- function() {
    theme_minimal(base_size = 12) +
        theme(
            panel.border     = element_rect(color = "grey40", fill = NA, linewidth = 0.6),
            panel.grid.minor = element_blank(),
            plot.title       = element_text(face = "bold"),
            plot.subtitle    = element_text(color = "grey25"),
            strip.background = element_rect(color = "grey40", fill = "grey95")
        )
}

# ----------------------------
# 1) Yearly variation (nationwide aggregates)
# ----------------------------
annual_agg <- cli_df %>%
    group_by(year) %>%
    summarise(
        mean_air_temp_c = mean(mean_air_temp_c, na.rm = TRUE),
        total_precip_mm = sum(precipitation_mm, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    pivot_longer(
        cols = c(mean_air_temp_c, total_precip_mm),
        names_to = "metric", values_to = "value"
    ) %>%
    mutate(metric = case_when(
        metric == "mean_air_temp_c" ~ "Mean Air Temperature (°C)",
        metric == "total_precip_mm" ~ "Total Precipitation (mm)",
        TRUE ~ metric
    ))

p_yearly <- ggplot(annual_agg, aes(x = year, y = value)) +
    geom_line(linewidth = 0.9) +
    facet_wrap(~ metric, scales = "free_y", ncol = 1) +
    scale_x_continuous(breaks = pretty_breaks()) +
    labs(
        title = "Ireland: Yearly Climate Variation",
        subtitle = "Nationwide mean air temperature and total precipitation (aggregated across stations)",
        x = "Year", y = NULL
    ) +
    theme_bordered()

print(p_yearly)

# ----------------------------
# 2) Decade-wise increments with % change labels
# ----------------------------
annual_for_decade <- cli_df %>%
    group_by(year) %>%
    summarise(
        mean_air_temp_c = mean(mean_air_temp_c, na.rm = TRUE),
        total_precip_mm = sum(precipitation_mm, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    mutate(decade = paste0(floor(year / 10) * 10, "s"))

decade_agg <- annual_for_decade %>%
    group_by(decade) %>%
    summarise(
        mean_air_temp_c = mean(mean_air_temp_c, na.rm = TRUE),
        total_precip_mm = mean(total_precip_mm, na.rm = TRUE),
        .groups = "drop"
    )

decade_long <- decade_agg %>%
    pivot_longer(
        cols = c(mean_air_temp_c, total_precip_mm),
        names_to = "metric", values_to = "value"
    ) %>%
    group_by(metric) %>%
    arrange(decade, .by_group = TRUE) %>%
    mutate(pct_change = 100 * (value / dplyr::lag(value) - 1)) %>%
    ungroup() %>%
    mutate(metric = case_when(
        metric == "mean_air_temp_c" ~ "Mean Air Temperature (°C)",
        metric == "total_precip_mm" ~ "Total Precipitation (mm)",
        TRUE ~ metric
    ))

p_decade <- ggplot(decade_long, aes(x = decade, y = value)) +
    geom_col(width = 0.7) +
    geom_text(
        aes(label = ifelse(is.na(pct_change), "",
                           paste0(ifelse(pct_change >= 0, "+", ""),
                                  round(pct_change, 1), "%"))),
        vjust = -0.4, size = 3
    ) +
    facet_wrap(~ metric, scales = "free_y") +
    labs(
        title = "Decade-wise Climate Change",
        subtitle = "Bars show decade averages; labels show % change vs previous decade",
        x = "Decade", y = NULL
    ) +
    theme_bordered()

print(p_decade)

# ----------------------------
# 3) Extremes: top-10 hottest & wettest years (lollipop)
# ----------------------------
annual_ext <- cli_df %>%
    group_by(year) %>%
    summarise(
        mean_air_temp_c = mean(mean_air_temp_c, na.rm = TRUE),
        total_precip_mm = sum(precipitation_mm, na.rm = TRUE),
        .groups = "drop"
    )

top_hot <- annual_ext %>% slice_max(mean_air_temp_c, n = 10)
p_hot <- ggplot(top_hot, aes(x = reorder(factor(year), mean_air_temp_c), y = mean_air_temp_c)) +
    geom_segment(aes(xend = factor(year), y = 0, yend = mean_air_temp_c), linewidth = 0.8) +
    geom_point(size = 2.2) +
    coord_flip() +
    labs(
        title = "Top 10 Hottest Years",
        subtitle = "Ranked by nationwide mean air temperature",
        x = "Year", y = "Mean Air Temperature (°C)"
    ) +
    theme_bordered()

print(p_hot)

top_wet <- annual_ext %>% slice_max(total_precip_mm, n = 10)
p_wet <- ggplot(top_wet, aes(x = reorder(factor(year), total_precip_mm), y = total_precip_mm)) +
    geom_segment(aes(xend = factor(year), y = 0, yend = total_precip_mm), linewidth = 0.8) +
    geom_point(size = 2.2) +
    coord_flip() +
    labs(
        title = "Top 10 Wettest Years",
        subtitle = "Ranked by nationwide total precipitation",
        x = "Year", y = "Total Precipitation (mm)"
    ) +
    theme_bordered()

print(p_wet)

# ----------------------------
# 4) Seasonal climatology (average by month)
# ----------------------------
monthly_climo <- cli_df %>%
    group_by(month) %>%
    summarise(
        mean_air_temp_c = mean(mean_air_temp_c, na.rm = TRUE),
        precip_mm       = mean(precipitation_mm, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    pivot_longer(
        cols = c(mean_air_temp_c, precip_mm),
        names_to = "metric", values_to = "value"
    ) %>%
    mutate(metric = case_when(
        metric == "mean_air_temp_c" ~ "Mean Air Temperature (°C)",
        metric == "precip_mm"       ~ "Mean Precipitation (mm)",
        TRUE ~ metric
    ))

p_season <- ggplot(monthly_climo, aes(x = month, y = value, group = 1)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2) +
    facet_wrap(~ metric, scales = "free_y", ncol = 1) +
    labs(
        title = "Seasonal Climatology",
        subtitle = "Long-term monthly means across all stations and years",
        x = NULL, y = NULL
    ) +
    theme_bordered() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

print(p_season)

# ----------------------------
# 5) Heatmap of monthly temperature anomalies (baseline 1991–2020)
# ----------------------------
base_window <- 1991:2020

monthly_baseline <- cli_df %>%
    filter(year %in% base_window) %>%
    group_by(month) %>%
    summarise(baseline_temp = mean(mean_air_temp_c, na.rm = TRUE), .groups = "drop")

monthly_anom <- cli_df %>%
    group_by(year, month) %>%
    summarise(mean_air_temp_c = mean(mean_air_temp_c, na.rm = TRUE), .groups = "drop") %>%
    left_join(monthly_baseline, by = "month") %>%
    mutate(temp_anomaly = mean_air_temp_c - baseline_temp)

p_heat <- ggplot(monthly_anom, aes(x = month, y = factor(year), fill = temp_anomaly)) +
    geom_tile(color = "grey85") +
    scale_fill_gradient2(
        low = "#2c7bb6", mid = "white", high = "#d7191c",
        midpoint = 0, name = "°C vs 1991–2020"
    ) +
    labs(
        title = "Monthly Temperature Anomalies",
        subtitle = "Baseline: 1991–2020 monthly average; positive = warmer than baseline",
        x = NULL, y = "Year"
    ) +
    theme_bordered() +
    theme(
        axis.text.x = element_text(angle = 0, vjust = 0.5),
        panel.grid = element_blank()
    )

print(p_heat)


# =========================================================================================

# Cleaning Rain Data
# Wrangling with rain_data 
# Step 1: Pivot to wide format based on the statistic_label
rain_wide <- rain_data %>%
    dplyr::select(year, month_number, meteorological_weather_station, statistic_label, value) %>%
    pivot_wider(names_from = statistic_label, values_from = value)


# Step 2: Add month name and reorder columns
rain_wide <- rain_wide %>%
    mutate(
        month_name = month(month_number, label = TRUE, abbr = FALSE)
    ) %>%
    dplyr::select(year, month_number, month_name, everything())

# Step 1: Filter data from 1990 onward
rain_1990 <- rain_data %>%
    filter(year >= 1990)

# Step 2: Add numeric month for sorting
rain_1990$month_num <- match(rain_1990$month, month.abb)

# Step 3: Pivot to wide format so each statistic becomes a column
rain_1990_wide <- rain_1990 %>%
    dplyr::select(year, month, month_num, meteorological_weather_station, statistic_label, value) %>%
    pivot_wider(names_from = statistic_label, values_from = value)

# Step 4: Interpolate missing values for each station
rain_interp_1990 <- rain_1990_wide %>%
    arrange(meteorological_weather_station, year, month_num) %>%
    group_by(meteorological_weather_station) %>%
    mutate(
        `Total Rainfall` = na.approx(`Total Rainfall`, na.rm = FALSE),
        `Most Rainfall in a Day` = na.approx(`Most Rainfall in a Day`, na.rm = FALSE),
        `Raindays (0.2mm or More)` = na.approx(`Raindays (0.2mm or More)`, na.rm = FALSE)
    ) %>%
    ungroup()

# Step 4: Fallback filling for remaining NAs
rain_filled_1990 <- rain_interp_1990 %>%
    group_by(meteorological_weather_station) %>%
    mutate(
        `Total Rainfall` = coalesce(
            na.locf(`Total Rainfall`, na.rm = FALSE),
            na.locf(`Total Rainfall`, fromLast = TRUE, na.rm = FALSE)
        ),
        `Most Rainfall in a Day` = coalesce(
            na.locf(`Most Rainfall in a Day`, na.rm = FALSE),
            na.locf(`Most Rainfall in a Day`, fromLast = TRUE, na.rm = FALSE)
        ),
        `Raindays (0.2mm or More)` = coalesce(
            na.locf(`Raindays (0.2mm or More)`, na.rm = FALSE),
            na.locf(`Raindays (0.2mm or More)`, fromLast = TRUE, na.rm = FALSE)
        )
    ) %>%
    ungroup()



# Step 5: Final NA check
rain_filled_1990 %>%
    summarise(
        na_rainfall = sum(is.na(`Total Rainfall`)),
        na_most_day = sum(is.na(`Most Rainfall in a Day`)),
        na_raindays = sum(is.na(`Raindays (0.2mm or More)`))
    )

# Count total NA values in the entire dataset
sum(is.na(rain_filled_1990))


# Pivoting our data for our boxplots

# Convert to long format for easier boxplotting
rain_long <- rain_filled_1990 %>%
    pivot_longer(
        cols = c(`Total Rainfall`, `Most Rainfall in a Day`, `Raindays (0.2mm or More)`),
        names_to = "variable",
        values_to = "value"
    )

# Plot
ggplot(rain_long, aes(x = variable, y = value)) +
    geom_boxplot(fill = "#9ecae1", outlier.color = "red") +
    labs(
        title = "Boxplot of Rainfall Measures (1990 onwards)",
        x = "Rainfall Variable",
        y = "Value"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 25, hjust = 1),
        panel.border = element_rect(color = "black", fill = NA)
    )

# ====================== Visualization for rain_data post cleaning=================

# We'll keep dataset name unchanged
rain_df <- rain_filled_1990

# Function for consistent bordered theme
theme_bordered <- function() {
    theme_minimal(base_size = 12) +
        theme(
            panel.border     = element_rect(color = "grey40", fill = NA, linewidth = 0.6),
            panel.grid.minor = element_blank(),
            plot.title       = element_text(face = "bold"),
            plot.subtitle    = element_text(color = "grey25"),
            strip.background = element_rect(color = "grey40", fill = "grey95")
        )
}

# 1) Yearly rainfall trends (total rain across all stations)
yearly_totals <- rain_df %>%
    group_by(year) %>%
    summarise(total_rain_mm = sum(`Total Rainfall`, na.rm = TRUE), .groups = "drop")

p_year <- ggplot(yearly_totals, aes(x = year, y = total_rain_mm)) +
    geom_line(linewidth = 0.9, color = "steelblue") +
    geom_point(size = 2, color = "steelblue") +
    labs(
        title = "Yearly Total Rainfall in Ireland",
        subtitle = "Aggregated across all stations",
        x = "Year", y = "Total Rainfall (mm)"
    ) +
    scale_x_continuous(breaks = pretty_breaks()) +
    theme_bordered()

print(p_year)

# 2) Monthly climatology (average rainfall by month)
monthly_climo <- rain_df %>%
    group_by(month) %>%
    summarise(mean_rain_mm = mean(`Total Rainfall`, na.rm = TRUE), .groups = "drop") %>%
    mutate(month = factor(month, levels = month.abb, ordered = TRUE))

p_month <- ggplot(monthly_climo, aes(x = month, y = mean_rain_mm, group = 1)) +
    geom_line(linewidth = 0.9, color = "darkgreen") +
    geom_point(size = 2, color = "darkgreen") +
    labs(
        title = "Average Monthly Rainfall (1990–2022)",
        subtitle = "Long-term station average by month",
        x = "Month", y = "Average Rainfall (mm)"
    ) +
    theme_bordered()

print(p_month)

# 3) Top 10 wettest years per station (lollipop chart)
top_wettest <- rain_df %>%
    group_by(year, meteorological_weather_station) %>%
    summarise(year_total = sum(`Total Rainfall`, na.rm = TRUE), .groups = "drop") %>%
    group_by(meteorological_weather_station) %>%
    slice_max(year_total, n = 1) %>%
    ungroup() %>%
    arrange(desc(year_total)) %>%
    slice_max(year_total, n = 10)

p_top10 <- ggplot(top_wettest, aes(x = reorder(paste(meteorological_weather_station, year, sep = " - "), year_total),
                                   y = year_total)) +
    geom_segment(aes(xend = paste(meteorological_weather_station, year, sep = " - "), y = 0, yend = year_total),
                 color = "skyblue", linewidth = 0.8) +
    geom_point(size = 2.5, color = "navy") +
    coord_flip() +
    labs(
        title = "Top 10 Wettest Station-Year Combinations",
        subtitle = "Based on total annual rainfall at each station",
        x = "Station - Year", y = "Total Rainfall (mm)"
    ) +
    theme_bordered()

print(p_top10)

# 4) Rain-day trends (≥ 0.2mm)
rain_days <- rain_df %>%
    group_by(year) %>%
    summarise(total_rain_days = sum(`Raindays (0.2mm or More)`, na.rm = TRUE), .groups = "drop")

p_raindays <- ggplot(rain_days, aes(x = year, y = total_rain_days)) +
    geom_line(linewidth = 0.9, color = "orange") +
    geom_point(size = 2, color = "orange") +
    labs(
        title = "Rain-day Trends in Ireland",
        subtitle = "Total number of days with rainfall ≥ 0.2mm across all stations",
        x = "Year", y = "Total Rain Days"
    ) +
    theme_bordered()

print(p_raindays)

# 5) Heatmap of station-year rainfall
station_heat <- rain_df %>%
    group_by(year, meteorological_weather_station) %>%
    summarise(year_total = sum(`Total Rainfall`, na.rm = TRUE), .groups = "drop")

p_heatmap <- ggplot(station_heat, aes(x = factor(year), y = meteorological_weather_station, fill = year_total)) +
    geom_tile(color = "white") +
    scale_fill_viridis_c(option = "C", name = "Total Rainfall (mm)") +
    labs(
        title = "Annual Rainfall Heatmap by Station",
        subtitle = "Highlights spatial and temporal rainfall extremes",
        x = "Year", y = "Station"
    ) +
    theme_bordered() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(p_heatmap)

# 6) Extreme daily rainfall events per month per station
extreme_daily <- rain_df %>%
    group_by(year, month, meteorological_weather_station) %>%
    summarise(max_daily_rain = max(`Most Rainfall in a Day`, na.rm = TRUE), .groups = "drop")

p_extreme <- ggplot(extreme_daily, aes(x = month, y = max_daily_rain, group = year, color = factor(year))) +
    geom_line(alpha = 0.5) +
    facet_wrap(~ meteorological_weather_station, scales = "free_y") +
    labs(
        title = "Extreme Daily Rainfall Events",
        subtitle = "Highest daily rainfall per month for each station",
        x = "Month", y = "Max Daily Rainfall (mm)", color = "Year"
    ) +
    theme_bordered() +
    theme(legend.position = "bottom")

print(p_extreme)



# Cleaning and wrangling the emissions pollution data
# Understanding the spread of our emis_data before cleaning and looking for NAs


# Load required packages
# Step 1: Filter only manufacturing-related sectors (including printing)
emis_filtered <- emis_data %>%
    filter(
        grepl("manufacture|manufacturing", nace_rev_2_sector, ignore.case = TRUE) |
            nace_rev_2_sector == "Printing and reproduction of recorded media (18)"
    )

# Step 1.1: Check for missing values by column
colSums(is.na(emis_filtered))  # Output should confirm zero NAs

# Step 2: Check skewness before transformation
emis_skewness <- emis_filtered %>%
    filter(value > 0) %>%
    group_by(statistic_label) %>%
    summarise(skewness = skewness(value), .groups = "drop")

print(emis_skewness)

# Step 3: Log-transform the emission values to reduce skewness
emis_filtered <- emis_filtered %>%
    mutate(log_value = ifelse(value > 0, log(value), NA))

# Step 4: Visualize spread before and after log transformation

# Before (Raw values)
plot_before <- ggplot(emis_filtered, aes(x = statistic_label, y = value)) +
    geom_boxplot(fill = "lightblue", outlier.color = "red") +
    scale_y_continuous(labels = comma) +
    labs(
        title = "Emissions Spread (Before Log Transformation)",
        x = "Pollutant",
        y = "Emissions (Tonnes)"
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.border = element_rect(color = "black", fill = NA))

# After (Log-transformed)
plot_after <- ggplot(emis_filtered, aes(x = statistic_label, y = log_value)) +
    geom_boxplot(fill = "steelblue", outlier.color = "black") +
    labs(
        title = "Emissions Spread (After Log Transformation)",
        x = "Pollutant",
        y = "Log(Emissions)"
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.border = element_rect(color = "black", fill = NA))

# Show both plots
print(plot_before)
print(plot_after)

# Step 5: Save the final dataset with log-transformed emissions
filtered_emis_data <- emis_filtered

# Optional: Final summary check
summary(filtered_emis_data$value)
summary(filtered_emis_data$log_value)

head(filtered_emis_data)
colnames(filtered_emis_data)
lapply(filtered_emis_data[,c("statistic_label","year","nace_rev_2_sector","unit")],unique)


# =========== Visualizing the log scaled emissions dataset ======================
# ------------------------------------------------------------
# Data setup
# ------------------------------------------------------------
emis_df <- filtered_emis_data

# Custom theme with border for all plots
theme_bordered <- function() {
    theme_minimal(base_size = 12) +
        theme(
            panel.border = element_rect(color = "grey40", fill = NA, linewidth = 0.6),
            panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold"),
            plot.subtitle = element_text(color = "grey25"),
            strip.background = element_rect(color = "grey40", fill = "grey95")
        )
}

# ------------------------------------------------------------
# 1. Yearly total emissions trend for each pollutant
# ------------------------------------------------------------
p_trends <- emis_df %>%
    group_by(year, statistic_label) %>%
    summarise(total_val = sum(value, na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(x = year, y = total_val, color = statistic_label)) +
    geom_line(linewidth = 1) +
    geom_point() +
    scale_y_continuous(labels = comma) +
    labs(
        title = "Yearly Emission Trends by Pollutant",
        subtitle = "Total emissions aggregated across all manufacturing sectors",
        x = "Year", y = "Emissions (Tonnes)", color = "Pollutant"
    ) +
    theme_bordered()

print(p_trends)

# ------------------------------------------------------------
# 2. Top sectors contributing to each pollutant (average over all years)
# ------------------------------------------------------------
top_sectors <- emis_df %>%
    group_by(statistic_label, nace_rev_2_sector) %>%
    summarise(mean_emis = mean(value, na.rm = TRUE), .groups = "drop") %>%
    group_by(statistic_label) %>%
    slice_max(mean_emis, n = 5) %>%
    ungroup()

p_topsec <- ggplot(top_sectors, aes(x = reorder(nace_rev_2_sector, mean_emis), y = mean_emis, fill = statistic_label)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    facet_wrap(~statistic_label, scales = "free_x") +
    labs(
        title = "Top 5 Sectors for Each Pollutant",
        subtitle = "Average annual emissions across all years",
        x = "Sector", y = "Average Emissions (Tonnes)"
    ) +
    theme_bordered()

print(p_topsec)

# ------------------------------------------------------------
# 3. Share of each pollutant in total emissions per year
# ------------------------------------------------------------
pollutant_share <- emis_df %>%
    group_by(year, statistic_label) %>%
    summarise(total_val = sum(value, na.rm = TRUE), .groups = "drop") %>%
    group_by(year) %>%
    mutate(share = total_val / sum(total_val))

# Ensure year is integer
pollutant_share <- pollutant_share %>%
    mutate(year = as.integer(year))

p_share <- ggplot(pollutant_share, aes(x = year, y = share, fill = statistic_label)) +
    geom_area(alpha = 0.85) +
    scale_y_continuous(labels = percent_format()) +
    scale_x_continuous(breaks = unique(pollutant_share$year), 
                       labels = as.character(unique(pollutant_share$year))) +
    labs(
        title = "Proportional Share of Each Pollutant by Year",
        subtitle = "Shows how dominant each pollutant is in total emissions",
        x = "Year", y = "Share of Total Emissions", fill = "Pollutant"
    ) +
    theme_bordered() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_share)


# ------------------------------------------------------------
# 4. Top 10 pollutant–sector combinations (total emissions across all years)
# ------------------------------------------------------------
top_combos <- emis_df %>%
    group_by(statistic_label, nace_rev_2_sector) %>%
    summarise(total_val = sum(value, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_val)) %>%
    slice_max(total_val, n = 10)

p_top10 <- ggplot(top_combos, aes(x = reorder(paste(statistic_label, nace_rev_2_sector, sep = " - "), total_val), y = total_val)) +
    geom_segment(aes(xend = paste(statistic_label, nace_rev_2_sector, sep = " - "), y = 0, yend = total_val),
                 color = "skyblue", linewidth = 0.8) +
    geom_point(size = 2.5, color = "navy") +
    coord_flip() +
    labs(
        title = "Top 10 Pollutant–Sector Combinations",
        subtitle = "Highest emitters across the entire dataset",
        x = "Pollutant - Sector", y = "Total Emissions (Tonnes)"
    ) +
    theme_bordered()

print(p_top10)

# ------------------------------------------------------------
# 5. Heatmap of log emissions by sector and year
# ------------------------------------------------------------
heatmap_df <- emis_df %>%
    mutate(sector_short = strtrim(nace_rev_2_sector, 40))  # shorten names for readability

p_heatmap <- ggplot(heatmap_df, aes(x = factor(year), y = fct_reorder(sector_short, log_value, .fun = median, .desc = TRUE),
                                    fill = log_value)) +
    geom_tile(color = "white") +
    scale_fill_viridis_c(option = "C", name = "Log Emissions") +
    labs(
        title = "Heatmap of Log Emissions by Sector and Year",
        subtitle = "Darker colors indicate higher emissions",
        x = "Year", y = "Sector"
    ) +
    theme_bordered() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(p_heatmap)

# ------------------------------------------------------------
# Helper: year range (robust to our data)
yr_min <- min(emis_df$year, na.rm = TRUE)
yr_max <- max(emis_df$year, na.rm = TRUE)

# ================================
# 1) Pareto (80/20) per pollutant in the latest year
# ================================
latest_year <- yr_max

pareto_df <- emis_df %>%
  filter(year == latest_year) %>%
  group_by(statistic_label, nace_rev_2_sector) %>%
  summarise(val = sum(value, na.rm = TRUE), .groups = "drop") %>%
  group_by(statistic_label) %>%
  arrange(desc(val), .by_group = TRUE) %>%
  mutate(
    cum_val   = cumsum(val),
    total_val = sum(val, na.rm = TRUE),
    cum_share = cum_val / total_val
  ) %>%
  ungroup()

p_pareto <- ggplot(pareto_df,
                   aes(x = fct_reorder(nace_rev_2_sector, val, .desc = TRUE), y = val)) +
  geom_col(fill = "steelblue") +
  geom_line(aes(y = cum_share * max(val)), group = 1, color = "tomato", linewidth = 1) +
  geom_hline(yintercept = 0.8 * max(pareto_df$val), linetype = "dashed", color = "grey50") +
  coord_flip() +
  facet_wrap(~ statistic_label, scales = "free_x") +
  scale_y_continuous(labels = comma) +
  labs(
    title = paste0("Pareto of Sector Emissions in ", latest_year),
    subtitle = "Bars = sector totals; red line = cumulative share (scaled to bar axis). Dashed line ≈ 80% threshold.",
    x = "NACE Rev.2 Sector", y = "Emissions (Tonnes)"
  ) +
  theme_bordered()
print(p_pareto)

# ================================
# 2) CAGR heatmap 2010–2022 (or your full span)
# ================================
# CAGR = (last/first)^(1/n_years)-1, per (pollutant, sector)
cagr_df <- emis_df %>%
  group_by(statistic_label, nace_rev_2_sector) %>%
  summarise(
    first_year  = first(year[order(year)]),
    last_year   = last(year[order(year)]),
    first_val   = value[which.min(year)],
    last_val    = value[which.max(year)],
    span_years  = last_year - first_year,
    .groups = "drop"
  ) %>%
  mutate(
    CAGR = ifelse(first_val > 0 & span_years > 0,
                  (last_val / first_val)^(1 / span_years) - 1,
                  NA_real_)
  )

p_cagr <- ggplot(
  cagr_df %>% filter(!is.na(CAGR)),
  aes(x = statistic_label,
      y = fct_reorder(nace_rev_2_sector, CAGR, .fun = median, .desc = TRUE),
      fill = CAGR)
) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#2c7bb6", mid = "white", high = "#d7191c", midpoint = 0,
                       labels = percent_format(accuracy = 1), name = "CAGR") +
  labs(
    title = "Sector Growth/Decline in Emissions (CAGR)",
    subtitle = paste0("From first to last year available (", yr_min, "–", yr_max, ")"),
    x = "Pollutant", y = "NACE Rev.2 Sector"
  ) +
  theme_bordered() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
print(p_cagr)

# ================================
# 3) Volatility vs Level (mean vs coefficient of variation)
# ================================
vol_df <- emis_df %>%
  group_by(statistic_label, nace_rev_2_sector) %>%
  summarise(
    mean_val = mean(value, na.rm = TRUE),
    sd_val   = sd(value, na.rm = TRUE),
    cv       = ifelse(mean_val > 0, sd_val / mean_val, NA_real_),
    .groups  = "drop"
  )

# Label only the top 10 biggest mean emitters per pollutant to keep plot readable
vol_labs <- vol_df %>%
  group_by(statistic_label) %>%
  slice_max(mean_val, n = 10) %>%
  ungroup() %>%
  mutate(label = nace_rev_2_sector)

p_vol <- ggplot(vol_df, aes(x = mean_val, y = cv)) +
  geom_point(alpha = 0.5, color = "grey30") +
  geom_point(data = vol_labs, aes(x = mean_val, y = cv), color = "tomato", size = 2) +
  geom_text(data = vol_labs, aes(label = label), hjust = 0, vjust = 0.5, size = 3,
            nudge_x = 0.02 * max(vol_df$mean_val, na.rm = TRUE)) +
  facet_wrap(~ statistic_label, scales = "free") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Volatility vs Average Emission Level",
    subtitle = "CV (sd/mean) vs mean emissions by sector and pollutant; labels show top 10 mean emitters",
    x = "Average Emissions (Tonnes)", y = "Coefficient of Variation"
  ) +
  theme_bordered()
print(p_vol)

# ================================
# 4) Anomaly timeline: z-scores by pollutant (baseline 2010–2019)
# ================================

baseline_years <- 2010:2019

# Totals per year & pollutant
pollutant_totals <- emis_df %>%
    group_by(year, statistic_label) %>%
    summarise(total_val = sum(value, na.rm = TRUE), .groups = "drop")

# Baseline mean/sd per pollutant over 2010–2019
baseline_stats <- pollutant_totals %>%
    filter(year %in% baseline_years) %>%
    group_by(statistic_label) %>%
    summarise(
        mu    = mean(total_val, na.rm = TRUE),
        sigma = sd(total_val,   na.rm = TRUE),
        .groups = "drop"
    )

# To see which pollutants lack baseline coverage
missing_baseline <- setdiff(unique(pollutant_totals$statistic_label),
                            unique(baseline_stats$statistic_label))
if (length(missing_baseline) > 0) {
    message("No 2010–2019 baseline for: ", paste(missing_baseline, collapse = ", "))
}

# Join baseline; compute z = (x - mu)/sigma with safe denominator
zs <- pollutant_totals %>%
    left_join(baseline_stats, by = "statistic_label") %>%
    mutate(
        # if sigma is NA or 0 (flat baseline), set z = 0
        z = ifelse(is.na(sigma) | sigma == 0, 0, (total_val - mu) / sigma)
    )

# Plot anomalies
p_z <- ggplot(zs, aes(x = year, y = z, fill = z > 0)) +
    geom_col() +
    facet_wrap(~ statistic_label, scales = "free_y") +
    scale_fill_manual(values = c("TRUE" = "tomato", "FALSE" = "steelblue"), guide = "none") +
    geom_hline(yintercept = 0, color = "grey40") +
    labs(
        title = "Emissions Anomalies by Pollutant (Z-scores)",
        subtitle = "Baseline = 2010–2019 mean and SD; bars above/below baseline",
        x = "Year", y = "Z-score"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        panel.border = element_rect(color = "grey40", fill = NA, linewidth = 0.6),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(color = "grey40", fill = "grey95")
    )

print(p_z)



# ================================
# 5) Share shift slopegraph (2010 vs 2022) for a chosen pollutant
# ================================
chosen_pollutant <- "Carbon monoxide (CO)"
start_year <- 2010
end_year   <- 2022

shares <- emis_df %>%
  filter(statistic_label == chosen_pollutant, year %in% c(start_year, end_year)) %>%
  group_by(year) %>%
  mutate(year_total = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(year, nace_rev_2_sector) %>%
  summarise(share = sum(value, na.rm = TRUE) / first(year_total), .groups = "drop")

# keep top sectors by average share across the two years to avoid spaghetti
keep_secs <- shares %>%
  group_by(nace_rev_2_sector) %>%
  summarise(avg_share = mean(share, na.rm = TRUE), .groups = "drop") %>%
  slice_max(avg_share, n = 12) %>%
  pull(nace_rev_2_sector)

slope_df <- shares %>% filter(nace_rev_2_sector %in% keep_secs)

p_slope <- ggplot(slope_df,
                  aes(x = factor(year), y = share, group = nace_rev_2_sector, color = nace_rev_2_sector)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = paste0("Sector Share Shift: ", chosen_pollutant, " (", start_year, " → ", end_year, ")"),
    subtitle = "Top sectors by average share; lines show change in share of total pollutant emissions",
    x = "Year", y = "Share of Pollutant Emissions", color = "Sector"
  ) +
  theme_bordered() +
  theme(legend.position = "bottom")
print(p_slope)


# ------------------------------------------------------------
# 7. Compare trends of pollutants with similar sources (Example: NOx vs SOx)
# ------------------------------------------------------------
similar_pollutants <- c(
    "Nitrogen oxides in NO2 equivalent (NOX)",
    "Sulphur oxides in SO2 equivalent (SOX_SO2E)"
)

p_compare_similar <- emis_df %>%
    filter(statistic_label %in% similar_pollutants) %>%
    group_by(year, statistic_label) %>%
    summarise(total_val = sum(value, na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(x = year, y = total_val, color = statistic_label)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_y_continuous(labels = comma) +
    labs(
        title = "Comparison of NOx vs SOx Emissions",
        subtitle = "Yearly totals for pollutants with similar industrial sources",
        x = "Year", y = "Emissions (Tonnes)", color = "Pollutant"
    ) +
    theme_bordered()

print(p_compare_similar)

# 8. Detect pollutants where top sector changes frequently over time
# ------------------------------------------------------------
sector_changes <- emis_df %>%
    group_by(year, statistic_label) %>%
    slice_max(value, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    group_by(statistic_label) %>%
    summarise(
        n_changes = n_distinct(nace_rev_2_sector),
        .groups = "drop"
    ) %>%
    arrange(desc(n_changes))

p_sector_changes <- ggplot(sector_changes, aes(x = reorder(statistic_label, n_changes), y = n_changes, fill = n_changes)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    scale_fill_viridis_c() +
    labs(
        title = "Pollutants with Frequent Top-Sector Changes",
        subtitle = "Number of different sectors that ranked #1 in emissions over the years",
        x = "Pollutant", y = "Count of Unique #1 Sectors"
    ) +
    theme_bordered()
print(p_sector_changes)

# ================================================================================================================================================================================


#Cleaning the hospital dataset
view(hosp_data)

#Checking if the hosp_data has empty values, that are to be imputed. 
colSums(is.na(hosp_data))

#Filtering out rows with empty values. 
hosp_data_clean <- hosp_data %>%
    filter(!is.na(value))

# Splitting the dataset based on 'statistic_label'
totaldisch_data <- hosp_data %>%
    filter(statistic_label == "Total Hospital Discharges")

disch_1000 <- hosp_data %>%
    filter(statistic_label == "Hospital Discharges Per 1,000 Population")


#Checking if the total hospital discharges has empty values, that are to be imputed. 
colSums(is.na(totaldisch_data)) # checking the total empty values before imputation


#Checking if the discharges per 1000 population has empty values, that are to be imputed. 
colSums(is.na(disch_1000)) # checking the total empty values before imputation

#Visualizing the totaldisch_data to check for skewness
# Step 1: Filter and summarize
yearly_discharges <- totaldisch_data %>%
    filter(sex %in% c("Male", "Female")) %>%
    group_by(year, sex) %>%
    summarise(total_discharges = sum(value, na.rm = TRUE), .groups = "drop")


# Step 2: Plot area chart
ggplot(yearly_discharges, aes(x = year, y = total_discharges, fill = sex)) +
    geom_area(alpha = 0.7, position = "stack") +
    scale_y_continuous(labels = scales::comma) +
    scale_fill_manual(values = c("steelblue", "tomato")) +
    labs(
        title = "Yearly Total Hospital Discharges by Sex",
        x = "Year",
        y = "Total Discharges",
        fill = "Sex"
    ) +
    theme_minimal(base_size = 13)





#Visualizing the totaldisch_data to check for skewness
# Step 1: Filter out "Both sexes" and summarise
yearly_disch_1000 <- disch_1000 %>%
    filter(sex %in% c("Male", "Female")) %>%
    group_by(year, sex) %>%
    summarise(total_rate = sum(value, na.rm = TRUE), .groups = "drop")

# Step 2: Create area chart
ggplot(yearly_disch_1000, aes(x = year, y = total_rate, fill = sex)) +
    geom_area(alpha = 0.7, position = "stack") +
    scale_y_continuous(labels = scales::comma) +
    scale_fill_manual(values = c("skyblue", "salmon")) +
    labs(
        title = "Hospital Discharges per 1,000 Population by Sex (Yearly)",
        x = "Year",
        y = "Discharges per 1,000 Population",
        fill = "Sex"
    ) +
    theme_minimal(base_size = 13)



# Histogram to visually inspect skewness
ggplot(totaldisch_data, aes(x = value)) +
    geom_histogram(fill = "steelblue", bins = 50, alpha = 0.7) +
    labs(title = "Distribution of Total Hospital Discharges", x = "Discharges", y = "Count")
# There is some heavy skewness



# Compressing the skewness, making it easier for imputation.
ggplot(totaldisch_data, aes(x = value)) +
    geom_histogram(fill = "steelblue", bins = 50, alpha = 0.7) +
    scale_x_log10() +
    labs(title = "Log-Scaled Distribution of Total Hospital Discharges",
         x = "Discharges (log scale)", y = "Count")

# Note: Log-transforming value before imputation might help normalize the scale and improve accuracy for methods like MICE

# Imputation of the Total Hospital Discharges begins from here
# Make a copy and log-transform
totaldisch_mice <- totaldisch_data %>%
    mutate(value_log = ifelse(value > 0, log(value), NA))  # avoiding log(0)

# Select variables for imputation (excluding original 'value')
mice_input <- totaldisch_mice %>%
    dplyr::select(year, sex, age_group, area, high_level_ishmt, unit, value_log)

# Running MICE
mice_model <- mice(mice_input, m = 5, method = "pmm", seed = 123)

# Checking if imputations happened
summary(mice_model)

# Visual diagnostic (optional)
# plot(mice_model)   # Uncomment to see convergence plots

# Check how many missing values were imputed
mice_model$imp$value_log


imputed_data <- complete(mice_model, 1)  # use first imputed dataset

# Back-transform to original scale
imputed_data <- imputed_data %>%
    mutate(value_imputed = exp(value_log))

# Check if all missing values are gone
sum(is.na(imputed_data$value_imputed)) # should be 0

# Compare before & after: how many NA values were fixed
original_na <- sum(is.na(totaldisch_data$value))
cat("Original missing:", original_na, "\n")

imputed_na <- sum(is.na(imputed_data$value_imputed))
cat("After imputation:", imputed_na, "\n")

# Rename imputed data for Total Discharges
totaldisch_impdata <- imputed_data



# ======= We're here visualizing the discharge for every 1000 at the unit rate =======
# Checking skewness for discharges for every 1000. 
ggplot(disch_1000, aes(x = value)) +
    geom_histogram(fill = "tomato", bins = 50, alpha = 0.7) +
    labs(title = "Distribution of Discharges per 1,000 Population",
         x = "Discharges per 1,000", y = "Count")

# Reducing skewness using log transformation
ggplot(disch_1000, aes(x = value)) +
    geom_histogram(fill = "steelblue", bins = 50, alpha = 0.7) +
    scale_x_log10() +
    labs(title = "Log-Scaled Distribution: Discharges per 1,000 Population",
         x = "Discharges per 1,000 (log scale)", y = "Count")


#Step 1: Apply Log Transformation (safe fix for right skew)
disch_1000_mice <- disch_1000 %>%
    mutate(value_log = ifelse(value > 0, log(value), NA))

#Step 2: Running MICE on log-transformed data
mice_input_1000 <- disch_1000_mice %>%
    dplyr::select(year, sex, age_group, area, high_level_ishmt, unit, value_log)

mice_model_1000 <- mice(mice_input_1000, m = 5, method = "pmm", seed = 456)

# Step 3: Extracting and back-transform. Complete the first imputed dataset
imputed_data_1000 <- complete(mice_model_1000, 1)

# Step 4: Back-transform to original scale
disch_1000_impdata <- disch_1000_mice %>%
    dplyr::select(-value_log) %>%
    dplyr::mutate(value_imputed = exp(imputed_data_1000$value_log))


# Step 5: Validation check (no NAs)
sum(is.na(disch_1000_impdata$value_imputed)) # Should be 0 if the imputation has taken a place. 



# Step 6: Comparing before/after visually
ggplot(disch_1000_impdata, aes(x = value_imputed)) +
    geom_histogram(fill = "darkgreen", bins = 50, alpha = 0.7) +
    labs(title = "Distribution After Imputation (Log Back-Transformed)",
         x = "Discharges per 1,000", y = "Count")


# ============================================================
# Respiratory Health Insights from totaldisch_impdata (2010–2024)
# Focus: "Diseases of the respiratory system (J00-J99)"
# Creates multiple ggplots that support your research framing
# ============================================================
# -----------------------------
# 0) Setup and safe coercions
# -----------------------------
df_raw <- totaldisch_impdata

# Make sure expected columns exist
required_cols <- c("year","sex","age_group","area","high_level_ishmt","unit","value_imputed")
missing <- setdiff(required_cols, names(df_raw))
if (length(missing) > 0) stop("Missing columns in totaldisch_impdata: ", paste(missing, collapse = ", "))

# Safe copies / coercions
df <- df_raw %>%
    mutate(
        year = suppressWarnings(as.integer(year)),
        sex  = factor(sex, levels = c("Both sexes","Male","Female")),
        # Order age groups for readability
        age_group = factor(
            age_group,
            levels = c("0 - 14 years","15 - 24 years","25 - 34 years","35 - 44 years",
                       "45 - 54 years","55 - 64 years","65 - 74 years","75 - 84 years",
                       "85 years and over","All ages"),
            ordered = TRUE
        )
    )

# Filter to Respiratory (J00-J99)
resp_str <- "Diseases of the respiratory system (J00-J99)"
resp <- df %>% filter(high_level_ishmt == resp_str)

# Useful helpers
theme_bordered <- function() {
    theme_minimal(base_size = 12) +
        theme(
            panel.border     = element_rect(color = "grey40", fill = NA, linewidth = 0.6),
            panel.grid.minor = element_blank(),
            plot.title       = element_text(face = "bold"),
            plot.subtitle    = element_text(color = "grey25"),
            strip.background = element_rect(color = "grey40", fill = "grey95")
        )
}
yr_min <- min(resp$year, na.rm = TRUE)
yr_max <- max(resp$year, na.rm = TRUE)

# You can exclude national aggregates if you want county-only views:
area_exclude <- c("Ireland","Non-residents","Ireland plus non-residents")

# ============================================================
# 1) National time series by Sex (sums across age/areas)
# ============================================================
nat_sex_year <- resp %>%
    group_by(year, sex) %>%
    summarise(total_cases = sum(value_imputed, na.rm = TRUE), .groups = "drop")

p1_trend_sex <- ggplot(nat_sex_year, aes(x = year, y = total_cases, color = sex, group = sex)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_y_continuous(labels = comma) +
    labs(
        title = "Respiratory Discharges in Ireland (J00–J99)",
        subtitle = "National totals by sex (2010–2024, imputed where applicable)",
        x = "Year", y = "Hospital Discharges", color = "Sex"
    ) +
    theme_bordered()
print(p1_trend_sex)

# ============================================================
# 2) Age-group vulnerability (cumulative totals) + stacked by sex
# ============================================================
age_totals <- resp %>%
    group_by(age_group, sex) %>%
    summarise(total_cases = sum(value_imputed, na.rm = TRUE), .groups = "drop")

p2_age_vulnerability <- ggplot(age_totals, aes(x = age_group, y = total_cases, fill = sex)) +
    geom_col(position = "stack") +
    coord_flip() +
    scale_y_continuous(labels = comma) +
    labs(
        title = "Age-Group Vulnerability in Respiratory Discharges",
        subtitle = "Cumulative totals across all years, stacked by sex",
        x = "Age Group", y = "Total Discharges", fill = "Sex"
    ) +
    theme_bordered()
print(p2_age_vulnerability)

# ============================================================
# 3a) County hotspots (TOTALS 2010–2024) – bar (exclude aggregates)
# ============================================================
area_totals <- resp %>%
    filter(!area %in% area_exclude) %>%
    group_by(area) %>%
    summarise(total_cases = sum(value_imputed, na.rm = TRUE), .groups = "drop")

p3a_area_bar <- ggplot(area_totals, aes(x = reorder(area, total_cases), y = total_cases, fill = total_cases)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    scale_y_continuous(labels = comma) +
    scale_fill_viridis_c() +
    labs(
        title = "Geographic Hotspots: Respiratory Discharges by County",
        x = "County", y = "Total Discharges"
    ) +
    theme_bordered()
print(p3a_area_bar)

# ============================================================
# 3b) County-year heatmap (temporal hotspots)
# ============================================================
area_year <- resp %>%
    filter(!area %in% area_exclude) %>%
    group_by(area, year) %>%
    summarise(total_cases = sum(value_imputed, na.rm = TRUE), .groups = "drop")

p3b_area_heat <- ggplot(area_year, aes(x = factor(year), y = fct_reorder(area, total_cases, .fun = median, .desc = TRUE),
                                       fill = total_cases)) +
    geom_tile(color = "white") +
    scale_fill_viridis_c(labels = comma, name = "Discharges") +
    labs(
        title = "County-Year Heatmap of Respiratory Discharges",
        subtitle = "Highlights spatial and temporal clustering (2010–2024)",
        x = "Year", y = "County"
    ) +
    theme_bordered() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
print(p3b_area_heat)

# ============================================================
# 4) Extreme years: anomaly (z-score) detection (baseline 2010–2019)
#    Adjust baseline years easily if your framing changes.
# ============================================================
baseline_years <- max(2010, yr_min):min(2019, yr_max)

resp_year_nat <- resp %>%
    group_by(year) %>%
    summarise(total_cases = sum(value_imputed, na.rm = TRUE), .groups = "drop")

baseline_stats <- resp_year_nat %>%
    filter(year %in% baseline_years) %>%
    summarise(
        mu    = mean(total_cases, na.rm = TRUE),
        sigma = sd(total_cases,   na.rm = TRUE)
    )

resp_year_nat <- resp_year_nat %>%
    mutate(
        z = ifelse(is.na(baseline_stats$sigma) || baseline_stats$sigma == 0,
                   0, (total_cases - baseline_stats$mu) / baseline_stats$sigma),
        anomaly = ifelse(abs(z) >= 2, "Anomaly (|z|≥2)", "Normal")
    )

p4_anomaly <- ggplot(resp_year_nat, aes(x = year, y = total_cases, color = anomaly)) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    scale_color_manual(values = c("Anomaly (|z|≥2)" = "red", "Normal" = "black")) +
    scale_y_continuous(labels = comma) +
    labs(
        title = "Anomalies in Respiratory Discharges (National)",
        subtitle = paste0("Baseline = ", min(baseline_years), "–", max(baseline_years),
                          " mean±sd; anomalies flagged by |z|≥2"),
        x = "Year", y = "Total Discharges", color = NULL
    ) +
    theme_bordered()
print(p4_anomaly)

# ============================================================
# 5) Male–Female gap over time (share & difference)
# ============================================================
mf_year <- resp %>%
    filter(sex %in% c("Male","Female")) %>%
    group_by(year, sex) %>%
    summarise(val = sum(value_imputed, na.rm = TRUE), .groups = "drop") %>%
    group_by(year) %>%
    mutate(total = sum(val), share = val / total) %>%
    ungroup()

mf_wide <- mf_year %>%
    dplyr::select(year, sex, share) %>%
    pivot_wider(names_from = sex, values_from = share) %>%
    mutate(gap = Female - Male)

p5_share <- ggplot(mf_year, aes(x = year, y = share, color = sex)) +
    geom_line(linewidth = 1) +
    scale_y_continuous(labels = percent_format()) +
    labs(
        title = "Male vs Female Share of Respiratory Discharges",
        subtitle = "National shares over time (excludes 'Both sexes' rows)",
        x = "Year", y = "Share of Total", color = "Sex"
    ) +
    theme_bordered()
print(p5_share)

p5_gap <- ggplot(mf_wide, aes(x = year, y = gap)) +
    geom_hline(yintercept = 0, color = "grey50") +
    geom_line(linewidth = 1, color = "tomato") +
    scale_y_continuous(labels = percent_format()) +
    labs(
        title = "Female–Male Share Gap Over Time",
        subtitle = "Positive = Female share higher; Negative = Male share higher",
        x = "Year", y = "Female minus Male (pp)"
    ) +
    theme_bordered()
print(p5_gap)

# ============================================================
# 6) Diverging Age Pyramid (2024) by sex (national)
#    Great for a recent snapshot that you can relate to predictions for 2026.
# ============================================================
yr_snap <- ifelse(any(resp$year == 2024, na.rm = TRUE), 2024, max(resp$year, na.rm = TRUE))

pyr <- resp %>%
    filter(year == yr_snap, sex %in% c("Male","Female")) %>%
    group_by(age_group, sex) %>%
    summarise(total_cases = sum(value_imputed, na.rm = TRUE), .groups = "drop") %>%
    mutate(value_signed = ifelse(sex == "Male", -total_cases, total_cases))

p6_pyramid <- ggplot(pyr, aes(x = age_group, y = value_signed, fill = sex)) +
    geom_col(width = 0.8) +
    coord_flip() +
    scale_y_continuous(labels = function(x) comma(abs(x))) +
    labs(
        title = paste0("Respiratory Discharge Age Pyramid (", yr_snap, ")"),
        subtitle = "Male values shown to the left (negative) and Female to the right (positive)",
        x = "Age Group", y = "Hospital Discharges (absolute scale)"
    ) +
    theme_bordered()
print(p6_pyramid)

# ============================================================
# Notes for thesis narrative:
# - p1: use as your core national trend by sex; connect spikes/dips to emissions/climate years later.
# - p2: identify the most burdened age bands; these often amplify climate/emission effects.
# - p3a/p3b: map industrial counties vs hotspots; later overlay with manufacturing emissions by county (if available).
# - p4: anomaly years give defensible windows for causal exploration with emissions/climate anomalies.
# - p5: gender dynamics—useful for fairness/targeted interventions.
# - p6: recent snapshot; sets up your 2026 forecasting target profile by age.
# ============================================================
              
# ========================================================================================

head(disch_1000_impdata)
colnames(disch_1000_impdata)
lapply(disch_1000_impdata[,c("statistic_label" , "year"      ,       "sex"         ,     "age_group"     ,   "area"        ,     "high_level_ishmt" ,"unit" )], unique)


# ===============================================================
# Respiratory insights from disch_1000_impdata (Rate)
# To extract unique patterns that inform 2026 prediction
# ===============================================================



# ---------- Helper: neat theme with borders ----------
theme_bordered <- function() {
    theme_minimal(base_size = 12) +
        theme(
            panel.border = element_rect(color = "grey40", fill = NA, linewidth = 0.7),
            plot.title = element_text(face = "bold"),
            plot.subtitle = element_text(color = "grey30"),
            strip.background = element_rect(fill = "grey95", color = "grey80")
        )
}

# ---------- 0) Filter respiratory; sanity constraints ----------
resp <- disch_1000_impdata %>%
    filter(
        unit == "Rate",
        high_level_ishmt == "Diseases of the respiratory system (J00-J99)"
    ) %>%
    mutate(
        year = as.integer(year),
        sex = factor(sex, levels = c("Both sexes","Male","Female")),
        age_group = factor(
            age_group,
            levels = c("0 - 14 years","15 - 24 years","25 - 34 years","35 - 44 years",
                       "45 - 54 years","55 - 64 years","65 - 74 years","75 - 84 years",
                       "85 years and over","All ages"),
            ordered = TRUE
        )
    )

# ---------- 1) NATIONAL TREND (All ages, Both sexes, Ireland) ----------
nat_trend <- resp %>%
    filter(age_group == "All ages", sex == "Both sexes", area == "Ireland") %>%
    group_by(year) %>%
    summarise(rate = mean(value, na.rm = TRUE), .groups = "drop")

p_nat <- ggplot(nat_trend, aes(year, rate)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_vline(xintercept = 2016.5, linetype = "dashed", color = "grey40") +
    geom_vline(xintercept = 2023.5, linetype = "dashed", color = "grey40") +
    scale_y_continuous(labels = label_number(accuracy = 0.1)) +
    labs(
        title = "Respiratory Discharges per 1,000 – National Trend",
        subtitle = "All ages · Both sexes · Ireland. Dashed lines mark transition (end of imputation window) and 2024.",
        x = "Year", y = "Rate per 1,000"
    ) +
    theme_bordered()
print(p_nat)

# ---------- 2) COUNTY HEATMAP: change vs 2010 and vs 2017 ----------
county_allages <- resp %>%
    filter(age_group == "All ages", sex == "Both sexes",
           !area %in% c("Non-residents","Ireland plus non-residents")) %>%
    group_by(area, year) %>%
    summarise(rate = mean(value, na.rm = TRUE), .groups = "drop")

# Anchor baselines
baseline_2010 <- county_allages %>% filter(year == 2010) %>% dplyr::select(area, rate_2010 = rate)
baseline_2017 <- county_allages %>% filter(year == 2017) %>% dplyr::select(area, rate_2017 = rate)

heat_df <- county_allages %>%
    left_join(baseline_2010, by = "area") %>%
    left_join(baseline_2017, by = "area") %>%
    mutate(
        diff_from_2010 = rate - rate_2010,
        diff_from_2017 = rate - rate_2017
    )

p_heat_2010 <- ggplot(heat_df, aes(x = factor(year), y = fct_reorder(area, rate_2010), fill = diff_from_2010)) +
    geom_tile() +
    scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0) +
    labs(
        title = "County Heatmap: Change in Respiratory Rates vs 2010",
        subtitle = "Both sexes · All ages. Red = higher than 2010 baseline, Blue = lower.",
        x = "Year", y = "County", fill = "Δ vs 2010"
    ) +
    theme_bordered()
print(p_heat_2010)

p_heat_2017 <- ggplot(heat_df, aes(x = factor(year), y = fct_reorder(area, rate_2017), fill = diff_from_2017)) +
    geom_tile() +
    scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0) +
    labs(
        title = "County Heatmap: Change in Respiratory Rates vs 2017",
        subtitle = "Both sexes · All ages. Helps check continuity against preserved period.",
        x = "Year", y = "County", fill = "Δ vs 2017"
    ) +
    theme_bordered()
print(p_heat_2017)

# ---------- 3) TOP MOVERS: biggest rises/falls 2010→2024 ----------
top_moves <- county_allages %>%
    filter(year %in% c(2010, 2024)) %>%
    pivot_wider(names_from = year, values_from = rate, names_prefix = "y") %>%
    mutate(change_10_24 = y2024 - y2010) %>%
    arrange(desc(abs(change_10_24)))

p_top_up <- top_moves %>%
    slice_max(change_10_24, n = 10) %>%
    ggplot(aes(x = change_10_24, y = fct_reorder(area, change_10_24))) +
    geom_col() +
    labs(
        title = "Largest Increases in Respiratory Rates (2010 → 2024)",
        subtitle = "Both sexes · All ages · County-level",
        x = "Change in rate per 1,000", y = "County"
    ) +
    theme_bordered()
print(p_top_up)

p_top_down <- top_moves %>%
    slice_min(change_10_24, n = 10) %>%
    ggplot(aes(x = change_10_24, y = fct_reorder(area, change_10_24))) +
    geom_col() +
    labs(
        title = "Largest Decreases in Respiratory Rates (2010 → 2024)",
        subtitle = "Both sexes · All ages · County-level",
        x = "Change in rate per 1,000", y = "County"
    ) +
    theme_bordered()
print(p_top_down)

# ---------- 4) SEX GAP over time (Ireland) ----------
sex_gap <- resp %>%
    filter(age_group == "All ages", area == "Ireland", sex %in% c("Male","Female")) %>%
    group_by(year, sex) %>%
    summarise(rate = mean(value, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = sex, values_from = rate) %>%
    mutate(gap_female_minus_male = Female - Male)

p_gap <- ggplot(sex_gap, aes(year, gap_female_minus_male)) +
    geom_hline(yintercept = 0, color = "grey60") +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    labs(
        title = "Sex Gap: Female minus Male Respiratory Rates (Ireland, All ages)",
        subtitle = "Positive values → Female rate higher than Male",
        x = "Year", y = "Female − Male (per 1,000)"
    ) +
    theme_bordered()
print(p_gap)

# ---------- 5) AGE PROFILE SHIFT (Ireland, Both sexes) ----------
age_profile <- resp %>%
    filter(area == "Ireland", sex == "Both sexes", year %in% c(2010, 2016, 2017, 2024)) %>%
    group_by(year, age_group) %>%
    summarise(rate = mean(value, na.rm = TRUE), .groups = "drop")

p_age <- ggplot(age_profile, aes(x = age_group, y = rate, group = factor(year), color = factor(year))) +
    geom_line(linewidth = 1) + geom_point(size = 2) +
    labs(
        title = "Age Profile of Respiratory Rates (Ireland, Both sexes)",
        subtitle = "Compare shape at the start/end of imputation window and the preserved/imputed boundary",
        x = "Age group", y = "Rate per 1,000", color = "Year"
    ) +
    theme_bordered() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
print(p_age)

# ---------- 6) OUTLIER YEARS by COUNTY using z-scores (baseline 2010–2019) ----------
baseline_years <- 2010:2019
z_df <- county_allages %>%
    group_by(area) %>%
    mutate(
        mu = mean(rate[year %in% baseline_years], na.rm = TRUE),
        sdv = sd(rate[year %in% baseline_years], na.rm = TRUE),
        z   = (rate - mu) / ifelse(sdv == 0 | is.na(sdv), 1, sdv)
    ) %>%
    ungroup()

p_outliers <- z_df %>%
    mutate(is_outlier = abs(z) >= 2) %>%
    ggplot(aes(year, z, group = area)) +
    geom_hline(yintercept = c(-2, 0, 2), linetype = c("dashed","solid","dashed"), color = "grey50") +
    geom_line(alpha = 0.3) +
    geom_point(data = ~subset(.x, is_outlier), color = "firebrick", size = 2) +
    labs(
        title = "Outlier Years by County (z-score vs 2010–2019 baseline)",
        subtitle = "Points beyond ±2σ flagged as outliers",
        x = "Year", y = "z-score"
    ) +
    theme_bordered()
print(p_outliers)

# ---------- 7) SMALL MULTIPLES: top-6 counties by 2010→2024 growth ----------
top6 <- top_moves %>% slice_max(change_10_24, n = 6) %>% pull(area)

p_small <- county_allages %>%
    filter(area %in% top6) %>%
    ggplot(aes(year, rate)) +
    geom_line(linewidth = 1) + geom_point(size = 1.8) +
    facet_wrap(~ area, scales = "free_y") +
    labs(
        title = "Top-6 Counties by Respiratory Rate Growth (2010 → 2024)",
        subtitle = "Both sexes · All ages",
        x = "Year", y = "Rate per 1,000"
    ) +
    theme_bordered()
print(p_small)





# -------------------- Hypothesis Testing---------------------------------------------------
# H1.1 & H1.2 — Manufacturing pollutants vs respiratory health (annual)

# --- 1) Prepare respiratory health data (Both sexes, respiratory) ---
resp_health <- disch_1000_impdata[
    disch_1000_impdata$sex == "Both sexes" &
        grepl("resp", tolower(disch_1000_impdata$high_level_ishmt)), 
]
resp_health <- aggregate(
    value_imputed ~ year,
    data = resp_health,
    FUN = function(x) mean(x, na.rm = TRUE)
)
names(resp_health)[2] <- "Avg_Resp_DischargeRate"

# --- 2) Filter manufacturing CO emissions ---
emis_manu_all <- filtered_emis_data[
    grepl("^Manufactur", filtered_emis_data$nace_rev_2_sector, ignore.case = TRUE) &
        grepl("tonne", filtered_emis_data$unit, ignore.case = TRUE) &
        grepl("carbon monoxide|\\bCO\\b", filtered_emis_data$statistic_label, ignore.case = TRUE),
]


# --- 3) Aggregate CO emissions by year ---
emis_year_co <- aggregate(
    value ~ year,
    data = emis_manu_all,
    FUN = function(x) sum(x, na.rm = TRUE)
)
names(emis_year_co)[2] <- "CO"

# --- 4) Merge with respiratory health data ---
merged_co <- merge(emis_year_co, resp_health, by = "year")
merged_co <- merged_co[order(merged_co$year), ]

cat("Merged rows:", nrow(merged_co), "\n")
print(merged_co)

# --- 5) Hypothesis 1.1 — CO -> Health (OLS + Newey–West) ---
library(lmtest)
library(sandwich)

m_h11_co <- lm(Avg_Resp_DischargeRate ~ CO, data = merged_co)
cat("\n=== H1.1 (CO tonnes) — OLS ===\n")
print(summary(m_h11_co))

cat("\n=== H1.1 (CO tonnes) — Newey–West SEs (lag=1) ===\n")
print(coeftest(m_h11_co, vcov = NeweyWest(m_h11_co, lag = 1, prewhite = FALSE)))

# --- 6) Hypothesis 1.2 — Mediation-style check: NOx -> CO -> Health ---
# First check if NOx is in dataset
emis_manu_nox <- filtered_emis_data[
    grepl("^Manufactur", filtered_emis_data$nace_rev_2_sector, ignore.case = TRUE) &
        grepl("tonne", filtered_emis_data$unit, ignore.case = TRUE) &
        grepl("nitrogen oxides|NOX", filtered_emis_data$statistic_label, ignore.case = TRUE),
]
emis_year_nox <- aggregate(
    value ~ year,
    data = emis_manu_nox,
    FUN = function(x) sum(x, na.rm = TRUE)
)
names(emis_year_nox)[2] <- "NOx"

# Merge NOx with merged_co
merged_co_nox <- merge(merged_co, emis_year_nox, by = "year")

if ("NOx" %in% names(merged_co_nox)) {
    # Path a: NOx -> CO
    a_mod <- lm(CO ~ NOx, data = merged_co_nox)
    cat("\n=== H1.2 Path a (NOx -> CO) — Newey–West ===\n")
    print(coeftest(a_mod, vcov = NeweyWest(a_mod, lag = 1, prewhite = FALSE)))
    
    # Path b: CO -> Health
    b_mod <- lm(Avg_Resp_DischargeRate ~ CO, data = merged_co_nox)
    cat("\n=== H1.2 Path b (CO -> Health) — Newey–West ===\n")
    print(coeftest(b_mod, vcov = NeweyWest(b_mod, lag = 1, prewhite = FALSE)))
    
    # Combined: Health ~ NOx + CO
    c_mod <- lm(Avg_Resp_DischargeRate ~ NOx + CO, data = merged_co_nox)
    cat("\n=== H1.2 Combined (Health ~ NOx + CO) — Newey–West ===\n")
    print(coeftest(c_mod, vcov = NeweyWest(c_mod, lag = 1, prewhite = FALSE)))
    
} else {
    cat("\nH1.2 skipped: NOx column not available after merge.\n")
}

# Hypothesis 1.2 using Mediator

## ===== 0) Build Respiratory Health: Both sexes, respiratory only =====
stopifnot(exists("disch_1000_impdata"))
resp_health <- disch_1000_impdata %>%
    filter(
        sex == "Both sexes",
        grepl("resp", tolower(high_level_ishmt))
    ) %>%
    group_by(year) %>%
    summarise(Avg_Resp_DischargeRate = mean(value_imputed, na.rm = TRUE), .groups = "drop")

cat("Resp health years:", paste(range(resp_health$year), collapse=" - "), 
    "| n=", nrow(resp_health), "\n")

## ===== 1) Rebuild MANUFACTURING emissions from RAW long table, including CO =====
stopifnot(exists("filtered_emis_data"))

# Keep manufacturing + tonnes only
emis_long <- filtered_emis_data %>%
    filter(
        grepl("^Manufactur", nace_rev_2_sector, ignore.case = TRUE),
        grepl("tonne", unit, ignore.case = TRUE)
    ) %>%
    # Map statistic_label to pollutant keys (incl. CO)
    mutate(
        pollutant = case_when(
            grepl("pm\\s*2\\.?5|pm2\\.?5|PM2_5", statistic_label, ignore.case = TRUE) ~ "PM25",
            grepl("pm\\s*10|pm10", statistic_label, ignore.case = TRUE)                ~ "PM10",
            grepl("nitrogen oxides|\\bNOX\\b", statistic_label, ignore.case = TRUE)    ~ "NOx",
            grepl("sulphur|SOX|SO2", statistic_label, ignore.case = TRUE)              ~ "SOx",
            grepl("volatile organic|NMV", statistic_label, ignore.case = TRUE)         ~ "NMVOC",
            grepl("\\bNH3\\b|ammonia", statistic_label, ignore.case = TRUE)            ~ "NH3",
            grepl("carbon monoxide|\\(CO\\)|\\bCO\\b", statistic_label, ignore.case = TRUE) ~ "CO",
            TRUE ~ NA_character_
        )
    ) %>%
    filter(!is.na(pollutant))

# Sanity check what we actually have:
cat("Unique mapped pollutants in raw ->", paste(sort(unique(emis_long$pollutant)), collapse=", "), "\n")

# Aggregate all manufacturing subsectors per year & pollutant, then pivot wide
emis_year_wide <- emis_long %>%
    group_by(year, pollutant) %>%
    summarise(total_tonnes = sum(value, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = pollutant, values_from = total_tonnes, values_fill = 0) %>%
    arrange(year)

cat("Emissions columns after pivot:", paste(names(emis_year_wide), collapse=", "), "\n")

if (!"CO" %in% names(emis_year_wide)) {
    stop("CO column still not found after pivot. Check your 'statistic_label' strings; print a sample to verify mapping.")
}

## ===== 2) Merge with health =====
merged <- emis_year_wide %>%
    inner_join(resp_health, by = "year") %>%
    arrange(year)

cat("Merged years:", paste(range(merged$year), collapse=" - "), 
    "| n=", nrow(merged), "\n")
print(head(merged, 5))

if (nrow(merged) < 6) warning("Few years after merge (n < 6) — results will be fragile.")

## ===== 3) H1.1 — Does CO (manufacturing emissions, tonnes) predict respiratory discharges? =====
m_h11_co <- lm(Avg_Resp_DischargeRate ~ CO, data = merged)
cat("\n=== H1.1 (CO) — OLS ===\n")
print(summary(m_h11_co))

cat("\n=== H1.1 (CO) — Newey–West SEs (lag=1) ===\n")
print(coeftest(m_h11_co, vcov = NeweyWest(m_h11_co, lag = 1, prewhite = FALSE)))

# Plot
p_co <- ggplot(merged, aes(CO, Avg_Resp_DischargeRate)) +
    geom_point(size = 3, alpha = 0.85) +
    geom_smooth(method = "lm", se = TRUE) +
    labs(title = "Respiratory Discharge Rate vs Manufacturing CO Emissions",
         x = "CO Emissions (tonnes, manufacturing total)",
         y = "Avg Respiratory Discharge Rate") +
    theme_minimal(base_size = 13)
print(p_co)

## ===== 4) H1.2 — “Mediation-shaped” check: CO -> (PM25 or NOx) -> Health =====
# Choose a mediator we actually have: PM25 preferred, fallback NOx, otherwise skip
mediator <- if ("PM25" %in% names(merged)) "PM25" else if ("NOx" %in% names(merged)) "NOx" else NA_character_

if (!is.na(mediator)) {
    cat("\nUsing mediator:", mediator, "\n")
    
    # Path a: Mediator ~ CO
    form_a <- as.formula(paste(mediator, "~ CO"))
    a_mod <- lm(form_a, data = merged)
    cat("\n=== H1.2 Path a —", mediator, "~ CO (Newey–West) ===\n")
    print(coeftest(a_mod, vcov = NeweyWest(a_mod, lag = 1, prewhite = FALSE)))
    
    # Path b: Health ~ Mediator
    form_b <- as.formula(paste("Avg_Resp_DischargeRate ~", mediator))
    b_mod <- lm(form_b, data = merged)
    cat("\n=== H1.2 Path b — Health ~", mediator, "(Newey–West) ===\n")
    print(coeftest(b_mod, vcov = NeweyWest(b_mod, lag = 1, prewhite = FALSE)))
    
    # Combined: Health ~ CO + Mediator
    form_c <- as.formula(paste("Avg_Resp_DischargeRate ~ CO +", mediator))
    c_mod <- lm(form_c, data = merged)
    cat("\n=== H1.2 Combined — Health ~ CO +", mediator, "(Newey–West) ===\n")
    print(coeftest(c_mod, vcov = NeweyWest(c_mod, lag = 1, prewhite = FALSE)))
    
    cat("\nVIF (Combined):\n")
    print(vif(c_mod))
    
    # Compare against CO-only
    AICc <- function(m) { k <- length(coef(m)); n <- nobs(m); AIC(m) + (2*k*(k+1))/(n - k - 1) }
    cat("\nR^2 CO-only:", round(summary(m_h11_co)$r.squared, 3),
        " | R^2 CO +", mediator, ":", round(summary(c_mod)$r.squared, 3), "\n")
    
AICc <- function(m) {
        k <- length(coef(m))
        n <- nobs(m)
        AIC(m) + (2 * k * (k + 1)) / (n - k - 1)}
    
    
    cat("AICc CO-only:", round(AICc(m_h11_co), 2),
        " | AICc CO +", mediator, ":", round(AICc(c_mod), 2), "\n")
    
    # Quick visual: mediator vs Health
    p_med <- ggplot(merged, aes_string(mediator, "Avg_Resp_DischargeRate")) +
        geom_point() +
        geom_smooth(method = "lm", se = TRUE) +
        labs(title = paste(mediator, "Emissions vs Respiratory Discharges"),
             x = paste(mediator, "(tonnes, manufacturing total)"), y = "Avg Respiratory Discharge Rate") +
        theme_minimal()
    print(p_med)
    
} else {
    message("H1.2 skipped: Neither PM25 nor NOx exist in merged after pivot.")
}

#---------------------------- Hypothesis Testing 2.1---------------------------------



# ---------- 1) Standardise pollutant names from emis_df$statistic_label ----------
standardize_pollutant <- function(x){
    y <- tolower(trimws(x))
    y <- str_replace_all(y, "0", "o")                    # fix NMV0C -> NMVOC
    y <- str_replace_all(y, "\\(.*?\\)", "")             # drop parentheses content
    y <- str_replace_all(y, "<\\s*2\\.5\\s*micrometres.*", " pm25")
    y <- str_replace_all(y, "<\\s*10\\s*micrometres.*",  " pm10")
    y <- str_replace_all(y, "equivalent", "")
    y <- str_replace_all(y, "in\\s+no2", "")
    y <- str_replace_all(y, "in\\s+so2", "")
    y <- str_replace_all(y, "[^a-z0-9]+", " ")
    y <- str_squish(y)
    
    dplyr::case_when(
        str_detect(y, "\\bpm\\s*25\\b|\\bpm25\\b")                ~ "PM25",
        str_detect(y, "\\bpm\\s*10\\b|\\bpm10\\b")                ~ "PM10",
        str_detect(y, "\\bnox\\b|nitrogen ox")                    ~ "NOx",
        str_detect(y, "\\bsox\\b|sulphur ox|sulfur ox")           ~ "SOx",
        str_detect(y, "nmvoc|non methane volatile")               ~ "NMVOC",
        str_detect(y, "\\bnh3\\b|ammonia")                        ~ "NH3",
        str_detect(y, "\\bco2\\b|carbon diox")                    ~ "CO2",
        str_detect(y, "^co\\b|carbon monox")                      ~ "CO",
        TRUE                                                      ~ NA_character_
    )
}

stopifnot(exists("emis_df"), is.data.frame(emis_df))
stopifnot(all(c("statistic_label","year","value") %in% names(emis_df)))

# ---------- 2) Build wide year-by-pollutant table, then yearly totals ----------
emis_year_pollutants <- emis_df %>%
    transmute(
        year  = suppressWarnings(as.numeric(year)),
        pol   = standardize_pollutant(statistic_label),
        val   = suppressWarnings(as.numeric(value))
    ) %>%
    filter(!is.na(year), !is.na(pol), !is.na(val)) %>%
    group_by(year, pol) %>%
    summarise(value = sum(val, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = pol, values_from = value) %>%
    arrange(year)

emis_summary <- emis_year_pollutants %>%
    mutate(across(-year, ~ suppressWarnings(as.numeric(.)))) %>%
    mutate(Total_Emissions = rowSums(across(-year), na.rm = TRUE)) %>%
    dplyr::select(year, Total_Emissions)

# ---------- 3) Predictor Xvar from yearly totals ----------
emis_predictor <- emis_summary %>%
    transmute(
        year = suppressWarnings(as.numeric(year)),
        Xvar = suppressWarnings(as.numeric(Total_Emissions))
    ) %>%
    filter(!is.na(year), !is.na(Xvar)) %>%
    group_by(year) %>%
    summarise(Xvar = sum(Xvar, na.rm = TRUE), .groups = "drop") %>%
    arrange(year)

x_label <- "Total Manufacturing Emissions (tonnes)"

# ---------- 4) Climate aggregates ----------
stopifnot(exists("climate_imputed"), is.data.frame(climate_imputed))
stopifnot(exists("resp_health"), is.data.frame(resp_health))
stopifnot(all(c("year","Avg_Resp_DischargeRate") %in% names(resp_health)))

climate_imputed$year <- suppressWarnings(as.numeric(climate_imputed$year))
needed_climate_cols <- c("precipitation_mm","mean_air_temp_c","mean_max_temp_c","mean_min_temp_c","grass_min_temp_c")
miss <- setdiff(needed_climate_cols, names(climate_imputed))
if (length(miss) > 0) stop(paste("climate_imputed is missing:", paste(miss, collapse=", ")))

climate_annual <- climate_imputed %>%
    group_by(year) %>%
    summarise(
        Avg_Precip         = mean(precipitation_mm,  na.rm = TRUE),
        Avg_Air_Temp       = mean(mean_air_temp_c,   na.rm = TRUE),
        Avg_Max_Temp       = mean(mean_max_temp_c,   na.rm = TRUE),
        Avg_Min_Temp       = mean(mean_min_temp_c,   na.rm = TRUE),
        Avg_Grass_Min_Temp = mean(grass_min_temp_c,  na.rm = TRUE),
        .groups = "drop"
    ) %>% arrange(year)

resp_health <- resp_health %>%
    mutate(year = suppressWarnings(as.numeric(year))) %>%
    filter(!is.na(year)) %>%
    group_by(year) %>%
    summarise(Avg_Resp_DischargeRate = mean(Avg_Resp_DischargeRate, na.rm = TRUE), .groups = "drop") %>%
    arrange(year)

# ---------- 5) H2.1 ----------
merged_h2 <- emis_predictor %>%
    inner_join(resp_health, by = "year") %>%
    inner_join(climate_annual, by = "year") %>%
    arrange(year)

cat("H2.1 — rows after merge:", nrow(merged_h2), "\n")
if (nrow(merged_h2) < 6) warning("Few overlapping years — inference will be fragile.\n")

merged_h2 <- merged_h2 %>%
    mutate(
        Xvar_c = as.numeric(scale(Xvar, center = TRUE, scale = FALSE)),
        Temp_c = as.numeric(scale(Avg_Air_Temp, center = TRUE, scale = FALSE))
    )

m_e   <- lm(Avg_Resp_DischargeRate ~ Xvar_c,          data = merged_h2)
m_e_t <- lm(Avg_Resp_DischargeRate ~ Xvar_c + Temp_c, data = merged_h2)

cat("\n=== H2.1: Emissions-only ===\n"); print(summary(m_e))
cat("\n=== H2.1: Emissions + Avg_Air_Temp (additive) ===\n"); print(summary(m_e_t))
cat("\nR² Emissions only:", round(summary(m_e)$r.squared, 3),
    " | R² +Temp:", round(summary(m_e_t)$r.squared, 3), "\n")
print(AIC(m_e, m_e_t))
cat("\nANOVA (nested):\n"); print(anova(m_e, m_e_t))

p_h21 <- ggplot(merged_h2, aes(x = Xvar, y = Avg_Resp_DischargeRate)) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", se = TRUE) +
    labs(title = "H2.1: Respiratory health vs emissions",
         subtitle = "The fitted line indicates a positive trend, but high variability shows emissions explain only part of health outcomes",
         x = x_label, y = "Avg Respiratory Discharge Rate")
print(p_h21)

# ---------- 6) H2.2 ----------
if (exists("rain_filled_1990") && is.data.frame(rain_filled_1990)) {
    rain_col <- intersect(c("Total Rainfall","total_rainfall","total_rain","rain_mm"), names(rain_filled_1990))
    stopifnot(length(rain_col) >= 1)
    rc <- rain_col[1]
    rain_filled_1990$year <- suppressWarnings(as.numeric(rain_filled_1990$year))
    rain_annual <- rain_filled_1990 %>%
        group_by(year) %>%
        summarise(Avg_Annual_Rainfall = mean(.data[[rc]], na.rm = TRUE), .groups = "drop") %>%
        filter(!is.na(year))
} else {
    rain_annual <- climate_annual %>% select(year, Avg_Annual_Rainfall = Avg_Precip)
}

temp_thr <- quantile(climate_annual$Avg_Max_Temp,          0.90, na.rm = TRUE)
rain_thr <- quantile(rain_annual$Avg_Annual_Rainfall,      0.90, na.rm = TRUE)

extreme_flags <- climate_annual %>%
    left_join(rain_annual, by = "year") %>%
    mutate(
        ExtremeTemp        = as.integer(Avg_Max_Temp >= temp_thr),
        ExtremeRain        = as.integer(Avg_Annual_Rainfall >= rain_thr),
        ExtremeClimateYear = as.integer(ExtremeTemp == 1 | ExtremeRain == 1)
    ) %>%
    dplyr::select(year, ExtremeClimateYear) %>%
    distinct()

merged_extreme <- emis_predictor %>%
    inner_join(resp_health, by = "year") %>%
    left_join(extreme_flags, by = "year") %>%
    mutate(
        ExtremeClimateYear = ifelse(is.na(ExtremeClimateYear), 0L, ExtremeClimateYear),
        Xvar_c = as.numeric(scale(Xvar, center = TRUE, scale = FALSE))
    ) %>%
    arrange(year)

cat("\nH2.2 — rows after merge:", nrow(merged_extreme), "\n")
print(table(ExtremeClimateYear = merged_extreme$ExtremeClimateYear))

m_int  <- lm(Avg_Resp_DischargeRate ~ Xvar_c * ExtremeClimateYear, data = merged_extreme)
m_norm <- lm(Avg_Resp_DischargeRate ~ Xvar_c, data = subset(merged_extreme, ExtremeClimateYear == 0))
m_ext  <- lm(Avg_Resp_DischargeRate ~ Xvar_c, data = subset(merged_extreme, ExtremeClimateYear == 1))

cat("\n=== H2.2: Interaction model (Emissions × ExtremeClimateYear) ===\n"); print(summary(m_int))
cat("\nNormal-year slope:\n"); print(summary(m_norm)$coefficients)
cat("\nExtreme-year slope:\n"); print(summary(m_ext)$coefficients)

merged_extreme$Extreme_Label <- ifelse(merged_extreme$ExtremeClimateYear == 1, "Extreme Year", "Normal Year")
p_h22 <- ggplot(merged_extreme, aes(x = Xvar, y = Avg_Resp_DischargeRate, color = Extreme_Label)) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = "H2.2: Emissions → Respiratory health, by climate year type",
         subtitle = "Separate linear fits for Normal vs Extreme years",
         x = x_label, y = "Avg Respiratory Discharge Rate", color = "Climate") +
    theme_minimal(base_size = 13)
print(p_h22)

cat("\nDone. Built yearly emissions from emis_df and ran H2.1/H2.2.\n")


        



# --------------- Hypothesis Testing 3.1 ---------------------------------------

# =========================
# H3.1 — Do emissions improve health forecasts?
# Models: ARIMA (no X) vs ARIMAX (with emissions as xreg)


# ---- 1) Build the working dataframe (Year, Resp, Emis) ----
# We try to be flexible with your objects. We end up with df <- data.frame(year, resp, emis)

make_df <- function() {
    # CASE A: you already have a 'data' frame with expected columns
    if (exists("data") && is.data.frame(data) &&
        all(c("year") %in% names(data)) &&
        any(grepl("^resp", names(data), ignore.case = TRUE)) &&
        any(grepl("manufacturing|emis|PM25|CO|Total_Emissions", names(data), ignore.case = TRUE))) {
        
        resp_col <- names(data)[grepl("^resp", names(data), ignore.case = TRUE)][1]
        emis_col <- names(data)[grepl("manufacturing|emis|PM25|CO|Total_Emissions", names(data), ignore.case = TRUE)][1]
        
        out <- data %>%
            dplyr::select(year, resp = all_of(resp_col), emis = all_of(emis_col))
        return(out)
    }
    
    # CASE B: build from common objects in your workspace
    if (exists("resp_health") && is.data.frame(resp_health) &&
        all(c("year") %in% names(resp_health)) &&
        any(grepl("Avg_Resp_DischargeRate|resp", names(resp_health), ignore.case = TRUE))) {
        
        resp_col <- names(resp_health)[grepl("Avg_Resp_DischargeRate|resp", names(resp_health), ignore.case = TRUE)][1]
        df_resp <- resp_health %>% dplyr::select(year, resp = all_of(resp_col))
        
        # emissions: prefer emis_summary$Total_Emissions, then emis_year_pollutants$PM25, else any obvious emissions col
        if (exists("emis_summary") && is.data.frame(emis_summary) &&
            all(c("year","Total_Emissions") %in% names(emis_summary))) {
            df_emis <- emis_summary %>% dplyr::select(year, emis = Total_Emissions)
        } else if (exists("emis_year_pollutants") && is.data.frame(emis_year_pollutants) &&
                   all(c("year","PM25") %in% names(emis_year_pollutants))) {
            df_emis <- emis_year_pollutants %>% dplyr::select(year, emis = PM25)
        } else if (exists("emis_predictor") && is.data.frame(emis_predictor) &&
                   all(c("year") %in% names(emis_predictor))) {
            # If you built Xvar earlier
            emis_col <- if ("Xvar" %in% names(emis_predictor)) "Xvar" else names(emis_predictor)[names(emis_predictor)!="year"][1]
            df_emis <- emis_predictor %>% dplyr::select(year, emis = all_of(emis_col))
        } else {
            stop("Could not find an emissions source. Provide emis_summary(Total_Emissions) or emis_year_pollutants(PM25).")
        }
        
        out <- df_resp %>%
            inner_join(df_emis, by = "year")
        
        return(out)
    }
    
    stop("Could not build dataframe. Provide either `data` with year/resp/emissions columns, or `resp_health` + emissions.")
}

df <- make_df()

# Basic hygiene
df <- df %>%
    mutate(year = as.integer(year)) %>%
    arrange(year) %>%
    distinct(year, .keep_all = TRUE)

# Drop rows with NA in either series
df <- df %>% filter(is.finite(resp), is.finite(emis))

cat("Years available:", min(df$year), "to", max(df$year), " — n =", nrow(df), "\n")

if (nrow(df) < 8) {
    warning("Very short annual series (n<8). ARIMA may be unstable; treat results as illustrative.")
}

# ---- 2) Create time series (annual) ----
resp_ts <- ts(df$resp, start = min(df$year), frequency = 1)
emis_ts <- ts(df$emis, start = min(df$year), frequency = 1)

# ---- 3) Fit models ----
# A) Baseline ARIMA with no exogenous regressor
model_noX <- auto.arima(resp_ts, stepwise = FALSE, approximation = FALSE)
# B) ARIMAX with emissions as xreg (aligned 1:1)
model_withX <- auto.arima(resp_ts, xreg = emis_ts, stepwise = FALSE, approximation = FALSE)

cat("\n=== Model summaries ===\n")
cat("\n[Model A] ARIMA (no emissions)\n"); print(summary(model_noX))
cat("\n[Model B] ARIMAX (with emissions)\n"); print(summary(model_withX))

cat("\nAIC comparison:\n")
cat("  AIC (no emissions)  :", model_noX$aic, "\n")
cat("  AIC (with emissions):", model_withX$aic, "\n")

# ---- 4) Simple hold-out evaluation (last 2 years) ----
# If series too short for 2-year holdout, fall back to 1 year.
h <- if (nrow(df) >= 10) 2 else 1

train_end <- max(df$year) - h
resp_train <- window(resp_ts, end = train_end)
emis_train <- window(emis_ts, end = train_end)
resp_test  <- window(resp_ts,  start = train_end + 1)
emis_test  <- window(emis_ts,  start = train_end + 1)

model_noX_tr  <- auto.arima(resp_train, stepwise = FALSE, approximation = FALSE)
model_withX_tr <- auto.arima(resp_train, xreg = emis_train, stepwise = FALSE, approximation = FALSE)

fc_noX   <- forecast(model_noX_tr,  h = h)
fc_withX <- forecast(model_withX_tr, xreg = emis_test, h = h)

cat("\n=== Hold-out accuracy (last", h, "year(s)) ===\n")
acc_noX   <- accuracy(fc_noX,   resp_test)
acc_withX <- accuracy(fc_withX, resp_test)

print(acc_noX[, c("ME","RMSE","MAE","MAPE")])
print(acc_withX[, c("ME","RMSE","MAE","MAPE")])

cat("\nSummary:\n")
cat("  RMSE (no emissions)   :", round(acc_noX["Test set","RMSE"], 3), "\n")
cat("  RMSE (with emissions) :", round(acc_withX["Test set","RMSE"], 3), "\n")
if (is.finite(acc_noX["Test set","RMSE"]) && is.finite(acc_withX["Test set","RMSE"])) {
    delta <- acc_noX["Test set","RMSE"] - acc_withX["Test set","RMSE"]
    cat("  ΔRMSE (noX - withX)   :", round(delta, 3), ifelse(delta > 0, "(with emissions improves)", "(no improvement)"), "\n")
}

# ---- 5) Optional: quick visual of fitted vs actual ----
library(ggplot2)

# Build a combined ts object
plot_data <- cbind(
    Actual        = resp_ts,
    Fitted_noX    = fitted(model_noX),
    Fitted_withX  = fitted(model_withX)
)

# Plot
autoplot(plot_data) +
    labs(
        title = "H3.1: Actual vs Fitted Values (ARIMA vs ARIMAX)",
        y     = "Respiratory Outcome",
        x     = "Year"
    ) +
    theme_minimal()


# -------------------- Hypothesis 3.2 -------------------------------------------
# ===========================================
# H3.2 — Scenario analysis (BAU vs Low) using yearly manufacturing emissions
# ===========================================


# ---------- Helpers ----------
detect_pollutant_code <- function(x){
    y <- tolower(trimws(x))
    y <- str_replace_all(y, "0", "o")                      # fix NMV0C -> NMVOC
    y <- str_replace_all(y, "\\(.*?\\)", "")               # drop (...) parts
    y <- str_replace_all(y, "<\\s*2\\.?5\\s*µ?m.*", " pm25")
    y <- str_replace_all(y, "<\\s*10\\s*µ?m.*",  " pm10")
    y <- str_replace_all(y, "equivalent|in\\s+no2|in\\s+so2|so2e", "")
    y <- str_replace_all(y, "[^a-z0-9]+", " ")
    y <- str_squish(y)
    
    dplyr::case_when(
        str_detect(y, "\\bpm\\s*25\\b|\\bpm25\\b|2\\.5")               ~ "PM25",
        str_detect(y, "\\bpm\\s*10\\b|\\bpm10\\b|\\b10\\b")             ~ "PM10",
        str_detect(y, "\\bnox\\b|nitrogen ox")                          ~ "NOx",
        str_detect(y, "\\bsox\\b|sulphur ox|sulfur ox")                 ~ "SOx",
        str_detect(y, "nmvoc|non methane volatile|n m v o c")           ~ "NMVOC",
        str_detect(y, "\\bnh3\\b|ammonia")                              ~ "NH3",
        str_detect(y, "\\bco2\\b|carbon diox")                          ~ "CO2",
        str_detect(y, "^co\\b|carbon monox")                            ~ "CO",
        TRUE                                                            ~ NA_character_
    )
}

long_to_wide <- function(df, year_col = "year", label_col = "statistic_label", value_col = "value") {
    stopifnot(all(c(year_col, label_col, value_col) %in% names(df)))
    df %>%
        transmute(
            year = suppressWarnings(as.numeric(.data[[year_col]])),
            pol  = detect_pollutant_code(.data[[label_col]]),
            val  = suppressWarnings(as.numeric(.data[[value_col]]))
        ) %>%
        filter(!is.na(year), !is.na(pol), !is.na(val)) %>%
        group_by(year, pol) %>%
        summarise(value = sum(val, na.rm = TRUE), .groups = "drop") %>%
        tidyr::pivot_wider(names_from = pol, values_from = value) %>%
        arrange(year)
}

build_emis_summary <- function() {
    # Prefer emis_manu_all (your long table)
    if (exists("emis_manu_all") && is.data.frame(emis_manu_all)) {
        wide <- long_to_wide(emis_manu_all, year_col = "year", label_col = "statistic_label", value_col = "value")
    } else if (exists("filtered_emis_data") && is.data.frame(filtered_emis_data)) {
        wide <- long_to_wide(filtered_emis_data, year_col = "year", label_col = "statistic_label", value_col = "value")
    } else if (exists("emis_df") && is.data.frame(emis_df)) {
        wide <- long_to_wide(emis_df, year_col = "year", label_col = "statistic_label", value_col = "value")
    } else {
        stop("Need one of: emis_manu_all, filtered_emis_data, or emis_df (long).")
    }
    
    pollutant_cols <- intersect(c("NH3","NMVOC","NOx","PM10","PM25","SOx","CO","CO2"), names(wide))
    if (length(pollutant_cols) == 0) {
        stop("Could not recognize any pollutant columns after pivot. Check statistic_label values.")
    }
    
    wide %>%
        mutate(across(all_of(pollutant_cols), ~ suppressWarnings(as.numeric(.)))) %>%
        mutate(total_emissions_tonnes = rowSums(across(all_of(pollutant_cols)), na.rm = TRUE)) %>%
        transmute(year = suppressWarnings(as.numeric(year)),
                  total_emissions_tonnes) %>%
        filter(!is.na(year)) %>%
        group_by(year) %>%
        summarise(total_emissions_tonnes = sum(total_emissions_tonnes, na.rm = TRUE), .groups = "drop") %>%
        arrange(year)
}

# ---------- 1) Build yearly emissions summary ----------
emis_summary <- build_emis_summary()
cat("Built emis_summary with", nrow(emis_summary), "year rows.\n")

# ---------- 2) Build resp_health (year, Avg_Resp_DischargeRate) ----------
if (exists("resp_health") && is.data.frame(resp_health) &&
    all(c("year","Avg_Resp_DischargeRate") %in% names(resp_health))) {
    
    resp_health <- resp_health %>% dplyr::select(year, Avg_Resp_DischargeRate)
    
} else if (exists("disch_1000_impdata") && is.data.frame(disch_1000_impdata)) {
    
    stopifnot(all(c("year","sex","high_level_ishmt","value_imputed") %in% names(disch_1000_impdata)))
    resp_health <- disch_1000_impdata %>%
        filter(sex == "Both sexes", grepl("resp", tolower(high_level_ishmt))) %>%
        group_by(year) %>%
        summarise(Avg_Resp_DischargeRate = mean(value_imputed, na.rm = TRUE), .groups = "drop")
    
} else {
    stop("No 'resp_health' or 'disch_1000_impdata' available to build respiratory outcomes.")
}

# Ensure numeric year
resp_health$year  <- as.numeric(resp_health$year)
emis_summary$year <- as.numeric(emis_summary$year)

# ---------- 3) Merge and run H3.2 Scenario Analysis ----------
data_h32 <- resp_health %>%
    rename(RespDischargeRate = Avg_Resp_DischargeRate) %>%
    inner_join(emis_summary %>% rename(Manufacturing_Emissions = total_emissions_tonnes),
               by = "year") %>%
    rename(Year = year) %>%
    dplyr::select(Year, RespDischargeRate, Manufacturing_Emissions) %>%
    arrange(Year)

cat("Merged rows for H3.2:", nrow(data_h32), "\n")
if (nrow(data_h32) < 8) warning("Few historical years (<8). Forecasts will be fragile.\n")

# Historical model (linear)
model_hist <- lm(RespDischargeRate ~ Manufacturing_Emissions, data = data_h32)
cat("\n=== H3.2: Historical model summary ===\n")
print(summary(model_hist))

# Future horizon to 2030 (adjust if needed)
hist_end    <- max(data_h32$Year, na.rm = TRUE)
target_year <- max(2030, hist_end + 1)
future_years <- seq(from = hist_end + 1, to = target_year, by = 1)
if (length(future_years) == 0) stop("No future years available (history already extends to target).")

scenario_df <- data.frame(Year = future_years)

# BAU emissions: linear extrapolation from history
em_trend <- lm(Manufacturing_Emissions ~ Year, data = data_h32)
scenario_df$Emissions_BAU <- predict(em_trend, newdata = scenario_df)

# Low emissions: ramp down to −51% of last observed by target_year
current_emis <- tail(data_h32$Manufacturing_Emissions, 1)
target_emis  <- current_emis * 0.49
scenario_df$Emissions_Low <- seq(from = current_emis,
                                 to   = target_emis,
                                 length.out = nrow(scenario_df))

# Predict health under scenarios
pred_BAU <- predict(model_hist,
                    newdata = data.frame(Manufacturing_Emissions = scenario_df$Emissions_BAU))
pred_Low <- predict(model_hist,
                    newdata = data.frame(Manufacturing_Emissions = scenario_df$Emissions_Low))

scenario_df <- scenario_df %>%
    mutate(
        Health_BAU = as.numeric(pred_BAU),
        Health_Low = as.numeric(pred_Low)
    )

# Report final-year difference
last_row <- tail(scenario_df, 1)
abs_diff  <- last_row$Health_BAU - last_row$Health_Low
perc_diff <- 100 * abs_diff / last_row$Health_BAU

cat("\n=== Target-year difference (", last_row$Year, ") ===\n", sep = "")
cat(sprintf("Predicted Health (BAU): %.2f\n", last_row$Health_BAU))
cat(sprintf("Predicted Health (Low): %.2f\n", last_row$Health_Low))
cat(sprintf("Absolute diff: %.2f | Percent diff: %.1f%%\n", abs_diff, perc_diff))

# ---------- 4) Plots ----------
# Health: historical + scenarios
plot_df <- bind_rows(
    data_h32 %>% transmute(Year, Scenario = "Historical", Health = RespDischargeRate),
    scenario_df %>% transmute(Year, Scenario = "BAU (pred)", Health = Health_BAU),
    scenario_df %>% transmute(Year, Scenario = "Low Emissions (pred)", Health = Health_Low)
)

p <- ggplot(plot_df, aes(Year, Health, color = Scenario)) +
    geom_line(linewidth = 1) +
    geom_point(data = subset(plot_df, Scenario == "Historical"), size = 2) +
    labs(
        title = "H3.2: Predicted Respiratory Health under Emission Scenarios",
        subtitle = "BAU (trend extrapolation) vs Low Emissions (−51% by target year)",
        y = "Respiratory Discharge Rate (per 100k)", x = "Year"
    ) +
    theme_minimal(base_size = 13)
print(p)

# Emissions paths
emis_paths <- bind_rows(
    data_h32 %>% transmute(Year, Scenario = "Historical Emissions", Emissions = Manufacturing_Emissions),
    scenario_df %>% transmute(Year, Scenario = "BAU Emissions", Emissions = Emissions_BAU),
    scenario_df %>% transmute(Year, Scenario = "Low Emissions", Emissions = Emissions_Low)
)

p_e <- ggplot(emis_paths, aes(Year, Emissions, color = Scenario)) +
    geom_line(linewidth = 1) +
    geom_point(data = subset(emis_paths, Scenario == "Historical Emissions"), size = 2) +
    labs(
        title = "Manufacturing Emissions: Historical + Scenario Paths",
        y = "Total Emissions (tonnes)", x = "Year"
    ) +
    theme_minimal(base_size = 13)
print(p_e)

cat("\nDone. Objects available: emis_summary, data_h32, model_hist, scenario_df.\n")



# Predictive Modelling begins from here
# Predict National Respiratory Cases for 2026 (Male & Female)
# Using: Linear Regression, ARIMAX, Random Forest
# =============================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(forecast)
  library(randomForest)
})

# --- 1) Prepare national totals ---
# Filter for respiratory system cases only (J00–J99)
resp_nat <- totaldisch_impdata %>%
  filter(high_level_ishmt == "Diseases of the respiratory system (J00-J99)",
         sex %in% c("Male", "Female")) %>%   # keep only male/female
  group_by(year, sex) %>%
  summarise(national_sum = sum(value_imputed, na.rm = TRUE), .groups = "drop") %>%
  arrange(sex, year)

# --- Check 2023 & 2024 actual sums ---
resp_nat %>% filter(year %in% c(2023, 2024))

# --- 2) Define evaluation metrics ---
rmse <- function(actual, pred) sqrt(mean((actual - pred)^2, na.rm = TRUE))
mape <- function(actual, pred) mean(abs((actual - pred)/actual), na.rm = TRUE) * 100

# --- 3) Modeling function ---
fit_and_predict <- function(df_sex, target_year = 2026, exclude_years = c(2020)) {
  
  # Remove anomaly years
  df_train <- df_sex %>% filter(!year %in% exclude_years)
  
  # Hold last 2 years for model testing
  cutoff <- max(df_train$year) - 1
  train <- df_train %>% filter(year <= cutoff)
  test  <- df_train %>% filter(year >  cutoff)
  
  # ---- Linear Regression ----
  lm_fit <- lm(national_sum ~ year, data = train)
  lm_pred <- predict(lm_fit, newdata = test)
  lm_rmse <- rmse(test$national_sum, lm_pred)
  lm_mape <- mape(test$national_sum, lm_pred)
  
  # ---- ARIMAX ----
  y_train <- ts(train$national_sum, start = min(train$year), frequency = 1)
  x_train <- train$year
  arimax_fit <- auto.arima(y_train, xreg = x_train)
  y_test <- test$year
  fc_arimax <- forecast(arimax_fit, xreg = y_test, h = length(y_test))
  ax_pred <- as.numeric(fc_arimax$mean)
  ax_rmse <- rmse(test$national_sum, ax_pred)
  ax_mape <- mape(test$national_sum, ax_pred)
  
  # ---- Random Forest ----
  train_rf <- train %>%
    arrange(year) %>%
    mutate(lag1 = lag(national_sum, 1),
           lag2 = lag(national_sum, 2)) %>%
    filter(!is.na(lag1), !is.na(lag2))
  test_rf <- test %>%
    arrange(year) %>%
    mutate(lag1 = lag(national_sum, 1),
           lag2 = lag(national_sum, 2))
  rf_pred <- rep(NA_real_, nrow(test_rf))
  if (nrow(train_rf) > 2) {
    rf_fit <- randomForest(national_sum ~ year + lag1 + lag2,
                           data = train_rf, ntree = 500)
    rf_pred <- predict(rf_fit, newdata = test_rf)
  }
  rf_rmse <- rmse(test$national_sum, rf_pred)
  rf_mape <- mape(test$national_sum, rf_pred)
  
  # ---- Retrain on full data & predict 2026 ----
  lm_full <- lm(national_sum ~ year, data = df_train)
  pred_lm_2026 <- predict(lm_full, newdata = tibble(year = target_year)) %>% as.numeric()
  
  y_full <- ts(df_train$national_sum, start = min(df_train$year), frequency = 1)
  x_full <- df_train$year
  arimax_full <- auto.arima(y_full, xreg = x_full)
  pred_ax_2026 <- forecast(arimax_full, xreg = target_year, h = 1)$mean %>% as.numeric()
  
  rf_pred_2026 <- NA_real_
  wf <- df_train %>%
    arrange(year) %>%
    mutate(lag1 = lag(national_sum, 1), lag2 = lag(national_sum, 2)) %>%
    filter(!is.na(lag1), !is.na(lag2))
  if (nrow(wf) >= 3) {
    rf_full <- randomForest(national_sum ~ year + lag1 + lag2, data = wf, ntree = 500)
    cur_year <- max(df_train$year)
    cur_val <- tail(df_train$national_sum, 1)
    prev_val <- df_train$national_sum[nrow(df_train)-1]
    while (cur_year < target_year) {
      next_year <- cur_year + 1
      new_row <- tibble(year = next_year, lag1 = cur_val, lag2 = prev_val)
      next_val <- predict(rf_full, newdata = new_row)
      prev_val <- cur_val
      cur_val <- next_val
      cur_year <- next_year
    }
    rf_pred_2026 <- cur_val
  }
  
  tibble(
    model = c("Linear", "ARIMAX", "RandomForest"),
    rmse = c(lm_rmse, ax_rmse, rf_rmse),
    mape = c(lm_mape, ax_mape, rf_mape),
    pred_cases_2026 = c(pred_lm_2026, pred_ax_2026, rf_pred_2026)
  )
}

# --- 4) Run models for each sex ---
results_nat <- resp_nat %>%
  group_by(sex) %>%
  group_modify(~ fit_and_predict(.x)) %>%
  ungroup()

print(results_nat)

# --- 5) Best model per sex ---
best_nat <- results_nat %>%
  group_by(sex) %>%
  arrange(rmse, mape) %>%
  slice(1) %>%
  ungroup()

print(best_nat)

# --- 6) Visualization of best predictions ---
ggplot(best_nat, aes(x = sex, y = pred_cases_2026, fill = sex)) +
    geom_col(width = 0.6, color = "black", linewidth = 0.4) +   # border around bars
    geom_text(aes(label = scales::comma(round(pred_cases_2026, 0))),
              vjust = -0.4, size = 5, fontface = "bold") +
    labs(
        title = "Predicted National Respiratory Cases (2026)",
        subtitle = "Best model per sex (2020 excluded)",
        x = "Sex",
        y = "Predicted total cases"
    ) +
    scale_y_continuous(labels = scales::comma,
                       expand = expansion(mult = c(0, 0.1))) +   # add headroom for labels
    theme_minimal(base_size = 14) +
    theme(
        legend.position = "none",
        panel.border = element_rect(color = "grey50", fill = NA, linewidth = 0.6),
        panel.grid.major.x = element_line(color = "grey85"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 12, color = "grey30")
    )



# =========================================================
# Predictive Modelling at County Level
# =========================================================

suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
    library(ggplot2)
    library(forecast)
    library(randomForest)
})

# --- 1) Prepare county totals ---
# Filter for respiratory system cases only (J00–J99)
resp_county <- totaldisch_impdata %>%
    filter(high_level_ishmt == "Diseases of the respiratory system (J00-J99)",
           sex %in% c("Male", "Female")) %>%   # keep only male/female
    group_by(year, area, sex) %>%
    summarise(county_sum = sum(value_imputed, na.rm = TRUE), .groups = "drop") %>%
    arrange(area, sex, year)

# --- Check 2023 & 2024 for sanity ---
resp_county %>% filter(year %in% c(2023, 2024)) %>% head()

# --- 2) Define metrics ---
rmse <- function(actual, pred) sqrt(mean((actual - pred)^2, na.rm = TRUE))
mape <- function(actual, pred) mean(abs((actual - pred)/actual), na.rm = TRUE) * 100

# --- 3) County modelling function (same logic as national) ---
fit_and_predict_county <- function(df_sex, target_year = 2026, exclude_years = c(2020)) {
    
    df_train <- df_sex %>% filter(!year %in% exclude_years)
    cutoff <- max(df_train$year) - 1
    train <- df_train %>% filter(year <= cutoff)
    test  <- df_train %>% filter(year >  cutoff)
    
    # ---- Linear Regression ----
    lm_fit <- lm(county_sum ~ year, data = train)
    lm_pred <- predict(lm_fit, newdata = test)
    lm_rmse <- rmse(test$county_sum, lm_pred)
    lm_mape <- mape(test$county_sum, lm_pred)
    
    # ---- ARIMAX ----
    y_train <- ts(train$county_sum, start = min(train$year), frequency = 1)
    x_train <- train$year
    arimax_fit <- auto.arima(y_train, xreg = x_train)
    y_test <- test$year
    fc_arimax <- forecast(arimax_fit, xreg = y_test, h = length(y_test))
    ax_pred <- as.numeric(fc_arimax$mean)
    ax_rmse <- rmse(test$county_sum, ax_pred)
    ax_mape <- mape(test$county_sum, ax_pred)
    
    # ---- Random Forest ----
    train_rf <- train %>%
        arrange(year) %>%
        mutate(lag1 = lag(county_sum, 1),
               lag2 = lag(county_sum, 2)) %>%
        filter(!is.na(lag1), !is.na(lag2))
    test_rf <- test %>%
        arrange(year) %>%
        mutate(lag1 = lag(county_sum, 1),
               lag2 = lag(county_sum, 2))
    rf_pred <- rep(NA_real_, nrow(test_rf))
    if (nrow(train_rf) > 2) {
        rf_fit <- randomForest(county_sum ~ year + lag1 + lag2,
                               data = train_rf, ntree = 500)
        rf_pred <- predict(rf_fit, newdata = test_rf)
    }
    rf_rmse <- rmse(test$county_sum, rf_pred)
    rf_mape <- mape(test$county_sum, rf_pred)
    
    # ---- Retrain full & predict 2026 ----
    lm_full <- lm(county_sum ~ year, data = df_train)
    pred_lm_2026 <- predict(lm_full, newdata = tibble(year = target_year)) %>% as.numeric()
    
    y_full <- ts(df_train$county_sum, start = min(df_train$year), frequency = 1)
    x_full <- df_train$year
    arimax_full <- auto.arima(y_full, xreg = x_full)
    pred_ax_2026 <- forecast(arimax_full, xreg = target_year, h = 1)$mean %>% as.numeric()
    
    rf_pred_2026 <- NA_real_
    wf <- df_train %>%
        arrange(year) %>%
        mutate(lag1 = lag(county_sum, 1), lag2 = lag(county_sum, 2)) %>%
        filter(!is.na(lag1), !is.na(lag2))
    if (nrow(wf) >= 3) {
        rf_full <- randomForest(county_sum ~ year + lag1 + lag2, data = wf, ntree = 500)
        cur_year <- max(df_train$year)
        cur_val <- tail(df_train$county_sum, 1)
        prev_val <- df_train$county_sum[nrow(df_train)-1]
        while (cur_year < target_year) {
            next_year <- cur_year + 1
            new_row <- tibble(year = next_year, lag1 = cur_val, lag2 = prev_val)
            next_val <- predict(rf_full, newdata = new_row)
            prev_val <- cur_val
            cur_val <- next_val
            cur_year <- next_year
        }
        rf_pred_2026 <- cur_val
    }
    
    tibble(
        model = c("Linear", "ARIMAX", "RandomForest"),
        rmse = c(lm_rmse, ax_rmse, rf_rmse),
        mape = c(lm_mape, ax_mape, rf_mape),
        pred_cases_2026 = c(pred_lm_2026, pred_ax_2026, rf_pred_2026)
    )
}

# --- 4) Run models for each county & sex ---
results_county <- resp_county %>%
    group_by(area, sex) %>%
    group_modify(~ fit_and_predict_county(.x)) %>%
    ungroup()

print(results_county)

# ---- De-cluttered county totals chart (excludes national/visitor rows) ----
suppressPackageStartupMessages({
    library(dplyr)
    library(ggplot2)
    library(stringr) # for str_to_lower
})

# 1) Choose which 'areas' to hide ONLY in the chart
exclude_areas_plot <- c(
    "Ireland",
    "Ireland (including non-residents)",
    "Ireland (residents and non-residents)",
    "Ireland incl. non-residents",
    "Non-residents"
)

best_county <- results_county %>%
    dplyr::mutate(
        rmse_rank = dplyr::if_else(is.na(rmse) | is.nan(rmse), Inf, rmse),
        mape_rank = dplyr::if_else(is.na(mape) | is.nan(mape), Inf, mape)
    ) %>%
    dplyr::group_by(area, sex) %>%
    dplyr::arrange(rmse_rank, mape_rank, .by_group = TRUE) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(-rmse_rank, -mape_rank)


best_county %>% dplyr::arrange(desc(pred_cases_2026)) %>% head(10)

# 2) Start from best_county and filter out rows you don't want to show
best_county_plot <- best_county %>%
    filter(!is.na(pred_cases_2026)) %>%
    mutate(area = as.character(area)) %>%
    filter(
        !str_to_lower(area) %in% str_to_lower(exclude_areas_plot),   # drop Ireland & non-residents variants
        !grepl("^ireland\\b", area, ignore.case = TRUE)              # extra safety
    )

# 3) Collapse to totals (sum across sexes) and order by size
best_county_totals <- best_county_plot %>%
    group_by(area) %>%
    summarise(total_pred_2026 = sum(pred_cases_2026, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_pred_2026))

# 4) Plot — single bar per county, readable labels, slight extra right margin for labels
ggplot(best_county_totals, aes(x = reorder(area, total_pred_2026), y = total_pred_2026)) +
    geom_col(fill = "steelblue", color = "grey25") +
    geom_text(aes(label = scales::comma(round(total_pred_2026))),
              hjust = -0.2, size = 3.5) +
    coord_flip(clip = "off") +
    scale_y_continuous(labels = scales::comma, expand = ggplot2::expansion(mult = c(0.02, 0.15))) +
    labs(
        title = "Predicted Respiratory Discharges by County (2026)",
        subtitle = "Total across sexes • Best model per county (Ireland & non-residents hidden)",
        x = "County", y = "Predicted discharges (2026)",
        caption = "Models compared: Linear, ARIMAX, Random Forest; 2020 excluded from training"
    ) +
    theme_minimal(base_size = 13) +
    theme(
        panel.grid.major.y = element_blank(),
        panel.border = element_rect(color = "grey70", fill = NA),
        plot.title = element_text(face = "bold")
    )


# ============================================================
# Predicting high-emitting manufacturing sectors & key pollutants
# Data expected: filtered_emis_data with columns:
#   statistic_label, year, nace_rev_2_sector, unit, value (Tonnes), log_value
# ============================================================

# ---- Packages ----
suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
    library(stringr)
    library(ggplot2)
    library(forecast)      # for simple per-sector forecasting
    library(randomForest)  # for pollutant importance
    library(glmnet)        # LASSO (cv.glmnet)
})

# ---- 0) Guard rails ----
stopifnot(all(c("statistic_label","year","nace_rev_2_sector","unit","value") %in% names(filtered_emis_data)))

# ---- 1) Tidy: map pollutant names and build sector-year wide table ----
emis_clean <- filtered_emis_data %>%
    # keep manufacturing + tonnes (your table is already manufacturing; being explicit is harmless)
    dplyr::filter(grepl("^Manufactur", nace_rev_2_sector, ignore.case = TRUE),
                  grepl("tonne", unit, ignore.case = TRUE)) %>%
    # unify pollutant names to tidy columns
    dplyr::mutate(
        pollutant = dplyr::case_when(
            grepl("pm\\s*2\\.?5|pm2\\.?5|PM2_5", statistic_label, ignore.case = TRUE) ~ "PM25",
            grepl("pm\\s*10|pm10", statistic_label, ignore.case = TRUE)                ~ "PM10",
            grepl("nitrogen oxides|\\bNOX\\b", statistic_label, ignore.case = TRUE)    ~ "NOx",
            grepl("sulphur|SOX|SO2", statistic_label, ignore.case = TRUE)              ~ "SOx",
            grepl("volatile organic|NMV", statistic_label, ignore.case = TRUE)         ~ "NMVOC",
            grepl("\\bNH3\\b|ammonia", statistic_label, ignore.case = TRUE)            ~ "NH3",
            grepl("carbon monoxide|\\bCO\\b", statistic_label, ignore.case = TRUE)     ~ "CO",
            TRUE ~ NA_character_
        )
    ) %>%
    dplyr::filter(!is.na(pollutant)) %>%
    dplyr::mutate(year = as.integer(year)) %>%
    group_by(nace_rev_2_sector, year, pollutant) %>%
    summarise(total_tonnes = sum(value, na.rm = TRUE), .groups = "drop")

# Pivot to wide: one row per sector-year, columns per pollutant
sector_year <- emis_clean %>%
    tidyr::pivot_wider(
        names_from = pollutant,
        values_from = total_tonnes,
        values_fill = 0
    ) %>%
    # total emissions per sector-year across pollutants
    dplyr::mutate(Sector_Total = rowSums(dplyr::across(c(PM25, PM10, NOx, SOx, NMVOC, NH3, CO), ~ .x), na.rm = TRUE)) %>%
    dplyr::arrange(nace_rev_2_sector, year)

# Quick peek
cat("Rows in sector_year:", nrow(sector_year), "\n")
print(head(sector_year))

# ============================================================
# PART A — Which sector will contribute the most by 2026?
# ============================================================

# Helper: small forecasting function per sector on Sector_Total
forecast_sector_total <- function(df_sector, horizon_year = 2026) {
    # expects df_sector: rows for a single sector with columns year, Sector_Total
    df_sector <- df_sector %>% arrange(year)
    yrs <- df_sector$year
    vals <- df_sector$Sector_Total
    if (length(vals) >= 6) {
        # use ARIMA if we have >= 6 observations
        ts_obj <- ts(vals, start = min(yrs), frequency = 1)
        fit <- suppressWarnings(auto.arima(ts_obj))
        h <- horizon_year - max(yrs)
        if (h <= 0) {
            # already have horizon year — take last observed
            fc_val <- vals[which.max(yrs)]
        } else {
            fc <- forecast(fit, h = h)
            fc_val <- as.numeric(tail(fc$mean, 1))
        }
    } else {
        # fallback: linear trend
        fit <- lm(Sector_Total ~ year, data = df_sector)
        fc_val <- predict(fit, newdata = data.frame(year = horizon_year))
    }
    return(fc_val)
}

# Compute latest totals and forecast 2026 per sector
latest_year <- max(sector_year$year, na.rm = TRUE)

sector_summary <- sector_year %>%
    group_by(nace_rev_2_sector) %>%
    summarise(
        Latest_Year = max(year, na.rm = TRUE),
        Latest_Total = Sector_Total[which.max(year)],
        Forecast_2026 = forecast_sector_total(dplyr::select(cur_data(), year, Sector_Total), horizon_year = 2026),
        .groups = "drop"
    ) %>%
    arrange(desc(Forecast_2026))

cat("\n=== Top 10 sectors by forecasted 2026 emissions ===\n")
print(head(sector_summary, 10))

# Plot top 10 forecast 2026
p_top10 <- sector_summary %>%
    slice_head(n = 10) %>%
    ggplot(aes(x = reorder(nace_rev_2_sector, Forecast_2026), y = Forecast_2026)) +
    geom_col() +
    coord_flip() +
    labs(
        title = "Forecasted manufacturing emissions by sector (2026)",
        x = "NACE Rev.2 sector",
        y = "Total emissions (tonnes, all pollutants)"
    ) +
    theme_minimal(base_size = 12)
print(p_top10)


library(dplyr)
library(ggplot2)
library(scales)
library(grid)

df_top10 <- sector_summary %>%
    slice_max(Forecast_2026, n = 10, with_ties = FALSE) %>%
    arrange(Forecast_2026) %>%
    mutate(nace_rev_2_sector = factor(nace_rev_2_sector, levels = nace_rev_2_sector))

# Identify the longest bar (max emissions)
longest_sector <- df_top10$nace_rev_2_sector[which.max(df_top10$Forecast_2026)]

ggplot(df_top10, aes(x = nace_rev_2_sector, y = Forecast_2026)) +
    geom_col(fill = "white", color = "white", width = 0.7) +
    
    # Label inside for the longest bar
    geom_text(
        data = subset(df_top10, nace_rev_2_sector == longest_sector),
        aes(label = nace_rev_2_sector, y = Forecast_2026 * 0.95),
        hjust = 1, color = "black", size = 3.6
    ) +
    
    # Labels outside for all other bars
    geom_text(
        data = subset(df_top10, nace_rev_2_sector != longest_sector),
        aes(label = nace_rev_2_sector, y = Forecast_2026 + max(df_top10$Forecast_2026) * 0.02),
        hjust = 0, color = "white", size = 3.6
    ) +
    
    coord_flip(clip = "off") +
    scale_y_continuous(labels = comma) +
    expand_limits(y = max(df_top10$Forecast_2026) * 1.1) +
    labs(
        title = "Forecasted manufacturing emissions by sector (2026)",
        x = NULL,
        y = "Total emissions (tonnes, all pollutants)"
    ) +
    theme_classic(base_size = 13) +
    theme(
        plot.background  = element_rect(fill = "#0b0f19", color = NA),
        panel.background = element_rect(fill = "#0b0f19", color = NA),
        axis.text        = element_text(color = "white"),
        axis.title       = element_text(color = "white"),
        axis.ticks       = element_line(color = "white"),
        panel.grid.major = element_line(color = "#2a2f3a"),
        panel.grid.minor = element_blank(),
        axis.text.y      = element_blank(),
        plot.title       = element_text(color = "white", face = "bold", size = 16),
        plot.margin      = unit(c(10, 40, 10, 10), "pt")
    )


👉 Now only the **first bar** (longest one) will have its label **inside in black**, while all others remain **outside in white**.

Do you also want me to make the **inside label bold** so it stands out against the white bar background?
    

# ============================================================
# PART B — Which pollutants should be controlled? (overall drivers)
#   Approach 1: Random Forest importance (non-linear, robust)
#   Approach 2: LASSO (sparse linear model)
# ============================================================

# Build modeling frame: predictors = pollutants, target = Sector_Total
pollutant_cols <- intersect(c("PM25","PM10","NOx","SOx","NMVOC","NH3","CO"), names(sector_year))
model_df <- sector_year %>%
    dplyr::select(all_of(pollutant_cols), Sector_Total) %>%
    as.data.frame()

# ---- B1) Random Forest importance ----
set.seed(123)
rf_fit <- randomForest(
    Sector_Total ~ .,
    data = model_df,
    ntree = 500,
    importance = TRUE,
    na.action = na.omit
)

cat("\nRandom Forest: %Var explained ~", round(rf_fit$rsq[length(rf_fit$rsq)] * 100, 1), "%\n")
imp <- importance(rf_fit, type = 1) # %IncMSE
imp_df <- data.frame(Pollutant = rownames(imp), IncMSE = imp[,1], row.names = NULL) %>%
    arrange(desc(IncMSE))

cat("\n=== Pollutant importance (Random Forest, %IncMSE) ===\n")
print(imp_df)

p_imp <- ggplot(imp_df, aes(x = reorder(Pollutant, IncMSE), y = IncMSE)) +
    geom_col() +
    coord_flip() +
    labs(title = "Pollutant importance for total emissions (Random Forest)",
         x = "Pollutant", y = "% Increase in MSE when permuted") +
    theme_minimal(base_size = 12)
print(p_imp)

library(dplyr)
library(ggplot2)
library(scales)
library(grid)  # for unit()

# Order for horizontal bars
imp_df2 <- imp_df %>%
    arrange(IncMSE) %>%
    mutate(Pollutant = factor(Pollutant, levels = Pollutant))

# padding so text sits just inside the right edge
pad <- max(imp_df2$IncMSE, na.rm = TRUE) * 0.03

ggplot(imp_df2, aes(x = Pollutant, y = IncMSE)) +
    # bars (mid-gray so white text pops)
    geom_col(fill = "white", color = "white", width = 0.7) +
    
    # pollutant names INSIDE bars, near the right edge
    geom_text(
        aes(
            # keep inside even for small bars
            y = pmax(IncMSE - pad, IncMSE * 0.15),
            label = Pollutant
        ),
        hjust = 1, color = "black", size = 3.6
    ) +
    
    coord_flip() +
    scale_y_continuous(labels = percent_format(scale = 1)) +
    labs(
        title = "Pollutant importance for total emissions (Random Forest)",
        x = "Pollutant",
        y = "% Increase in MSE when permuted"
    ) +
    
    # dark background (no theme_minimal)
    theme_classic(base_size = 13) +
    theme(
        plot.background  = element_rect(fill = "#0b0f19", color = NA),
        panel.background = element_rect(fill = "#0b0f19", color = NA),
        axis.text        = element_text(color = "white"),
        axis.title       = element_text(color = "white"),
        axis.ticks       = element_line(color = "white"),
        panel.grid.major = element_line(color = "#2a2f3a"),
        panel.grid.minor = element_blank(),
        # hide y-axis labels since names are inside bars
        axis.text.y      = element_blank(),
        plot.title       = element_text(color = "white", face = "bold", size = 16),
        plot.margin      = unit(c(10, 24, 10, 10), "pt")
    )

# ---- B2) LASSO (cv.glmnet) ----
x_mat <- as.matrix(model_df[, pollutant_cols, drop = FALSE])
y_vec <- model_df$Sector_Total

set.seed(123)
cvfit <- cv.glmnet(x_mat, y_vec, alpha = 1, standardize = TRUE, nfolds = 5)
lasso <- glmnet(x_mat, y_vec, alpha = 1, lambda = cvfit$lambda.min, standardize = TRUE)
coef_df <- data.frame(
    term = rownames(coef(lasso)),
    coef = as.numeric(coef(lasso))
) %>% dplyr::filter(term != "(Intercept)") %>% arrange(desc(abs(coef)))

cat("\n=== Pollutant coefficients (LASSO @ lambda.min) ===\n")
print(coef_df)

# ============================================================
# PART C — (Optional) Per-sector pollutant mix: who drives whom?
#   For a recent year, show each sector’s top pollutant share.
# ============================================================

recent_year <- latest_year

mix_recent <- sector_year %>%
    dplyr::filter(year == recent_year) %>%
    dplyr::select(nace_rev_2_sector, all_of(pollutant_cols), Sector_Total) %>%
    tidyr::pivot_longer(cols = all_of(pollutant_cols), names_to = "Pollutant", values_to = "Tonnes") %>%
    dplyr::mutate(Share = ifelse(Sector_Total > 0, Tonnes / Sector_Total, 0)) %>%
    group_by(nace_rev_2_sector) %>%
    slice_max(order_by = Share, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    arrange(desc(Share))

cat("\n=== For the most recent year (", recent_year, ") : top pollutant per sector ===\n", sep = "")
print(head(mix_recent, 10))

# Plot (top 10 sectors) — dominant pollutant share in recent year
p_mix <- mix_recent %>%
    slice_head(n = 10) %>%
    ggplot(aes(x = reorder(nace_rev_2_sector, Share), y = Share, fill = Pollutant)) +
    geom_col() +
    coord_flip() +
    scale_y_continuous(labels = scales::percent) +
    labs(
        title = paste0("Dominant pollutant share by sector (", recent_year, ")"),
        x = "NACE Rev.2 sector", y = "Share of sector total"
    ) +
    theme_minimal(base_size = 12)
print(p_mix)



# =========================================================
# Modeling & Validation: Sector Leaders and Priority Pollutants (to 2026)
# Data required in memory: filtered_emis_data
# Columns expected: statistic_label, year, nace_rev_2_sector, unit, value
# =========================================================

# ---------- 0) Packages ----------
need <- c("dplyr","tidyr","stringr","ggplot2","forecast","randomForest","glmnet","broom","scales","purrr")
to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, quiet = TRUE)
invisible(lapply(need, library, character.only = TRUE))

# ---------- 1) Clean & reshape emissions ----------
stopifnot(exists("filtered_emis_data"), is.data.frame(filtered_emis_data))

emis_raw <- filtered_emis_data %>%
    # keep tonnes only and positive/non-missing values
    filter(!is.na(value), value >= 0, grepl("tonne", unit, ignore.case = TRUE)) %>%
    mutate(
        # standardize pollutant names to compact keys
        pollutant = dplyr::case_when(
            grepl("pm\\s*2\\.?5|pm2\\.?5|PM2_5", statistic_label, ignore.case = TRUE) ~ "PM25",
            grepl("pm\\s*10|pm10", statistic_label, ignore.case = TRUE)                ~ "PM10",
            grepl("nitrogen oxides|NOX", statistic_label, ignore.case = TRUE)          ~ "NOx",
            grepl("sulphur|SOX|SO2", statistic_label, ignore.case = TRUE)              ~ "SOx",
            grepl("volatile organic|NMV", statistic_label, ignore.case = TRUE)         ~ "NMVOC",
            grepl("\\bammonia\\b|NH3", statistic_label, ignore.case = TRUE)            ~ "NH3",
            grepl("carbon monoxide|\\bCO\\b", statistic_label, ignore.case = TRUE)     ~ "CO",
            TRUE ~ NA_character_
        )
    ) %>%
    filter(!is.na(pollutant)) %>%
    mutate(
        year = as.integer(year),
        nace_rev_2_sector = as.character(nace_rev_2_sector)
    )

# ---------- 2) Aggregate to sector-year totals ----------
sector_year <- emis_raw %>%
    group_by(year, nace_rev_2_sector, pollutant) %>%
    summarise(Pollutant_Tonnes = sum(value, na.rm = TRUE), .groups = "drop") %>%
    # also compute sector-year TOTAL over pollutants
    group_by(year, nace_rev_2_sector) %>%
    mutate(Sector_Total = sum(Pollutant_Tonnes, na.rm = TRUE)) %>%
    ungroup()

# Quick sanity
cat("Years covered:", paste(range(sector_year$year), collapse = " - "), "\n")
cat("Sectors:", length(unique(sector_year$nace_rev_2_sector)), "\n")
cat("Pollutants:", paste(sort(unique(sector_year$pollutant)), collapse = ", "), "\n")

# =========================================================
# PART A — Forecast to 2026: Which sector will emit the most?
# =========================================================

# Helper: robust ARIMA forecast per sector with fallback to linear trend
forecast_to_year <- function(df_sector, horizon_year = 2026) {
    df_sector <- df_sector %>% arrange(year)
    yrs <- df_sector$year
    vals <- df_sector$Sector_Total
    
    # Make annual ts
    ts_obj <- ts(vals, start = min(yrs), frequency = 1)
    
    # Forecast horizon
    h <- horizon_year - max(yrs)
    if (h <= 0) {
        return(tibble(Year = max(yrs), Forecast = tail(vals, 1)))
    }
    
    # Try ARIMA, fallback to simple linear model if needed
    fc_vals <- NA
    fit_class <- NA
    aic_val <- NA
    
    try_arima <- try({
        fit <- forecast::auto.arima(ts_obj)
        aic_val <- as.numeric(fit$aic)
        fc <- forecast::forecast(fit, h = h)
        fc_vals <- as.numeric(fc$mean)
        fit_class <- "ARIMA"
    }, silent = TRUE)
    
    if (inherits(try_arima, "try-error") || any(is.na(fc_vals))) {
        # Fallback: linear trend via lm(Sector_Total ~ year)
        m <- lm(Sector_Total ~ year, data = df_sector)
        newdf <- tibble(year = seq.int(max(yrs) + 1, horizon_year))
        fc_vals <- predict(m, newdata = newdf)
        fit_class <- "LinearTrend"
    }
    
    tibble(Year = seq.int(max(yrs) + 1, horizon_year),
           Forecast = as.numeric(fc_vals),
           Fit = fit_class)
}

# Compute 2026 forecast for each sector
fc_2026 <- sector_year %>%
    group_by(nace_rev_2_sector) %>%
    reframe(forecast_to_year(cur_data(), horizon_year = 2026)) %>%
    filter(Year == 2026)

top_sectors_2026 <- fc_2026 %>%
    arrange(desc(Forecast)) %>%
    mutate(Rank_2026 = row_number())

cat("\n=== Top sectors by forecast TOTAL emissions in 2026 ===\n")
print(top_sectors_2026 %>% dplyr::select(Rank_2026, nace_rev_2_sector, Forecast, Fit) %>% head(10))

# Optional plot: Top 5 forecast trajectories
top5_names <- top_sectors_2026 %>% slice_head(n = 5) %>% pull(nace_rev_2_sector)

plot_df <- sector_year %>%
    filter(nace_rev_2_sector %in% top5_names) %>%
    dplyr::select(year, nace_rev_2_sector, Sector_Total) %>%
    distinct() %>%
    group_by(nace_rev_2_sector) %>%
    do({
        hist <- .
        fc <- forecast_to_year(sector_year %>% filter(nace_rev_2_sector == unique(hist$nace_rev_2_sector)),
                               horizon_year = 2026)
        bind_rows(
            hist %>% rename(Year = year, Value = Sector_Total) %>% mutate(Type = "Observed"),
            fc %>% rename(Value = Forecast) %>% mutate(Type = "Forecast")
        )
    }) %>% ungroup()

ggplot(plot_df, aes(Year, Value, color = Type)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    facet_wrap(~ nace_rev_2_sector, scales = "free_y") +
    scale_y_continuous(labels = scales::comma) +
    labs(title = "Sector emissions: observed & forecast to 2026",
         y = "Tonnes", x = NULL, color = NULL) +
    theme_minimal(base_size = 12)

# =========================================================
# PART B — Primary & secondary pollutants to regulate per sector (recent mix)
# =========================================================

# Use the most recent available year per sector to rank pollutants
recent_year <- max(sector_year$year, na.rm = TRUE)

mix_recent <- sector_year %>%
    filter(year == recent_year) %>%
    group_by(nace_rev_2_sector) %>%
    mutate(Share = Pollutant_Tonnes / sum(Pollutant_Tonnes, na.rm = TRUE)) %>%
    arrange(nace_rev_2_sector, desc(Share)) %>%
    summarise(
        Primary_Pollutant   = first(pollutant),
        Primary_Share       = first(Share),
        Secondary_Pollutant = nth(pollutant, 2),
        Secondary_Share     = nth(Share, 2),
        .groups = "drop"
    ) %>%
    arrange(desc(Primary_Share))

cat("\n=== Priority pollutants by sector (most recent year =", recent_year, ") ===\n")
print(mix_recent %>%
          mutate(across(ends_with("Share"), ~ scales::percent(., accuracy = 0.1))))

# =========================================================
# PART C — Feature importance: which pollutants drive sector totals?
# (Random Forest + LASSO across all sectors/years)
# =========================================================

# --- Step 1: Wide table (pollutants as columns) ---
wide_poll <- emis_raw %>%
    group_by(year, nace_rev_2_sector, pollutant) %>%
    summarise(Tonnes = sum(value), .groups = "drop") %>%
    tidyr::pivot_wider(
        names_from = pollutant,
        values_from = Tonnes,
        values_fill = 0
    )

# --- Step 2: Add sector total (sum across pollutants) ---
poll_list <- intersect(c("PM25","PM10","NOx","SOx","NMVOC","NH3","CO"), names(wide_poll))
wide_poll <- wide_poll %>%
    mutate(Sector_Total = rowSums(across(all_of(poll_list)), na.rm = TRUE))

# --- Step 3: Prep data for ML ---
rf_df <- wide_poll %>%
    dplyr::select(-year, -nace_rev_2_sector)

# Keep only pollutant columns actually present
poll_cols <- intersect(c("PM25","PM10","NOx","SOx","NMVOC","NH3","CO"), names(rf_df))
stopifnot(length(poll_cols) > 0)

# =========================================================
# Random Forest
# =========================================================
set.seed(123)
rf_fit <- randomForest(
    x = as.matrix(rf_df[, poll_cols, drop = FALSE]),
    y = rf_df$Sector_Total,
    ntree = 1000,
    importance = TRUE
)

cat("\nRandom Forest %Var explained:",
    round(rf_fit$rsq[length(rf_fit$rsq)]*100, 1), "%\n")

cat("\nRandom Forest importance:\n")
print(importance(rf_fit))

# =========================================================
# LASSO Regression (glmnet)
# =========================================================
x_mat <- as.matrix(rf_df[, poll_cols, drop = FALSE])
y_vec <- rf_df$Sector_Total

set.seed(123)
cvfit <- cv.glmnet(x_mat, y_vec, alpha = 1, nfolds = 5, standardize = TRUE)

# Coefficients at lambda.min and lambda.1se
lasso_min  <- coef(cvfit, s = "lambda.min")
lasso_1se  <- coef(cvfit, s = "lambda.1se")

cat("\nLASSO coefficients (lambda.min):\n")
print(lasso_min)

cat("\nLASSO coefficients (lambda.1se):\n")
print(lasso_1se)

# Non-zero predictors = those selected by LASSO
nz_min <- rownames(lasso_min)[as.numeric(lasso_min) != 0]
cat("\nNon-zero predictors at lambda.min:\n")
print(nz_min)

nz_1se <- rownames(lasso_1se)[as.numeric(lasso_1se) != 0]
cat("\nNon-zero predictors at lambda.1se:\n")
print(nz_1se)


# =========================================================

# =========================================================
# PART D — Validation & Diagnostics
# =========================================================

cat("\n--- VALIDATION CHECKS ---\n")

# 1) Data sanity
cat("Sector totals by year (top 5):\n")
print(
    sector_year %>%
        group_by(year) %>%
        summarise(National_Total = sum(Pollutant_Tonnes), .groups = "drop") %>%
        arrange(desc(year)) %>%
        head(5)
)

# 2) Compare ARIMA vs linear trend for a top sector
top_one <- top_sectors_2026 %>% slice(1) %>% pull(nace_rev_2_sector)
cat("\nTop sector by 2026 forecast:", top_one, "\n")

df_top <- sector_year %>%
    filter(nace_rev_2_sector == top_one) %>%
    arrange(year)

ts_top <- ts(df_top$Sector_Total, start = min(df_top$year), frequency = 1)

fit_arima <- tryCatch(auto.arima(ts_top), error = function(e) NULL)
fit_lm    <- lm(Sector_Total ~ year, data = df_top)

cat("AIC ARIMA:", ifelse(is.null(fit_arima), NA, fit_arima$aic),
    "| AIC Linear:", AIC(fit_lm), "\n")

# Plot ARIMA vs Linear fitted values
df_plot_fit <- df_top %>%
    mutate(
        Fitted_ARIMA = if (!is.null(fit_arima)) as.numeric(fitted(fit_arima)) else NA_real_,
        Fitted_LM    = as.numeric(fitted(fit_lm))
    )

ggplot(df_plot_fit, aes(year, Sector_Total)) +
    geom_point() + geom_line() +
    geom_line(aes(y = Fitted_LM), linetype = 2) +
    geom_line(aes(y = Fitted_ARIMA), linetype = 3, na.rm = TRUE) +
    labs(title = paste("Diagnostics:", top_one),
         subtitle = "Observed vs fitted (Linear dashed; ARIMA dotted)",
         x = NULL, y = "Tonnes") +
    theme_minimal(base_size = 12)

# 3) Importance stability: re-fit RF with different seeds and compare ranks
get_rf_rank <- function(seed=1) {
    set.seed(seed)
    rf <- randomForest(x = x_mat, y = y_vec, ntree = 800, importance = TRUE)
    imp <- importance(rf)[,1]
    tibble(pollutant = names(imp), IncMSE = as.numeric(imp)) %>%
        arrange(desc(IncMSE)) %>%
        mutate(rank = row_number(), seed = seed)
}
rf_ranks <- map_dfr(1:5, get_rf_rank)
cat("\nRF importance rank stability (first few rows):\n")
print(
    rf_ranks %>%
        arrange(pollutant, rank) %>%
        group_by(pollutant) %>%
        summarise(mean_rank = mean(rank), sd_rank = sd(rank), .groups="drop")
)

# 4) Do top pollutants by RF align with LASSO non-zero betas?
nz_lasso_min <- rownames(lasso_min)[as.numeric(lasso_min) != 0]
nz_lasso_1se <- rownames(lasso_1se)[as.numeric(lasso_1se) != 0]

cat("\nNon-zero LASSO predictors at lambda.min:", paste(nz_lasso_min, collapse = ", "), "\n")
cat("Non-zero LASSO predictors at lambda.1se:", paste(nz_lasso_1se, collapse = ", "), "\n")

rf_importance <- importance(rf_fit)
rf_top <- rownames(rf_importance)[order(rf_importance[,1], decreasing = TRUE)]

overlap_min <- intersect(rf_top, nz_lasso_min)
overlap_1se <- intersect(rf_top, nz_lasso_1se)

cat("\nOverlap RF vs LASSO (lambda.min):", paste(overlap_min, collapse = ", "), "\n")
cat("Overlap RF vs LASSO (lambda.1se):", paste(overlap_1se, collapse = ", "), "\n")

# 5) Sensitivity: rank sectors for 2025 vs 2026 consistency
fc_2025 <- sector_year %>%
    group_by(nace_rev_2_sector) %>%
    reframe(forecast_to_year(cur_data(), horizon_year = 2025)) %>%
    filter(Year == 2025) %>%
    arrange(desc(Forecast)) %>%
    mutate(Rank_2025 = row_number()) %>%
    dplyr::select(nace_rev_2_sector, Rank_2025)

rank_compare <- top_sectors_2026 %>%
    dplyr::select(nace_rev_2_sector, Rank_2026) %>%
    left_join(fc_2025, by = "nace_rev_2_sector")

cat("\nRank stability (2025 vs 2026):\n")
print(rank_compare %>% arrange(Rank_2026) %>% head(10))

# =========================================================
# PART E — Deliverables you can lift into report
# =========================================================
cat("\n=== REPORT TABLES YOU CAN EXPORT ===\n")
cat("- Top sectors 2026:\n")
print(top_sectors_2026 %>% arrange(Rank_2026) %>% mutate(Forecast = scales::comma(Forecast)))

cat("\n- Priority pollutants by sector (", recent_year, "):\n", sep = "")
print(mix_recent %>% mutate(across(ends_with("Share"), ~ scales::percent(., 0.1))))

cat("\n- RF importance (IncMSE) & LASSO coefficients shown above.\n")


# ============================
# Benchmarks + Threshold Flags (HOLD-OUT & FIXED)
# ============================

suppressPackageStartupMessages({
    library(dplyr)   # fully qualify dplyr:: throughout
    library(tidyr)
    library(tibble)
    library(purrr)
})

# --- Accuracy helpers ---
rmse <- function(actual, pred) sqrt(mean((actual - pred)^2, na.rm = TRUE))
mape <- function(actual, pred) 100 * mean(abs((actual - pred)/actual), na.rm = TRUE)

# --- Hold-out splitter (same rule you used: 2 years if n>=10 else 1) ---
holdout_h <- function(n_years) if (n_years >= 10) 2L else 1L

# --- Benchmarks computed on the SAME HOLD-OUT as your models ---
# df must contain: time_col (default "year") and outcome_col (e.g., "national_sum","county_sum")
benchmark_models <- function(df, outcome_col, time_col = "year") {
    stopifnot(all(c(outcome_col, time_col) %in% names(df)))
    df <- df %>% dplyr::arrange(.data[[time_col]]) %>% dplyr::distinct(.data[[time_col]], .keep_all = TRUE)
    
    y <- as.numeric(df[[outcome_col]])
    t <- as.numeric(df[[time_col]])
    n <- length(y); if (n < 3) stop("Not enough points for benchmarks")
    
    h <- holdout_h(n)
    cut <- max(t) - h
    idx_tr <- which(t <= cut)
    idx_te <- which(t >  cut)
    
    y_tr <- y[idx_tr]; t_tr <- t[idx_tr]
    y_te <- y[idx_te]; t_te <- t[idx_te]
    
    # Mean benchmark (train mean applied to test)
    mean_pred  <- rep(mean(y_tr, na.rm = TRUE), length(y_te))
    
    # Naive benchmark (last train value for all horizons)
    last_tr    <- tail(y_tr, 1)
    naive_pred <- rep(last_tr, length(y_te))
    
    # Linear trend on train, predict test
    lm_fit     <- lm(y_tr ~ t_tr)
    trend_pred <- as.numeric(predict(lm_fit, newdata = data.frame(t_tr = t_te)))
    r2_trend   <- summary(lm_fit)$r.squared
    
    tibble::tibble(
        Model = c("Mean", "Naive", "LinearTrend"),
        RMSE  = c(rmse(y_te, mean_pred), rmse(y_te, naive_pred), rmse(y_te, trend_pred)),
        MAPE  = c(mape(y_te, mean_pred), mape(y_te, naive_pred), mape(y_te, trend_pred)),
        R2    = c(NA_real_, NA_real_, r2_trend)
    )
}

# --- Harmonise your existing model results into (Model, RMSE, MAPE, R2) ---
as_results_table <- function(tbl, model_col = "model", rmse_col = "rmse", mape_col = "mape", r2_col = NULL) {
    stopifnot(all(c(model_col, rmse_col, mape_col) %in% names(tbl)))
    tibble::tibble(
        Model = as.character(tbl[[model_col]]),
        RMSE  = as.numeric(tbl[[rmse_col]]),
        MAPE  = as.numeric(tbl[[mape_col]]),
        R2    = if (!is.null(r2_col) && r2_col %in% names(tbl)) as.numeric(tbl[[r2_col]]) else NA_real_
    )
}

# --- Compute ΔRMSE vs Naive robustly (no vector recycling) + thresholds ---
compute_improvement_and_flags <- function(tbl) {
    # Expect exactly one Naive row
    naive_rmse <- tbl$RMSE[tbl$Model == "Naive"][1]
    if (!is.finite(naive_rmse)) {
        tbl$RMSE_impr_vs_Naive_pct <- NA_real_
    } else {
        tbl$RMSE_impr_vs_Naive_pct <- 100 * (naive_rmse - tbl$RMSE) / naive_rmse
    }
    tbl$PASS_R2    <- ifelse(is.na(tbl$R2), NA, tbl$R2 > 0.65)
    tbl$PASS_MAPE  <- ifelse(is.na(tbl$MAPE), NA, tbl$MAPE < 12)
    tbl$PASS_RMSEI <- ifelse(is.na(tbl$RMSE_impr_vs_Naive_pct), NA, tbl$RMSE_impr_vs_Naive_pct >= 10)
    tbl
}

# ============================
# NATIONAL (by sex)
# needs: resp_nat (year, sex, national_sum)
# optional: results_nat (sex, model, rmse, mape, pred_cases_2026)
# ============================

bench_nat <- resp_nat %>%
    dplyr::group_split(sex) %>%
    purrr::map_df(function(df_sex) {
        benchmark_models(df_sex, outcome_col = "national_sum", time_col = "year") %>%
            dplyr::mutate(sex = unique(df_sex$sex))
    })

if (exists("results_nat")) {
    nat_models <- results_nat %>%
        dplyr::group_split(sex) %>%
        purrr::map_df(function(dfm) {
            as_results_table(dfm, model_col = "model", rmse_col = "rmse", mape_col = "mape") %>%
                dplyr::mutate(sex = unique(dfm$sex))
        })
    
    nat_combined <- dplyr::bind_rows(
        bench_nat %>% dplyr::select(sex, Model, RMSE, MAPE, R2),
        nat_models %>% dplyr::select(sex, Model, RMSE, MAPE, R2)
    ) %>%
        dplyr::arrange(sex, match(Model, c("Mean","Naive","LinearTrend","Linear","ARIMAX","RandomForest")))
    
    nat_eval <- nat_combined %>%
        dplyr::group_by(sex) %>%
        dplyr::group_modify(~ compute_improvement_and_flags(.x)) %>%
        dplyr::ungroup()
} else {
    nat_eval <- bench_nat %>%
        dplyr::group_by(sex) %>%
        dplyr::group_modify(~ compute_improvement_and_flags(.x)) %>%
        dplyr::ungroup()
}

print(nat_eval)

# ============================
# COUNTY (area × sex)
# needs: resp_county (year, area, sex, county_sum)
# optional: results_county
# ============================

bench_county <- resp_county %>%
    dplyr::group_by(area, sex) %>%
    dplyr::group_modify(~ benchmark_models(.x, outcome_col = "county_sum", time_col = "year")) %>%
    dplyr::ungroup()

if (exists("results_county")) {
    cty_models <- results_county %>%
        dplyr::group_by(area, sex) %>%
        dplyr::group_modify(~ as_results_table(.x, model_col = "model", rmse_col = "rmse", mape_col = "mape")) %>%
        dplyr::ungroup()
    
    cty_combined <- dplyr::bind_rows(
        bench_county %>% dplyr::select(area, sex, Model, RMSE, MAPE, R2),
        cty_models   %>% dplyr::select(area, sex, Model, RMSE, MAPE, R2)
    ) %>%
        dplyr::arrange(area, sex, match(Model, c("Mean","Naive","LinearTrend","Linear","ARIMAX","RandomForest")))
    
    cty_eval <- cty_combined %>%
        dplyr::group_by(area, sex) %>%
        dplyr::group_modify(~ compute_improvement_and_flags(.x)) %>%
        dplyr::ungroup()
} else {
    cty_eval <- bench_county %>%
        dplyr::group_by(area, sex) %>%
        dplyr::group_modify(~ compute_improvement_and_flags(.x)) %>%
        dplyr::ungroup()
}

# ============================
# Compact national table (like Table 4.1)
# ============================

make_table_4_1 <- function(sex_label = "Female") {
    nat_eval %>%
        dplyr::filter(sex == sex_label) %>%
        dplyr::mutate(
            RMSE  = round(RMSE, 1),
            MAPE  = round(MAPE, 1),
            R2    = ifelse(is.na(R2), NA, round(R2, 3)),
            RMSE_impr_vs_Naive_pct = round(RMSE_impr_vs_Naive_pct, 1)
        ) %>%
        dplyr::select(Model, RMSE, MAPE, R2, RMSE_impr_vs_Naive_pct, PASS_R2, PASS_MAPE, PASS_RMSEI)
}

table_female <- make_table_4_1("Female"); print(table_female)
table_male   <- make_table_4_1("Male");   print(table_male)

# ============================
# Pretty summaries with cat()
# ============================

print_threshold_summary <- function(eval_tbl, level = c("sex","area/sex")) {
    level <- match.arg(level)
    if (level == "sex") {
        groups <- unique(eval_tbl$sex)
        cat("--------- Model Evaluation vs Thresholds (National) ---------\n")
        cat("Thresholds: R^2 > 0.65 | MAPE < 12% | RMSE improvement vs Naive ≥ 10%\n\n")
        for (g in groups) {
            sub <- eval_tbl %>% dplyr::filter(sex == g)
            naive_rmse <- sub$RMSE[sub$Model == "Naive"][1]
            cat(sprintf("[%s]\n", g))
            cat(sprintf(" Naive RMSE: %s\n", ifelse(is.finite(naive_rmse), format(round(naive_rmse,1), big.mark=","), "NA")))
            sub_fit <- sub %>% dplyr::filter(Model %in% c("Linear","ARIMAX","RandomForest"))
            for (i in seq_len(nrow(sub_fit))) {
                row <- sub_fit[i, ]
                cat(sprintf("  - %-12s | RMSE: %s | MAPE: %s%% | R2: %s | ΔRMSE vs Naive: %s%% | Pass(R2, MAPE, ΔRMSE): (%s, %s, %s)\n",
                            row$Model,
                            format(round(row$RMSE,1), big.mark=","),
                            ifelse(is.na(row$MAPE), "NA", round(row$MAPE,1)),
                            ifelse(is.na(row$R2), "NA", round(row$R2,3)),
                            ifelse(is.na(row$RMSE_impr_vs_Naive_pct), "NA", round(row$RMSE_impr_vs_Naive_pct,1)),
                            ifelse(is.na(row$PASS_R2), "NA", ifelse(row$PASS_R2,"✓","✗")),
                            ifelse(is.na(row$PASS_MAPE), "NA", ifelse(row$PASS_MAPE,"✓","✗")),
                            ifelse(is.na(row$PASS_RMSEI), "NA", ifelse(row$PASS_RMSEI,"✓","✗"))
                ))
                cat("\n")
            }
            cat("\n")
        }
    } else {
        cat("--------- Model Evaluation vs Thresholds (County) ---------\n")
        cat("Thresholds: R^2 > 0.65 | MAPE < 12% | RMSE improvement vs Naive ≥ 10%\n\n")
        example <- eval_tbl %>%
            dplyr::filter(Model %in% c("Linear","ARIMAX","RandomForest")) %>%
            dplyr::group_by(area, sex) %>%
            dplyr::slice_head(n = 3) %>%
            dplyr::ungroup() %>%
            dplyr::slice_head(n = 15)
        for (i in seq_len(nrow(example))) {
            row <- example[i, ]
            cat(sprintf("[%s | %s] %-12s | RMSE: %s | MAPE: %s%% | ΔRMSE vs Naive: %s%% | Pass(MAPE, ΔRMSE): (%s, %s)\n",
                        row$area, row$sex, row$Model,
                        format(round(row$RMSE,1), big.mark=","),
                        ifelse(is.na(row$MAPE), "NA", round(row$MAPE,1)),
                        ifelse(is.na(row$RMSE_impr_vs_Naive_pct), "NA", round(row$RMSE_impr_vs_Naive_pct,1)),
                        ifelse(is.na(row$PASS_MAPE), "NA", ifelse(row$PASS_MAPE,"✓","✗")),
                        ifelse(is.na(row$PASS_RMSEI), "NA", ifelse(row$PASS_RMSEI,"✓","✗"))
            ))
            cat("\n")
        }
        cat("\n(Above is a preview; use `cty_eval` to inspect all counties.)\n")
    }
}

# Print summaries
print_threshold_summary(nat_eval, level = "sex")
print_threshold_summary(cty_eval, level = "area/sex")




#------------------------ End of the code ------------------------------------------------