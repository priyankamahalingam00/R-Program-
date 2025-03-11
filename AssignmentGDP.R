library(ggplot2)
library(prophet)
library(reshape2)
library(viridis)

# GDP dataset file location
data <- read.csv("D:/User-Files/Priyanka Mahalingam/Documents/Sem -2/R Program/GDP data.csv")

# Disable scientific notation
options(scipen = 999)  
# Shows structure of data (data types)
str(data)   

# Count missing values per column
colSums(is.na(data))  

data$Year <- as.integer(data$Year)

# Find the latest year in the dataset
latest_year <- max(data$Year)

# Extract GDP data for the latest year 
latest_data <- data[data$Year == latest_year, -1]
print(latest_data)

# Find the country with the highest GDP
max_country <- names(latest_data)[which.max(latest_data)]
print(paste("Highest GDP in", latest_year, ":", max_country))

# Find the country with the lowest GDP
min_country <- names(latest_data)[which.min(latest_data)]
print(paste("Lowest GDP in", latest_year, ":", min_country))

# Find the top 5 countries with the highest GDP
top_countries <- names(sort(unlist(latest_data), decreasing = TRUE))[1:5]
print(paste("Top 5 countries with highest GDP in", latest_year, ":", paste(top_countries, collapse=", ")))

# Create a dataframe of top 5 countries with their GDP
top_5_gdp <- data.frame(Country = top_countries,
                        GDP = as.numeric(unlist(latest_data[top_countries])))
print(top_5_gdp)

# Visualize the Top 5 GDP Countries as a Bar Chart
ggplot(top_5_gdp, aes(x = reorder(Country, -GDP), y = GDP, fill = Country)) +
  geom_bar(stat = "identity", width = 0.6, show.legend = FALSE) +  
  geom_text(aes(label = scales::comma(GDP)), vjust = -0.4, size = 4) +  
  labs(title = paste("Top 5 GDP Countries in", latest_year),
       x = "Country", y = "GDP (in Trillions)") + scale_y_continuous(labels = scales::comma) + 
  theme_minimal()

  
# GDP growth of Germany Over Time using point graph
ggplot(data, aes(x = Year, y = Germany)) +
  geom_line(color = "blue", size = 1) +
  geom_smooth(method = "loess", color = "red", linetype = "dashed", se = FALSE) +
  labs(title = "GDP Growth of Germany Over Time",x = "Year",y = "GDP") +
  scale_x_continuous(breaks = seq(min(data$Year), max(data$Year), by = 5)) + scale_y_continuous(labels = scales::comma) + 
  theme_minimal()  

# Compare GDP of France, Germany, and UK in line chart
ggplot(data, aes(x=Year)) +
  geom_line(aes(y=France, color="France")) +
  geom_line(aes(y=Germany, color="Germany")) +
  geom_line(aes(y=United.Kingdom, color="UK")) +
  labs(title="GDP Trends of France, Germany, and UK", x="Year", y="GDP") + scale_y_continuous(labels = scales::comma) +
  theme_minimal()  

# Fletch Last 10 years data
start_year <- latest_year - 9  
last_10_years_data <- data[data$Year >= start_year, ]
print(last_10_years_data)

# Extract GDP data for the start and end year
gdp_start <- last_10_years_data[last_10_years_data$Year == start_year, -1]
gdp_latest <- last_10_years_data[last_10_years_data$Year == latest_year, -1]

# Calculate percentage growth ((New GDP - Old GDP) / Old GDP * 100)
gdp_growth <- ((gdp_latest - gdp_start) / gdp_start) * 100
gdp_growth_numeric <- as.numeric(unlist(gdp_growth))
names(gdp_growth_numeric) <- colnames(gdp_start)

# Find the country with the highest growth rate
fastest_growth_country <- names(which.max(gdp_growth_numeric))
fastest_growth_rate <- max(gdp_growth_numeric)

# Print result the country name with percentage
print(paste("The country with the fastest GDP growth in the last 10 years is", 
            fastest_growth_country, "with a growth rate of", 
            round(fastest_growth_rate, 2), "%"))

# Convert to a data frame for visualization
gdp_growth_df <- data.frame(Country = names(gdp_growth_numeric),
                            GrowthRate = gdp_growth_numeric)

# Sort data by highest growth
gdp_growth_df <- gdp_growth_df[order(-gdp_growth_df$GrowthRate), ]
#gdp_growth_df <- gdp_growth_df |> arrange(desc(GrowthRate))  


# The top 10 fastest-growing countries in bar chart
ggplot(gdp_growth_df[1:10, ], aes(x = reorder(Country, GrowthRate), y = GrowthRate, fill = Country)) +
  geom_bar(stat="identity", width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(GrowthRate, 1), "%")), 
            hjust = 1.2, size = 4, color = "black") +  # Position inside bars
  coord_flip() +
  labs(title="Top 10 Countries with Fastest GDP Growth in Last 10 Years",
       x="Country", y="GDP Growth Rate (%)") +
  theme_minimal()


# Future data prediction of Germany’s GDP look like in the next 10 years
gdp_data <- data.frame(ds = as.Date(paste(data$Year, "-01-01", sep="")), y = data$Germany)
model <- prophet(gdp_data, yearly.seasonality = TRUE)

# Create future data for prediction forecast till 2033
future <- make_future_dataframe(model, periods = 10, freq = "year")

# Create future data for prediction forecast till 2033 using prophate 
future <- make_future_dataframe(model, periods = 10, freq = "year")

# Predict future GDP
forecast <- predict(model, future)

# Filter only future predictions (from 2024 onward)
future_forecast <- forecast[forecast$ds > max(gdp_data$ds), ]

# Filter only future predictions (from 2024 onward)
future_forecast <- forecast[forecast$ds > max(as.Date(gdp_data$ds)), ]

# Future forecast of Germany from 2024 - 2033 
ggplot(future_forecast, aes(x = ds, y = yhat)) +
  geom_segment(aes(x = ds, xend = ds, y = 0, yend = yhat), 
               color = "blue", linetype = "dashed") +  
  geom_point(color = "red", size = 4) +  
  labs(title = "Germany GDP Forecast (2024 - 2033)",
       x = "Year", y = "Forecasted GDP") + scale_y_continuous(labels = scales::comma) +
  theme_minimal()


# Melt data for heatmap
melted_data <- melt(data, id.vars = "Year")

# Create Heatmap with Improved Colors
ggplot(melted_data, aes(x = Year, y = variable, fill = value)) +
  geom_tile(color = "black") +  # Light grid lines for clarity
  scale_fill_viridis(option = "G", direction = -1) +  
  labs(title = "Heatmap of GDP Trends", x = "Year", y = "Country", fill = "GDP Value") +
  theme_minimal()




