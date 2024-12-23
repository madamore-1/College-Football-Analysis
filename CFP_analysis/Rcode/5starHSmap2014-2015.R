

install.packages("tidygeocoder")
install.packages("ggplot2")
install.packages("sf")
install.packages("dplyr")

library(tidygeocoder)
library(dplyr)
library(ggplot2)
library(maps)


recruit_data <- read.csv("5starHS20142024.csv")  

# Geocode 
recruit_data <- recruit_data %>%
  geocode(address = HomeTown, method = "osm", lat = latitude, long = longitude)

# Group by location conference
recruit_counts <- recruit_data %>%
  group_by(latitude, longitude, HomeTown) %>%
  summarise(RecruitCount = n()) %>%
  ungroup()

# Group by location conference
recruit_counts <- recruit_data %>%
  group_by(latitude, longitude, Conference) %>%
  summarise(RecruitCount = n(), .groups = 'drop')

# Load US map with states
us_map <- map_data("state")

  
# Plot recruit counts 
ggplot() +
  geom_polygon(data = us_map_adjusted, aes(x = long, y = lat, group = group), fill = "grey", color = "black") +
  geom_point(data = recruit_counts, aes(x = longitude, y = latitude, size = RecruitCount, color = Conference), alpha = 0.5) +
  scale_size_continuous(
    name = "Number of Recruits",
    breaks = seq(1, max(recruit_counts$RecruitCount), by = 2)  
  ) +
  scale_color_manual(
    values = c("SEC" = "#FBCE28", "ACC" = "#013CA6", 
                 "Big Ten" = "#0088CE", "Pac-12" = "#092346", 
               "Big 12" = "#C41230")
  ) +
  labs(
    title = "Recruiting Hotspots by Conference (2014-2024)",
    x = "Longitude",
    y = "Latitude",
    color = "Conference"
  ) +
  theme_minimal()

library(dplyr)

# Aggregate recruit counts by city
top_10_locations <- recruit_data %>%
  group_by(HomeTown) %>%  # Group by hometown (city)
  summarise(TotalRecruits = n(), .groups = 'drop') %>%  # Count total recruits for each city
  arrange(desc(TotalRecruits)) %>%  # Sort in descending order
  slice_head(n = 10)  # Select the top 5 cities

# View the result
print(top_10_locations)




library(dplyr)
library(stringr)

# Extract the state from the HomeTown column
recruit_data <- recruit_data %>%
  mutate(State = str_extract(HomeTown, ",\\s*[A-Z]{2}$") %>% str_remove(",\\s*"))  # Extract state abbreviation

# Aggregate recruit counts by state
top_10_states <- recruit_data %>%
  group_by(State) %>%  # Group by extracted state
  summarise(TotalRecruits = n(), .groups = 'drop') %>%  # Count total recruits for each state
  arrange(desc(TotalRecruits)) %>%  # Sort in descending order
  slice_head(n = 10)  # Select the top 5 states

# View the result
print(top_10_states)








