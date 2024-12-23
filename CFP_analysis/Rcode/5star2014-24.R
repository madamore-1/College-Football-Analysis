library(ggplot2)
library(dplyr)

# Load your data - assuming it's stored in a CSV file
data <- read.csv("5star20142024.csv")

# Aggregate data by year and conference
annual_conference_recruits <- data %>%
  group_by(Year, Conference) %>%
  summarise(Total_5_Stars = sum(Five_Stars), .groups = 'drop')

# View the aggregated data
print(annual_conference_recruits)


# Plotting the line graph for each conference
library(ggplot2)
library(dplyr)


ggplot(data = annual_conference_recruits, aes(x = Year, y = Total_5_Stars, color = Conference)) +
  geom_line(size=5) +  # Increased line thickness
  geom_point(size=5, shape=21, fill="white") +  # Adds points for each data point
  scale_x_continuous(breaks = seq(2014, 2024, 1)) +  # Ensures all years are included
  labs(title = "Number of 5-Star Recruits Per Power 5 Conference Per Year (2014-2024)",
       x = "Year",
       y = "Total Number of 5-Star Recruits") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),  
        legend.title = element_text(size = 16),  
        legend.text = element_text(size = 14),  
        legend.key.size = unit(1.5, "lines")) +  
  scale_color_manual(values = c("SEC" = "#FBCE28", "ACC" = "#013CA6", 
                                "Big Ten" = "#0088CE", "Pac-12" = "#092346", 
                                "Big 12" = "#C41230", "Independent" = "gray"))  # Specific colors for each conference
# Applies specific colors to each conference
