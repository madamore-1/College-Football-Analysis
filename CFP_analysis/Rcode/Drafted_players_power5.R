# Load the necessary libraries
library(dplyr)
library(ggplot2)


draft_data <- read.csv("Draft_Data__Fully_Cleaned.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)



# Filter for years between 2014 and 2024
draft_data_filtered <- draft_data %>%
  filter(Year >= 2014 & Year <= 2024)

# Filter and summarize the number of drafted players for Power 5 conferences by year
power_5_conferences <- c("SEC", "ACC", "Big Ten", "Pac-12", "Big 12")
draft_summary_per_year <- draft_data_filtered %>%
  filter(Conf %in% power_5_conferences) %>%
  group_by(Year, Conf) %>%
  summarize(Number_of_Players = n(), .groups = 'drop')

# Create a grouped bar chart
ggplot(draft_summary_per_year, aes(x = as.factor(Year), y = Number_of_Players, fill = Conf)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of Drafted Players by Power 5 Conferences Per Year (2014-2024)", 
       x = "Year", 
       y = "Number of Players") +
  theme_minimal() +
  scale_fill_manual(values = c("SEC" = "#FBCE28", "ACC" = "#013CA6", "Big Ten" = "#0088CE", 
                               "Pac-12" = "#092346", "Big 12" = "#C41230"))


p1 <- ggplot(draft_summary, aes(x = reorder(Conf, -Number_of_Players), y = Number_of_Players, fill = Conf)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Drafted Players by Power 5 Conferences (2014-2024)", 
       x = "Conference", 
       y = "Number of Players") +
  theme_minimal() +
  scale_fill_manual(values = c("SEC" = "#FBCE28", "ACC" = "#013CA6", "Big Ten" = "#0088CE", 
                               "Pac-12" = "#092346", "Big 12" = "#C41230"))




# Filter for SEC schools only
sec_draft_data <- draft_data %>%
  filter(Conf == "SEC")

# Summarize the number of drafted players by school
sec_summary <- sec_draft_data %>%
  group_by(College) %>%
  summarize(Number_of_Players = n(), .groups = 'drop') %>%
  arrange(desc(Number_of_Players))

# Create a bar chart
p2 <- ggplot(sec_summary, aes(x = reorder(College, -Number_of_Players), y = Number_of_Players, fill = College)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Drafted Players by SEC Schools (2014-2024)", 
       x = "School", 
       y = "Number of Players") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_discrete(guide = "none")




# Filter for Big Ten schools only
big_ten_draft_data <- draft_data %>%
  filter(Conf == "Big Ten")

# Summarize the number of drafted players by school
big_ten_summary <- big_ten_draft_data %>%
  group_by(College) %>%
  summarize(Number_of_Players = n(), .groups = 'drop') %>%
  arrange(desc(Number_of_Players))

# Create a bar chart
p3 <- ggplot(big_ten_summary, aes(x = reorder(College, -Number_of_Players), y = Number_of_Players, fill = College)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Drafted Players by Big Ten Schools (2014-2024)", 
       x = "School", 
       y = "Number of Players") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_discrete(guide = "none")




# Summarize the number of drafted players by school
acc_summary <- acc_draft_data %>%
  group_by(College) %>%
  summarize(Number_of_Players = n(), .groups = 'drop') %>%
  arrange(desc(Number_of_Players))

# Create a bar chart
p4 <- ggplot(acc_summary, aes(x = reorder(College, -Number_of_Players), y = Number_of_Players, fill = College)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Drafted Players by ACC Schools (2014-2024)", 
       x = "School", 
       y = "Number of Players") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_discrete(guide = "none")



# Filter for Big 12 schools only
big_12_draft_data <- draft_data %>%
  filter(Conf == "Big 12")

# Summarize the number of drafted players by school
big_12_summary <- big_12_draft_data %>%
  group_by(College) %>%
  summarize(Number_of_Players = n(), .groups = 'drop') %>%
  arrange(desc(Number_of_Players))

# Create a bar chart
p5 <- ggplot(big_12_summary, aes(x = reorder(College, -Number_of_Players), y = Number_of_Players, fill = College)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Drafted Players by Big 12 Schools (2014-2024)", 
       x = "School", 
       y = "Number of Players") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_discrete(guide = "none")





pac12_draft_data <- draft_data %>%
  filter(Conf == "Pac-12")

# Summarize the number of drafted players by school
pac12_summary <- pac12_draft_data %>%
  group_by(College) %>%
  summarize(Number_of_Players = n(), .groups = 'drop') %>%
  arrange(desc(Number_of_Players))

# Create a bar chart
p6 <- ggplot(pac12_summary, aes(x = reorder(College, -Number_of_Players), y = Number_of_Players, fill = College)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Drafted Players by Pac-12 Schools (2014-2024)", 
       x = "School", 
       y = "Number of Players") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_discrete(guide = "none")



# Load the gridExtra package
library(gridExtra)

# Arrange the plots in a 3x2 grid
grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 3, ncol = 2)


