# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(car) # For ANOVA and post-hoc testing

# Load the dataset
data <- read.csv("CFBattendance.csv")

# Clean and preprocess the data
data$Current_Average_2024 <- as.numeric(gsub(",", "", data$X2024.Current.Average))
data$Average_2023 <- as.numeric(gsub(",", "", data$X2023.Average))
data$Five_Year_Average <- as.numeric(gsub(",", "", data$X5.Year.Average))
data$Conference <- trimws(data$Conference)

# Calculate average attendance by conference
conference_avg <- data %>%
  group_by(Conference) %>%
  summarize(Average_Attendance = mean(Current_Average_2024, na.rm = TRUE)) %>%
  arrange(desc(Average_Attendance))


avg_attendance_5yr <- data %>%
  group_by(Conference) %>%
  summarize(Average_Attendance_5yr = mean(Five_Year_Average, na.rm = TRUE)) %>%
  arrange(desc(Average_Attendance_5yr))


conference_colors <- c(
  "SEC" = "#FBCE28", 
  "ACC" = "#013CA6", 
  "Big Ten" = "#0088CE", 
  "Pac-12" = "#092346", 
  "Big 12" = "#C41230"
)

# Bar plot for average attendance by conference (2024)
B1 <- ggplot(conference_avg, aes(x = reorder(Conference, -Average_Attendance), y = Average_Attendance, fill = Conference)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = conference_colors) +
  theme_minimal() +
  labs(title = "Average Attendance by Conference (2024)",
       x = "Conference", y = "Average Attendance") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

# Bar plot for average attendance over the past 5 years by conference
B2 <- ggplot(avg_attendance_5yr, aes(x = reorder(Conference, -Average_Attendance_5yr), y = Average_Attendance_5yr, fill = Conference)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = conference_colors) +
  theme_minimal() +
  labs(title = "Average Attendance Over the Past 5 Years by Conference",
       x = "Conference",
       y = "Average Attendance (5-Year)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

# Perform one-way ANOVA
anova_model <- aov(Current_Average_2024 ~ Conference, data = data)
summary(anova_model)

# Post-hoc testing using Tukey's HSD
tukey_results <- TukeyHSD(anova_model)
print(tukey_results)

# Visualize Tukey's HSD results
# Create abbreviated conference labels
data$Conference <- abbreviate(data$Conference)

# Run Tukey's HSD 

# Increase left margin for y-axis labels
par(mar = c(5, 8, 4, 2))  # Bottom, Left, Top, Right margins
plot(tukey_results, las = 1)

# Filter the top 20 schools by attendance in 2024
top_10_attendance <- data %>%
  arrange(desc(Current_Average_2024)) %>%
  slice(1:15)

# Bar plot for the top 10 attendance
ggplot(top_10_attendance, aes(x = reorder(School, Current_Average_2024), y = Current_Average_2024, fill = Conference)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = conference_colors) +
  coord_flip() +  # Flips the plot to horizontal
  theme_minimal() +
  labs(title = "Top 15 Schools by Attendance (2024)",
       x = "Average Attendance",
       y = "School") +  # Adjust axis labels to match the flipped orientation
  theme(axis.text.y = element_text(size = 10), legend.position = "top")



grid.arrange(B1, B2
)


