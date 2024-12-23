# Load required libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(performance)

# Load datasets (replace with your file paths)

offense <- read.csv("offense24-14.csv")
defense <- read.csv("defense2024-2014.csv")

# Filter for Power 5 conferences
power_5 <- c("SEC", "Big Ten", "ACC", "Pac-12", "Big 12")
defense <- defense %>% filter(Conference %in% power_5)
offense <- offense %>% filter(Conference %in% power_5)

# Merge offense and defense datasets
data <- offense %>%
  inner_join(defense, by = c("School", "Conference")) %>%
  mutate(
    SEC_vs_Power4 = ifelse(Conference == "SEC", "SEC", "Power 4"),
    SEC_Binary = ifelse(SEC_vs_Power4 == "SEC", 1, 0)
  )

# Check merged data structure
print(head(data))

# T-Tests
# Points Scored (Offense)
ttest_scored <- t.test(Pts.x ~ SEC_vs_Power4, data = data)
print(ttest_scored)

# Points Allowed (Defense)
ttest_allowed <- t.test(Pts.y ~ SEC_vs_Power4, data = data)
print(ttest_allowed)

# Linear Models
# Offense and Defense Combined
lm_combined <- lm(Pts.x ~ Pts.y + Pass_yds + Rush_Yds.x + Rush_Yds.y + SEC_vs_Power4, data = data)
summary(lm_combined)

# Logistic Regression: Predict SEC vs. Power 4
glm_combined <- glm(SEC_Binary ~ Pts.x + Pts.y + Pass_yds + Rush_Yds.x + Rush_Yds.y + TO, data = data, family = "binomial")
summary(glm_combined)

# Visualizations
# Violin Plot: Points Scored (Offense)
D1 <- ggplot(data, aes(x = SEC_vs_Power4, y = Pts.x, fill = SEC_vs_Power4)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill = "white") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill = "yellow") +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0.2, color = "black") +
  labs(title = "Points Scored: SEC vs. Power 4", x = "Group", y = "Points Scored") +
  theme_minimal()

# Violin Plot: Points Allowed (Defense)
D2 <- ggplot(data, aes(x = SEC_vs_Power4, y = Pts.y, fill = SEC_vs_Power4)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill = "white") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill = "yellow") +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0.2, color = "black") +
  labs(title = "Points Allowed: SEC vs. Power 4", x = "Group", y = "Points Allowed") +
  theme_minimal()

# Scatterplot: Offense (Points Scored vs. Passing Yards)
ggplot(data, aes(x = Pass_yds, y = Pts.x, color = SEC_vs_Power4)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Points Scored vs. Passing Yards: SEC vs. Power 4", x = "Passing Yards", y = "Points Scored") +
  theme_minimal()

# Scatterplot: Defense (Points Allowed vs. Rushing Yards Allowed)
ggplot(data, aes(x = Rush_Yds.y, y = Pts.y, color = SEC_vs_Power4)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Points Allowed vs. Rushing Yards Allowed: SEC vs. Power 4", x = "Rushing Yards Allowed", y = "Points Allowed") +
  theme_minimal()


# Step 1: Load the Data
cfp_data <- read.csv("cfp_data.csv")

# Step 2: Assign Weights
cfp_data <- cfp_data %>%
  mutate(
    Champion_Points = 2,  # Champion earns 2 points
    Runner_Points = 1     # Runner-up earns 1 point
  )

# Step 3: Calculate Total Points by Conference
# Champion Points
champion_points <- cfp_data %>%
  group_by(Conference_Champion) %>%
  summarise(Champion_Total = sum(Champion_Points)) %>%
  rename(Conference = Conference_Champion)

# Runner-Up Points
runner_up_points <- cfp_data %>%
  group_by(Conferemce_Runner) %>%
  summarise(Runner_Total = sum(Runner_Points)) %>%
  rename(Conference = Conferemce_Runner)

# Merge Champion and Runner-Up Points
total_points <- full_join(champion_points, runner_up_points, by = "Conference") %>%
  mutate(
    Champion_Total = replace_na(Champion_Total, 0),
    Runner_Total = replace_na(Runner_Total, 0),
    Total_Points = Champion_Total + Runner_Total
  )

# Step 4: Highlight SEC Performance
# Filter SEC for comparison
sec_points <- total_points %>% filter(Conference == "SEC")

# Compare SEC with Other Conferences
comparison <- total_points %>%
  mutate(
    SEC_vs_Others = ifelse(Conference == "SEC", "SEC", "Other Conferences")
  ) %>%
  group_by(SEC_vs_Others) %>%
  summarise(
    Total_Points = sum(Total_Points),
    Champion_Wins = sum(Champion_Total),
    Runner_Ups = sum(Runner_Total)
  )

# Step 5: Visualization
# Bar Plot for Total Points by Conference
z1 <- ggplot(total_points, aes(x = reorder(Conference, Total_Points), y = Total_Points, fill = Conference)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Total Points by Conference (Weighted: Champion=2, Runner-Up=1)",
    x = "Conference",
    y = "Total Points"
  ) +
  theme_minimal()

# Pie Chart: SEC vs. Other Conferences
z2 <- ggplot(comparison, aes(x = "", y = Total_Points, fill = SEC_vs_Others)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(
    title = "SEC Dominance: Total Points vs. Other Conferences",
    fill = "Category"
  ) +
  theme_void()

# Step 6: Print SEC Metrics for Further Claims
print("SEC Metrics:")
print(sec_points)

print("Comparison with Other Conferences:")
print(comparison)


grid.arrange(D1, D2)
grid.arrange(z1, z2)



