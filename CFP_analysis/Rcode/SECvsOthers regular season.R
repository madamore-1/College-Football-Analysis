# Load necessary libraries
library(dplyr)
library(ggplot2)

# SECTION 1: Load datasets
SECvsACC <- read.csv("SECvsACC2014-2024.csv")
SECvsBig12 <- read.csv("SECvsBig1220142024.csv")
SECvsBig10 <- read.csv("SECvsBig1020142024.csv")
SECvsPAC12 <- read.csv("SECvsPAC1220142024.csv")

# SECTION 2: Define utility functions

# Function to calculate win-loss records
calculate_records <- function(data, sec_points_col, opp_points_col, location_col, sec_team_col, opp_team_col) {
  data %>%
    mutate(
      Winner = ifelse(
        !!sym(sec_points_col) > !!sym(opp_points_col), !!sym(sec_team_col),
        ifelse(!!sym(sec_points_col) < !!sym(opp_points_col), !!sym(opp_team_col), "Draw")
      )
    ) %>%
    group_by(!!sym(location_col)) %>%
    summarize(
      Wins = sum(Winner == !!sym(sec_team_col)),
      Losses = sum(Winner == !!sym(opp_team_col)),
      .groups = "drop"
    ) %>%
    mutate(Location = !!sym(location_col))
}

# SECTION 3: Calculate records for all datasets
records_acc <- calculate_records(SECvsACC, "SEC_points", "ACC_points", "Home.Away", "SEC", "ACC") %>%
  mutate(Conference = "ACC")
records_big12 <- calculate_records(SECvsBig12, "Points_SEC", "Points_Big12", "Home.Away", "SEC", "Big.12") %>%
  mutate(Conference = "Big 12")
records_big10 <- calculate_records(SECvsBig10, "Points_SEC", "Points_Big10", "Home.Away", "SEC", "Big_10") %>%
  mutate(Conference = "Big 10")
records_pac12 <- calculate_records(SECvsPAC12, "SEC_points", "Pac12_points", "Home.Away", "SEC", "PAC.12") %>%
  mutate(Conference = "PAC 12")

# Combine all records into a single dataframe
all_records <- bind_rows(records_acc, records_big12, records_big10, records_pac12)

# Add the win-loss record as a column
all_records <- all_records %>%
  mutate(
    Record = paste(Wins, "-", Losses),
    Win_Rate = Wins / (Wins + Losses)
  )

# SECTION 4: Visualize results


A1 <- ggplot(all_records, aes(x = Conference, y = Wins + Losses, fill = Location)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = Record), position = position_stack(vjust = 0.5), size = 4) +
  labs(
    title = "Total Games by Location Against Other Conferences (2014-2024)",
    x = "Conference",
    y = "Total Games",
    fill = "Location"
  ) +
  theme_minimal()


A2 <- ggplot(all_records, aes(x = Conference, y = Location, fill = Win_Rate)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Record), color = "black", size = 4) +
  labs(
    title = "SEC Win Rates Heatmap Against Other Conferences (2014-2024)",
    x = "Conference",
    y = "Location",
    fill = "Win Rate"
  ) +
  theme_minimal() +
  scale_fill_gradient(low = "orange", high = "green", labels = scales::percent)


library(gridExtra)
library(grid)
grid.arrange(A1, A2, nrow=1)
