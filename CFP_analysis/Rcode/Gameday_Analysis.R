library(ggplot2)
library(maps)
library(tidygeocoder)
library(dplyr)

# Load the gameday data
gameday_dat <- read.csv("gameday2024-2014.csv")

gameday_data <- gameday_dat %>%
  select(-Game)


# Geocode based on the `location` column
gameday_data <- gameday_data %>%
  mutate(address = Location) %>%  # Ensure the correct column name is used
  geocode(address = address, method = "osm", lat = latitude, long = longitude)


# Load US map data
us_map <- map_data("state")


# Add new columns for conditional styling
gameday_data <- gameday_data %>%
  mutate(
    MatchType = case_when(
      Winner.Conference == "SEC" & Loser.Conference != "SEC" ~ "SEC Win",
      Loser.Conference == "SEC" & Winner.Conference != "SEC" ~ "SEC Lose",
      TRUE ~ "Other"
    ),
    ShapeType = ifelse(MatchType %in% c("SEC Win", "SEC Lose"), "Triangle", "Circle")
  )

# Plot with customized colors and shapes
G1 <-  ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "grey90", color = "black") +
  geom_point(
    data = gameday_data,
    aes(
      x = longitude, 
      y = latitude, 
      color = MatchType, 
      shape = ifelse(MatchType == "Other", "Other vs Other", "SEC vs Other"), 
      alpha = MatchType
    ),
    size = 3
  ) +
  scale_color_manual(
    values = c("SEC Win" = "green", "SEC Lose" = "red", "Other" = "blue")
  ) +
  scale_shape_manual(
    values = c("SEC vs Other" = 17, "Other vs Other" = 16),  # 17 = triangle, 16 = circle
    name = "Game Type"
  ) +
  scale_alpha_manual(
    values = c("SEC Win" = 1, "SEC Lose" = 1, "Other" = 0.5),  # Translucent blue dots for "Other"
    name = "Match Type"
  ) +
  labs(
    title = "College GameDay Locations (2014-2024)",
    x = "Longitude",
    y = "Latitude",
    color = "Match Type"
  ) +
  theme_minimal()





# Calculate SEC wins vs other conferences
sec_wins <- gameday_data %>%
  filter(Winner.Conference == "SEC" & Loser.Conference != "SEC") %>%
  nrow()

# Calculate SEC losses vs other conferences
sec_losses_by_conference <- gameday_data %>%
  filter(Loser.Conference == "SEC" & Winner.Conference != "SEC") %>%
  group_by(Winner.Conference) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(desc(Count))

# Combine results into a dataframe for plotting
win_loss_data <- data.frame(
  Category = c("SEC Wins", "SEC Losses"),
  Count = c(sec_wins, sec_losses)
)

sec_results <- bind_rows(sec_wins, sec_losses)


G2 <-  ggplot(sec_results, aes(x = 2, y = Count, fill = Category)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = ifelse(
    Category == "SEC Wins",
    paste0("SEC Wins vs All: ", Count),
    paste0(Category, " Win vs SEC: ", Count)
  )), 
  position = position_stack(vjust = 0.5), size = 4) +
  theme_void() +  # Minimal theme
  labs(title = "SEC Wins and Losses by Conference (Gameday only 2014-2024)") +
  scale_fill_manual(values = c(
    "SEC Wins" = "#FBCE28", 
    "ACC" = "#013CA6", 
    "Big Ten" = "#0088CE", 
    "Pac-12" = "#092346", 
    "Big 12" = "#C41230",
    "Independent" = "gray"
  )) +
  xlim(.5, 2.5)  

library(gridExtra)
library(grid)

layout <- rbind(c(1, 1, 1, 2))  # G1 spans 3 parts, G2 spans 1 part

# Arrange plots with custom layout
grid.arrange(
  G1, G2,
  layout_matrix = layout
)

