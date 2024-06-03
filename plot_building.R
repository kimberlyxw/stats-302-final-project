# Load necessary libraries
library(ggplot2)
library(ggridges)
library(dplyr)
library(tidyr)

# Read the dataset
airline <- read.csv("data/Airline_customer_satisfaction.csv") |> 
  janitor::clean_names()



####### FOR PLOT 1 (but multiple graphs) #######

# Select relevant columns for service satisfaction ratings
service_ratings <- airline |> select(
  seat_comfort,
  departure_arrival_time_convenient,
  food_and_drink,
  gate_location,
  inflight_wifi_service,
  inflight_entertainment,
  online_support,
  ease_of_online_booking,
  on_board_service,
  leg_room_service,
  baggage_handling,
  checkin_service,
  cleanliness,
  online_boarding
)

# Convert data to long format for ggplot2
service_ratings_long <- service_ratings %>%
  pivot_longer(cols = everything(), names_to = "Service", values_to = "Rating")

service_labels <- c(
  seat_comfort = "Seat Comfort",
  departure_arrival_time_convenient = "Departure/Arrival Time Convenient",
  food_and_drink = "Food and Drink",
  gate_location = "Gate Location",
  inflight_wifi_service = "Inflight WiFi Service",
  inflight_entertainment = "Inflight Entertainment",
  online_support = "Online Support",
  ease_of_online_booking = "Ease of Online Booking",
  on_board_service = "On-Board Service",
  leg_room_service = "Leg Room Service",
  baggage_handling = "Baggage Handling",
  checkin_service = "Checkin Service",
  cleanliness = "Cleanliness",
  online_boarding = "Online Boarding"
)

# Density overlap plot
density_plot <- ggplot(service_ratings_long, aes(x = Rating, fill = Service)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  scale_fill_manual(values = scales::hue_pal()(length(service_labels)), labels = service_labels) +
  labs(
    title = "Density Overlap Plot of Service Satisfaction Ratings",
    subtitle = "Distribution of Ratings for Different Service Features",
    x = "Rating",
    y = "Density",
    fill = "Service"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

density_plot

# Ridge plot
ridge_plot <- ggplot(service_ratings_long, aes(x = Rating, y = Service, fill = Service)) +
  geom_density_ridges(alpha = 0.5) +
  scale_y_discrete(labels = service_labels) +
  theme_minimal() +
  labs(
    title = "Ridge Plot of Service Satisfaction Ratings",
    subtitle = "Distribution of Ratings for Different Service Features",
    x = "Rating",
    y = "Service"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none"
  )

ridge_plot

# Faceted density plots for readability with updated labels
facet_density_plot <- ggplot(service_ratings_long, aes(x = Rating, fill = Service)) +
  geom_density(alpha = 0.7) +
  theme_minimal() +
  scale_fill_manual(values = scales::hue_pal()(length(service_labels)), labels = service_labels) +
  labs(
    title = "Density Plots of Service Satisfaction Ratings",
    subtitle = "Distribution of Ratings for Different Service Features",
    x = "Rating",
    y = "Density",
    fill = "Service"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  facet_wrap(~ Service, scales = "free_y", labeller = as_labeller(service_labels))

facet_density_plot

first_half <- service_ratings_long %>% filter(Service %in% names(service_labels)[1:7])
second_half <- service_ratings_long %>% filter(Service %in% names(service_labels)[8:14])

# Faceted density plots for the first half
facet_density_plot1 <- ggplot(first_half, aes(x = Rating, fill = Service)) +
  geom_density(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Density Plots of Service Satisfaction Ratings (Part 1)",
    subtitle = "Distribution of Ratings for Different Service Features",
    x = "Rating",
    y = "Density",
    fill = "Service"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    strip.text = element_text(size = 10),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10)
  ) +
  facet_wrap(~ Service, scales = "free_y", nrow = 4, labeller = labeller(Service = service_labels)) +
  theme(panel.spacing = unit(1, "lines"))

# Faceted density plots for the second half
facet_density_plot2 <- ggplot(second_half, aes(x = Rating, fill = Service)) +
  geom_density(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Density Plots of Service Satisfaction Ratings (Part 2)",
    subtitle = "Distribution of Ratings for Different Service Features",
    x = "Rating",
    y = "Density",
    fill = "Service"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    strip.text = element_text(size = 10),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10)
  ) +
  facet_wrap(~ Service, scales = "free_y", nrow = 4, labeller = labeller(Service = service_labels)) +
  theme(panel.spacing = unit(1, "lines"))

facet_density_plot1
facet_density_plot2
