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

## i like these ones enough 

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
  scale_y_continuous(limits = c(0, 1)) +  # Set y-axis limit to 1
  scale_fill_manual(values = scales::hue_pal()(length(service_labels)), labels = service_labels) +  # Change legend names and colors
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    strip.text = element_text(size = 10),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey90"),  # Lighten the major grid lines
    panel.grid.minor = element_line(color = "grey90"),  # Lighten the minor grid lines
    panel.grid.major.x = element_blank(),  # Remove major vertical grid lines
    panel.grid.minor.x = element_blank(),  # Remove minor vertical grid lines
    panel.spacing = unit(1, "lines")
  ) +
  facet_wrap(~ Service, scales = "fixed", nrow = 4, labeller = labeller(Service = service_labels))

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
  scale_y_continuous(limits = c(0, 1)) +  # Set y-axis limit to 1
  scale_fill_manual(values = scales::hue_pal()(length(service_labels)), labels = service_labels) +  # Change legend names and colors
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    strip.text = element_text(size = 10),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),  # Remove major vertical grid lines ???
    panel.grid.minor.x = element_blank(),  # Remove minor vertical grid lines ??
    panel.spacing = unit(1, "lines")
  ) +
  facet_wrap(~ Service, scales = "fixed", nrow = 4, labeller = labeller(Service = service_labels))

facet_density_plot1
facet_density_plot2



####### FOR PLOT 2 #######

# Select relevant columns including travel class and service ratings
service_ratings_plot2 <- airline |> select(
  class,
  seat_comfort,
  cleanliness,
  inflight_entertainment, 
  on_board_service
)

# Convert data to long format for ggplot2
service_ratings_plot2_long <- service_ratings_plot2 |>
  pivot_longer(cols = -class, names_to = "Service", values_to = "Rating")

# Create a named vector for better labels
service_labels_plot2 <- c(
  seat_comfort = "Seat Comfort",
  cleanliness = "Cleanliness",
  inflight_entertainment = "Inflight Entertainment", 
  class = " Travel Class", 
  on_board_service = "On Board Service"
)

# Update Service variable in service_ratings_plot2_long with better labels
service_ratings_plot2_long <- service_ratings_plot2_long |>
  mutate(Service = factor(Service, levels = names(service_labels_plot2), labels = service_labels_plot2))
# Define a custom color palette
custom_colors <- c("Business" = "lightblue", "Eco" = "lightgreen", "Eco Plus" = "plum1")

boxplot_plot <- ggplot(service_ratings_plot2_long, aes(x = class, y = Rating, fill = class)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Boxplots of Service Ratings by Travel Class",
    subtitle = "Comparing Ratings for Different Service Features",
    x = "Travel Class",
    y = "Rating",
    fill = "Travel Class"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    strip.text = element_text(size = 10),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.position = "bottom"
  ) +
  facet_wrap(~ Service, scales = "free_y") + 
  scale_fill_manual(values = custom_colors)

# Create violin plots for each service category by travel class
violin_plot <- ggplot(service_ratings_plot2_long, aes(x = class, y = Rating, fill = class)) +
  geom_violin() +
  theme_minimal() +
  labs(
    title = "Violin Plots of Service Ratings by Travel Class",
    subtitle = "Comparing Ratings for Different Service Features",
    x = "Travel Class",
    y = "Rating",
    fill = "Travel Class"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    strip.text = element_text(size = 10),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.position = "bottom"
  ) +
  facet_wrap(~ Service, scales = "free_y") + 
  scale_fill_manual(values = custom_colors)


boxplot_plot
violin_plot

##### PLOT NUMBER 3 #######

# Select relevant columns for customer satisfaction and demographics
satisfaction_data <- airline |> select(
  satisfaction,
  type_of_travel,
  class,
  customer_type
)

# Create a stacked bar chart for each demographic variable

# Type of Travel
type_of_travel_plot <- ggplot(satisfaction_data, aes(x = type_of_travel, fill = satisfaction)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(
    title = "Customer Satisfaction by Type of Travel",
    x = "Type of Travel",
    y = "Proportion",
    fill = "Satisfaction"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

# Class
class_plot <- ggplot(satisfaction_data, aes(x = class, fill = satisfaction)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(
    title = "Customer Satisfaction by Class",
    x = "Class",
    y = "Proportion",
    fill = "Satisfaction"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

# Customer Type
customer_type_plot <- ggplot(satisfaction_data, aes(x = customer_type, fill = satisfaction)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(
    title = "Customer Satisfaction by Customer Type",
    x = "Customer Type",
    y = "Proportion",
    fill = "Satisfaction"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

# Display plots
print(type_of_travel_plot)
print(class_plot)
print(customer_type_plot)

satisfaction_count <- airline |>
  count(satisfaction)

# Create a bar plot for the total number of satisfied and unsatisfied customers
satisfaction_plot <- ggplot(satisfaction_count, aes(x = satisfaction, y = n, fill = satisfaction)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = n), vjust = 1.5, color = "white", size = 5) +
  scale_fill_manual(values = c("satisfied" = "#1b9e77", "neutral or dissatisfied" = "#d95f02")) +
  theme_minimal() +
  labs(
    title = "Total Number of Satisfied vs. Unsatisfied Customers",
    x = "Satisfaction",
    y = "Number of Customers",
    fill = "Satisfaction"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "none"
  )

satisfaction_plot


## same information as plot series 3 but in pie chart form 

# Type of Travel
# Aggregate the data
type_of_travel_agg <- satisfaction_data %>%
  count(type_of_travel, satisfaction) %>%
  group_by(type_of_travel) %>%
  mutate(prop = n / sum(n), 
         label = scales::percent(prop, accuracy = 0.1))

# Create pie chart
type_of_travel_pie <- ggplot(type_of_travel_agg, aes(x = "", y = prop, fill = satisfaction)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ type_of_travel) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), color = "white", size = 4) +
  theme_minimal() +
  labs(
    title = "Customer Satisfaction by Type of Travel",
    x = NULL,
    y = NULL,
    fill = "Satisfaction"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

# Display the plot
print(type_of_travel_pie)


# class
# Aggregate the data
class_agg <- satisfaction_data %>%
  count(class, satisfaction) %>%
  group_by(class) %>%
  mutate(prop = n / sum(n), 
         label = scales::percent(prop, accuracy = 0.1))

# Create pie chart
class_pie <- ggplot(class_agg, aes(x = "", y = prop, fill = satisfaction)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ class) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), color = "white", size = 4) +
  theme_minimal() +
  labs(
    title = "Customer Satisfaction by Class",
    x = NULL,
    y = NULL,
    fill = "Satisfaction"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

# Display the plot
print(class_pie)


# Customer Type
# Aggregate the data

customer_type_agg <- satisfaction_data %>%
  count(customer_type, satisfaction) %>%
  group_by(customer_type) %>%
  mutate(prop = n / sum(n), 
         label = scales::percent(prop, accuracy = 0.1))

# Create pie chart
customer_type_pie <- ggplot(customer_type_agg, aes(x = "", y = prop, fill = satisfaction)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ customer_type) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), color = "white", size = 4) +
  theme_minimal() +
  labs(
    title = "Customer Satisfaction by Customer Type",
    x = NULL,
    y = NULL,
    fill = "Satisfaction"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

# Display the plot
print(customer_type_pie)
