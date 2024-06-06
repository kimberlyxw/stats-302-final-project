# Load necessary libraries
library(ggplot2)
library(ggridges)
library(dplyr)
library(tidyr)
library(patchwork)

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

service_ratings_long <- service_ratings |> 
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

density_plot # not using density plot in final project, but keeping the code here 

# Ridge plot
ridge_plot <- ggplot(service_ratings_long, aes(x = Rating, y = Service, fill = Service)) +
  geom_density_ridges(alpha = 0.5) +
  scale_y_discrete(labels = service_labels) +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5)) + 
  labs(
    title = "Ridge Plot of Different Service Features Satisfaction Ratings",
    subtitle = "While it differs from service to service, the most common rating is a 4",
    x = "Rating",
    y = "Service"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20),  
    plot.subtitle = element_text(hjust = 0.5, size = 16),     
    axis.title.x = element_text(size = 15.5),                   
    axis.title.y = element_text(size = 15.5),                    
    axis.text.x = element_text(size = 13),                     
    axis.text.y = element_text(size = 13),                     
    legend.position = "none",
    strip.text = element_text(size = 15) 
  )

ridge_plot 
ggsave("ridge_plot.png", plot = ridge_plot, height = 12, width = 15)

## i like these ones enough 

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

service_ratings_long <- service_ratings |> 
  pivot_longer(cols = everything(), names_to = "Service", values_to = "Rating")

service_labels <- c(
  "seat_comfort" = "Seat Comfort",
  "departure_arrival_time_convenient" = "Departure/Arrival Time Convenient",
  "food_and_drink" = "Food and Drink",
  "gate_location" = "Gate Location",
  "inflight_wifi_service" = "Inflight WiFi Service",
  "inflight_entertainment" = "Inflight Entertainment",
  "online_support" = "Online Support",
  "ease_of_online_booking" = "Ease of Online Booking",
  "on_board_service" = "On-Board Service",
  "leg_room_service" = "Leg Room Service",
  "baggage_handling" = "Baggage Handling",
  "checkin_service" = "Checkin Service",
  "cleanliness" = "Cleanliness",
  "online_boarding" = "Online Boarding"
)


first_half <- service_ratings_long |> 
  filter(Service %in% names(service_labels)[1:7])
second_half <- service_ratings_long |> 
  filter(Service %in% names(service_labels)[8:14])

facet_density_plot1 <- ggplot(first_half, aes(x = Rating, fill = Service)) +
  geom_density(alpha = 0.7) +
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
    plot.title = element_text(hjust = 0.5, size = 22),
    plot.subtitle = element_text(hjust = 0.5, size = 17),
    strip.text = element_text(size = 15),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 15),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 12),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey90"),  # lighten the major grid lines
    panel.grid.minor = element_line(color = "grey90"),  
    panel.grid.major.x = element_blank(),  # remove major vertical grid lines
    panel.grid.minor.x = element_blank(),  
    panel.spacing = unit(1, "lines")
  ) +
  facet_wrap(~ Service, scales = "fixed", nrow = 4, labeller = labeller(Service = service_labels))

# facted density plots second half

num_services <- length(service_labels)
rainbow_colors <- rev(rainbow(num_services))

facet_density_plot2 <- ggplot(second_half, aes(x = Rating, fill = Service)) +
  geom_density(alpha = 0.7) +
  labs(
    title = "Density Plots of Service Satisfaction Ratings (Part 2)",
    subtitle = "Distribution of Ratings for Different Service Features",
    x = "Rating",
    y = "Density",
    fill = "Service"
  ) +
  scale_y_continuous(limits = c(0, 1)) +  # Set y-axis limit to 1
  scale_fill_manual(values = rainbow_colors, labels = service_labels) +  # Change legend names and colors
  theme(
    plot.title = element_text(hjust = 0.5, size = 22),
    plot.subtitle = element_text(hjust = 0.5, size = 17),
    strip.text = element_text(size = 15),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 15),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 12),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),  # Remove major vertical grid lines ???
    panel.grid.minor.x = element_blank(),  # Remove minor vertical grid lines ??
    panel.spacing = unit(1, "lines")
  ) +
  facet_wrap(~ Service, scales = "fixed", nrow = 4, labeller = labeller(Service = service_labels))

facet_density_plot1
facet_density_plot2

ggsave("facet_density_plot1.png", plot = facet_density_plot1, width = 15, height = 12)
ggsave("facet_density_plot2.png", plot = facet_density_plot2, width = 15, height = 12)
####### FOR PLOT 2 #######


service_ratings_plot2 <- airline |> select(
  class,
  seat_comfort,
  cleanliness,
  inflight_entertainment, 
  on_board_service
)

# convert data to so my life is easier 
service_ratings_plot2_long <- service_ratings_plot2 |>
  pivot_longer(cols = -class, names_to = "Service", values_to = "Rating")

# better label 
service_labels_plot2 <- c(
  seat_comfort = "Seat Comfort",
  cleanliness = "Cleanliness",
  inflight_entertainment = "Inflight Entertainment", 
  class = " Travel Class", 
  on_board_service = "On Board Service"
)


service_ratings_plot2_long <- service_ratings_plot2_long |>
  mutate(Service = factor(Service, levels = names(service_labels_plot2), labels = service_labels_plot2))

# colors 
custom_colors <- c("Business" = "lightblue", "Eco" = "lightgreen", "Eco Plus" = "plum1")

boxplot_plot <- ggplot(service_ratings_plot2_long, aes(x = class, y = Rating, fill = class)) +
  geom_boxplot() +
  labs(
    title = "Boxplots of Service Ratings by Travel Class",
    subtitle = "Comparing Ratings for Different Service Features",
    x = "Travel Class",
    y = "Rating",
    fill = "Travel Class"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 22),
    plot.subtitle = element_text(hjust = 0.5, size = 17),
    strip.text = element_text(size = 15),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 15),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 12),
    legend.position = "bottom"
  ) +
  facet_wrap(~ Service, scales = "free_y") + 
  scale_fill_manual(values = custom_colors)

# violin plot
violin_plot <- ggplot(service_ratings_plot2_long, aes(x = class, y = Rating, fill = class)) +
  geom_violin() +
  labs(
    title = "Violin Plots of Service Ratings by Travel Class",
    subtitle = "Comparing Ratings for Different Service Features",
    x = "Travel Class",
    y = "Rating",
    fill = "Travel Class"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 22),
    plot.subtitle = element_text(hjust = 0.5, size= 17),
    strip.text = element_text(size = 15),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 15),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 15),
    legend.position = "bottom"
  ) +
  facet_wrap(~ Service, scales = "free_y") + 
  scale_fill_manual(values = custom_colors)


boxplot_plot
violin_plot

ggsave("boxplot_plot.png", plot = boxplot_plot, width = 15, height = 12)
ggsave("violin_plot.png", plot = violin_plot, width = 15, height = 12)

##### PLOT NUMBER 3 #######


satisfaction_data <- airline |> select(
  satisfaction,
  type_of_travel,
  class,
  customer_type
)


# Type of Travel

satisfaction_data <- satisfaction_data |>
  mutate(type_of_travel = ifelse(type_of_travel == "Business travel", "Business Travel", type_of_travel))

type_of_travel_plot <- ggplot(satisfaction_data, aes(x = type_of_travel, fill = satisfaction)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(
    title = "Customer Satisfaction by Type of Travel",
    x = "Type of Travel",
    y = "Proportion",
    fill = "Satisfaction"
  ) +
  scale_fill_discrete(labels = c("Unsatisfied", "Satisfied")) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = "none"
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
  scale_fill_discrete(labels = c("Unsatisfied", "Satisfied")
                     ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = "none"
  )

# Customer Type
satisfaction_data <- satisfaction_data |>
  mutate(customer_type = ifelse(customer_type == "disloyal Customer", "Disloyal Customer", customer_type))

customer_type_plot <- ggplot(satisfaction_data, aes(x = customer_type, fill = satisfaction)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(
    title = "Customer Satisfaction by Customer Type",
    x = "Customer Type",
    y = "Proportion",
    fill = "Satisfaction"
  ) +
  scale_fill_discrete(labels = c("Unsatisfied", "Satisfied")) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = "none"
  )



type_of_travel_plot
class_plot
customer_type_plot

satisfaction_count <- airline |>
  count(satisfaction)

# bar plot with count
satisfaction_count_plot <- ggplot(satisfaction_count, aes(x = satisfaction, y = n, fill = satisfaction)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = n), vjust = 1.5, color = "white", size = 5) +
  scale_fill_manual(values = c("dissatisfied" = "#d95f02", "satisfied" = "#1b9e77"), 
                    labels = c("Unsatisfied", "Satisfied")) +
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

satisfaction_count_plot

# satisfaction grouped plot 

satisfaction_plot <- (type_of_travel_plot + class_plot) / (customer_type_plot + satisfaction_count_plot) + 
  plot_layout(guides = "collect") + 
  plot_annotation(title = "Analysis of Customer Satisfaction Across Different Dimensions")
satisfaction_plot <- satisfaction_plot & theme(
  legend.position = "bottom",
  legend.title = element_text(size = 10),
  legend.text = element_text(size = 8),
  plot.title = element_text(hjust = 0.5, size = 16)
)

satisfaction_plot
ggsave("satisfaction_plot.png", plot = satisfaction_plot,  width = 15, height = 10)

## same information as plot series 3 but in pie chart form 

# Type of Travel
satisfaction_data <- satisfaction_data |>
  mutate(type_of_travel = ifelse(type_of_travel == "Business travel", "Business Travel", type_of_travel))


type_of_travel_count <- satisfaction_data |>
  count(type_of_travel, satisfaction) |>
  group_by(type_of_travel) |>
  mutate(prop = n / sum(n), 
         label = scales::percent(prop, accuracy = 0.1))

# Create pie chart
type_of_travel_pie <- ggplot(type_of_travel_count, aes(x = "", y = prop, fill = satisfaction)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ type_of_travel) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), color = "white", size = 3) +
  theme_minimal() +
  labs(
    title = "By Type of Travel",
    x = NULL,
    y = NULL,
    fill = "Satisfaction"
  ) +
  scale_fill_discrete(labels = c("Unsatisfied", "Satisfied")) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 8),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8), 
    strip.text = element_text(size = 6.5)
  )

type_of_travel_pie


# class
class_count <- satisfaction_data |>
  count(class, satisfaction) |>
  group_by(class) |>
  mutate(prop = n / sum(n), 
         label = scales::percent(prop, accuracy = 0.1))

# Create pie chart
class_pie <- ggplot(class_count, aes(x = "", y = prop, fill = satisfaction)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ class) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), color = "white", size = 3) +
  theme_minimal() +
  labs(
    title = "By Class",
    x = NULL,
    y = NULL,
    fill = "Satisfaction"
  ) +
  scale_fill_discrete(labels = c("Unsatisfied", "Satisfied")) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 8),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8), 
    strip.text = element_text(size = 6.5)
  )

class_pie


# Customer Type

satisfaction_data <- satisfaction_data |>
  mutate(customer_type = ifelse(customer_type == "disloyal Customer", "Disloyal Customer", customer_type))


customer_type_agg <- satisfaction_data |>
  count(customer_type, satisfaction) |>
  group_by(customer_type) |>
  mutate(prop = n / sum(n), 
         label = scales::percent(prop, accuracy = 0.1))

# Create pie chart
customer_type_pie <- ggplot(customer_type_agg, aes(x = "", y = prop, fill = satisfaction)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ customer_type, nrow = 1) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), color = "white", size = 3) +
  theme_minimal() +
  labs(
    title = "By Customer Type",
    x = NULL,
    y = NULL,
    fill = "Satisfaction") +
  scale_fill_discrete(labels = c("Unsatisfied", "Satisfied")) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 8),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8), 
    strip.text = element_text(size = 6.5)
  )

customer_type_pie

piecharts <- (class_pie / (type_of_travel_pie  + customer_type_pie)) + 
  plot_layout(guides = "collect") + 
  plot_annotation(title = "Customer Satisfaction Analysis Across Various Dimensions with Percentage Values")

piecharts <- piecharts & theme(
  legend.position = "bottom",
  legend.title = element_text(size = 10),
  legend.text = element_text(size = 8),
  plot.title = element_text(hjust = 0.5, size = 15), 
  strip.text = element_text(size = 9.5), 
  plot.margin = margin(t = 15, r = 5, b = 5, l = 5)
)
piecharts


ggsave("piechats_plot.png", plot = piecharts,  width = 12, height = 8)
