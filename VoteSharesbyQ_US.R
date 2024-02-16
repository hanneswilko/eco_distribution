library(dplyr)
library(ggplot2)


# Create a data frame with the provided data
data <- data.frame(
  Year = rep(c(1996, 2000, 2004, 2008, 2012, 2016, 2020), each = 10),
  Quintile = rep(rep(1:5, each = 2), times = 7),
  Party = rep(c(0, 1), times = 70),
  Percentage = c(
    # 1996
    0.32, 0.68, # Quintile 1
    0.38, 0.62, # Quintile 2
    0.42, 0.58, # Quintile 3
    0.49, 0.51, # Quintile 4
    0.57, 0.43, # Quintile 5
    # 2000
    0.48, 0.52, # Quintile 1
    0.54, 0.46, # Quintile 2
    0.59, 0.41, # Quintile 3
    0.65, 0.35, # Quintile 4
    0.76, 0.24, # Quintile 5
    # 2004
    0.52, 0.48, # Quintile 1
    0.56, 0.44, # Quintile 2
    0.58, 0.42, # Quintile 3
    0.62, 0.38, # Quintile 4
    0.64, 0.36, # Quintile 5
    # 2008
    0.33, 0.67, # Quintile 1
    0.44, 0.56, # Quintile 2
    0.50, 0.50, # Quintile 3
    0.58, 0.42, # Quintile 4
    0.62, 0.38, # Quintile 5
    # 2012
    0.39, 0.61, # Quintile 1
    0.42, 0.58, # Quintile 2
    0.46, 0.54, # Quintile 3
    0.47, 0.53, # Quintile 4
    0.47, 0.53, # Quintile 5
    # 2016
    0.46, 0.54, # Quintile 1
    0.52, 0.48, # Quintile 2
    0.50, 0.50, # Quintile 3
    0.48, 0.52, # Quintile 4
    0.50, 0.50, # Quintile 5
    # 2020
    0.43, 0.57, # Quintile 1
    0.46, 0.54, # Quintile 2
    0.44, 0.56, # Quintile 3
    0.39, 0.61, # Quintile 4
    0.40, 0.60  # Quintile 5
  )
)

# Convert Year and Party to factor
data$Year <- as.factor(data$Year)
data$Party <- as.factor(data$Party)

# Create a new variable for the x-axis that combines Year and Quintile
data$Year_Quintile <- paste(data$Year, data$Quintile, sep = "-")

# Plot1
plot1 <- ggplot(data, aes(x = Year_Quintile, y = Percentage, fill = Party)) +
  geom_bar(stat = "identity", position = "fill", width = 0.9) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"), labels = c("0 (rep)", "1 (dem)")) +
  labs(x = "Year and Quintile", y = "Percentage", fill = "Party", 
       title = "Percentage of 0 (rep) and 1 (dem) by Year and Quintile") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("bar1_US_VotebyQ.png")

# Plot2 with spaced breaks after every fifth column
plot2 <- ggplot(data, aes(x = Year_Quintile, y = Percentage, fill = Party)) +
  geom_bar(stat = "identity", position = "fill", width = 0.9) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"), 
                    labels = c("Republicans (0)", "Democrats (1)")) +
  labs(x = "Year and Quintile", y = "Percentage", fill = "Party", 
       title = "Share of Voting for the Republicans and Democrats by Year & Quantile") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  facet_wrap(~Year, scales = "free_x", strip.position = "bottom")

ggsave("bar2_US_VotebyQ.png")


# Extract quintile from Year_Quintile
data$Quintile <- gsub("^\\d{4}-", "", data$Year_Quintile)

# Original code with modified x-axis labels
plot3 <- ggplot(data, aes(x = Quintile, y = Percentage, fill = Party)) +
  geom_bar(stat = "identity", position = "fill", width = 0.9) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"), 
                    labels = c("Republicans (0)", "Democrats (1)")) +
  labs(x = "Quintile", y = "Percentage", fill = "Party") +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust = 1)) +
  facet_wrap(~Year, scales = "free_x", strip.position = "bottom")

setwd("/Users/hannes/Documents/Studium/Master/Third_Semester/Economics_of_distribution/seminar/research_project/R_Code")
ggsave("bar3_US_VotebyQ.png")









