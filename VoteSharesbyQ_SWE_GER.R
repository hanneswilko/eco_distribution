library(dplyr)
library(ggplot2)

###########SWEDEN######################################
# Create a data frame with the provided data
data_SWE <- data.frame(
  Year = rep(c(2002, 2006), each = 10),
  Quintile = rep(rep(1:5, each = 2), times = 2),
  Party = rep(c(0, 1), times = 20),
  Percentage = c(
    # 2002
    0.38, 0.62, # Quintile 1
    0.42, 0.58, # Quintile 2
    0.42, 0.58, # Quintile 3
    0.45, 0.55, # Quintile 4
    0.51, 0.49, # Quintile 5
    # 2006
    0.47, 0.53, # Quintile 1
    0.47, 0.53, # Quintile 2
    0.49, 0.51, # Quintile 3
    0.51, 0.49, # Quintile 4
    0.55, 0.45 # Quintile 5
  )
)

# Convert Year and Party to factor
data_SWE$Year <- as.factor(data_SWE$Year)
data_SWE$Party <- as.factor(data_SWE$Party)

# Create a new variable for the x-axis that combines Year and Quintile
data_SWE$Year_Quintile <- paste(data_SWE$Year, data_SWE$Quintile, sep = "-")

# Plot1 with spaced breaks after every fifth column
plot1 <- ggplot(data_SWE, aes(x = Year_Quintile, y = Percentage, fill = Party)) +
  geom_bar(stat = "identity", position = "fill", width = 0.9) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"), 
                    labels = c("Center/Right Parties (0)", "Left Parties (1)")) +
  labs(x = "Year and Quintile", y = "Percentage", fill = "Party", 
       title = "Share of Voting for the Center/Right and Left Parties by Year & Quantile") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  facet_wrap(~Year, scales = "free_x", strip.position = "bottom")

ggsave("bar2_US_VotebyQ.png")


# Extract quintile from Year_Quintile
data_SWE$Quintile <- gsub("^\\d{4}-", "", data_SWE$Year_Quintile)

# Original code with modified x-axis labels
plot2 <- ggplot(data_SWE, aes(x = Quintile, y = Percentage, fill = Party)) +
  geom_bar(stat = "identity", position = "fill", width = 0.9) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"), 
                    labels = c("Center/Right Parties (0)", "Left Parties (1)")) +
  labs(x = "Quantile", y = "Percentage", fill = "Party") +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust = 1)) +
  facet_wrap(~Year, scales = "free_x", strip.position = "bottom")

setwd("/Users/hannes/Documents/Studium/Master/Third_Semester/Economics_of_distribution/seminar/research_project/R_Code")
ggsave("bar1_SWE_VotebyQ.png")


###########Germany######################################
# Create a data frame with the provided data
data_GER <- data.frame(
  Year = rep(c(2013, 2017), each = 10),
  Quintile = rep(rep(1:5, each = 2), times = 2),
  Party = rep(c(0, 1), times = 20),
  Percentage = c(
    # 2013
    0.46, 0.54, # Quintile 1
    0.48, 0.52, # Quintile 2
    0.42, 0.58, # Quintile 3
    0.49, 0.51, # Quintile 4
    0.47, 0.53, # Quintile 5
    # 2017
    0.51, 0.49, # Quintile 1
    0.52, 0.48, # Quintile 2
    0.55, 0.45, # Quintile 3
    0.57, 0.43, # Quintile 4
    0.58, 0.42 # Quintile 5
  )
)

# Convert Year and Party to factor
data_GER$Year <- as.factor(data_GER$Year)
data_GER$Party <- as.factor(data_GER$Party)

# Create a new variable for the x-axis that combines Year and Quintile
data_GER$Year_Quintile <- paste(data_GER$Year, data_GER$Quintile, sep = "-")

# Plot1 with spaced breaks after every fifth column
plot1 <- ggplot(data_GER, aes(x = Year_Quintile, y = Percentage, fill = Party)) +
  geom_bar(stat = "identity", position = "fill", width = 0.9) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"), 
                    labels = c("Center/Right Parties (0)", "Left Parties (1)")) +
  labs(x = "Year and Quintile", y = "Percentage", fill = "Party", 
       title = "Share of Voting for the Center/Right and Left Parties by Year & Quantile") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  facet_wrap(~Year, scales = "free_x", strip.position = "bottom")

ggsave("bar2_US_VotebyQ.png")


# Extract quintile from Year_Quintile
data_GER$Quintile <- gsub("^\\d{4}-", "", data_GER$Year_Quintile)

# Original code with modified x-axis labels
plot2 <- ggplot(data_GER, aes(x = Quintile, y = Percentage, fill = Party)) +
  geom_bar(stat = "identity", position = "fill", width = 0.9) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"), 
                    labels = c("Center/Right Parties (0)", "Left Parties (1)")) +
  labs(x = "Quintile", y = "Percentage", fill = "Party") +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust = 1)) +
  facet_wrap(~Year, scales = "free_x", strip.position = "bottom")

setwd("/Users/hannes/Documents/Studium/Master/Third_Semester/Economics_of_distribution/seminar/research_project/R_Code")
ggsave("bar2_GER_VotebyQ.png")






