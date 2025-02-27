# Name: Louisa Beckett
# Date: 2025-02-19
# Purpose: Read COVID-19 data from a URL and perform initial analysis

# Load necessary library
library(readr)

# URL to the COVID-19 dataset (this is just an example, you should replace with the actual URL)
url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-recent.csv"

# Read the data from the URL
covid_data <- read_csv(url)

# View the first few rows of the data
head(covid_data)# Name: Louisa Beckett
# Date: 2025-02-19
# Purpose: Read COVID-19 data from a URL and perform initial analysis

# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)

# URL to the COVID-19 dataset
url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-recent.csv"

# Read the data from the URL
covid_data <- read_csv(url)

# View the first few rows of the data
head(covid_data)

# Convert the date column to Date format
covid_data$date <- as.Date(covid_data$date)

# Summarize data to find the top 6 states with the most cases (sum by state)
top_states <- covid_data %>%
  group_by(state) %>%
  summarize(total_cases = sum(cases, na.rm = TRUE)) %>%
  arrange(desc(total_cases)) %>%
  slice_head(n = 6) %>%
  pull(state)  # Get just the names of the states

# Filter the data for the top 6 states
filtered_data <- covid_data %>%
  filter(state %in% top_states)

# Create the img directory if it doesn't exist
if (!dir.exists("img")) {
  dir.create("img")
}

# Create a ggplot
plot <- ggplot(filtered_data, aes(x = date, y = cases, color = state, group = state)) +
  geom_line() +  # Line plot
  labs(
    title = "COVID-19 Cases Over Time by State",
    x = "Date",
    y = "Number of Cases"
  ) +
  facet_wrap(~ state) +  # Facet by state
  theme_minimal() +  # Use a minimal theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.spacing = unit(1, "lines")  # Space between facets
  )

# Save the plot to the img directory
ggsave("img/top_6_states_cases.png", plot, width = 10, height = 6)
