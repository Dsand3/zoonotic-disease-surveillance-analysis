# -----------------------------------------------------------------------------
# Surveillance Analysis and Visualization Script
#
# Purpose:
#   Performs annual case count summarization and visualization
#   stratified by laboratory result.
#
# Description:
#   - Reads processed case dataset
#   - Converts date fields to Date objects
#   - Extracts year of receipt
#   - Aggregates counts by Year and Result
#   - Ensures full year representation (including zero counts)
#   - Generates bar plots and summary tables
# -----------------------------------------------------------------------------

# Load required libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(knitr)
library(kableExtra)
library(tidyr)

# -----------------------------------------------------------------------------
# Step 1: Read processed dataset
# -----------------------------------------------------------------------------
bru_data_processed_csv <- read.csv("Data.csv", stringsAsFactors = FALSE)

# -----------------------------------------------------------------------------
# Step 2: Convert date column and extract Year
# -----------------------------------------------------------------------------
bru_data_date_processed <- bru_data_processed_csv %>%
  mutate(
    Date.Received = as.Date(Date.Received, format = "%Y-%m-%d"),  # Convert to Date
    Year = year(Date.Received)                                   # Extract year
  ) %>%
  filter(!is.na(Date.Received))  # Remove rows with missing dates

# -----------------------------------------------------------------------------
# Step 3: Generate complete sequence of years in dataset
# -----------------------------------------------------------------------------
bru_years <- data.frame(
  Year = seq(
    min(bru_data_date_processed$Year, na.rm = TRUE),
    max(bru_data_date_processed$Year, na.rm = TRUE),
    by = 1
  )
)

# -----------------------------------------------------------------------------
# Step 4: Aggregate case counts by Year and Result
# -----------------------------------------------------------------------------
bru_data_filtered <- bru_data_date_processed %>%
  group_by(Year, Culture.Result) %>%
  summarise(Count = n(), .groups = 'drop')

# -----------------------------------------------------------------------------
# Step 5: Ensure all years are represented (including zero-case years)
# -----------------------------------------------------------------------------
summary_bru_data <- full_join(bru_years, bru_data_filtered, by = "Year") %>%
  replace_na(list(Count = 0))

# -----------------------------------------------------------------------------
# Step 6: Visualization – Annual Case Counts by Result
# -----------------------------------------------------------------------------
ggplot(summary_bru_data,
       aes(x = factor(Year),
           y = Count,
           fill = Culture.Result)) +
  geom_bar(stat = "identity") +
  labs(title = "Annual Case Counts by Result",
       x = "Year",
       y = "Number of Cases",
       fill = "Result") +
  scale_x_discrete(drop = FALSE) +
  theme_minimal()

# -----------------------------------------------------------------------------
# Step 7: Create Summary Tables
# -----------------------------------------------------------------------------

# Separate positive and negative subsets
bru_positives <- bru_data_filtered %>%
  filter(Culture.Result == "Positive")

bru_negatives <- bru_data_filtered %>%
  filter(Culture.Result == "Negative")

# Total counts by result
total_summary <- summary_bru_data %>%
  group_by(Culture.Result) %>%
  summarise(Total = sum(Count), .groups = 'drop')

# Format tables for reporting
total_summary_table <- total_summary %>%
  kable("html",
        col.names = c("Result", "Total Count"),
        caption = "Total Case Counts by Result") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

bru_pos_table <- bru_positives %>%
  kable("html",
        col.names = c("Year", "Result", "Count"),
        caption = "Positive Counts by Year") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

bru_neg_table <- bru_negatives %>%
  kable("html",
        col.names = c("Year", "Result", "Count"),
        caption = "Negative Counts by Year") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

# Print tables
bru_pos_table
bru_neg_table
total_summary_table
