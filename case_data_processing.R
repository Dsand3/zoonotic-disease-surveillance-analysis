# Load required libraries
library(dplyr)   # Data manipulation and aggregation
library(readxl)  # Reading Excel files

# -----------------------------------------------------------------------------
# Function: process_file
# Purpose:
#   Reads a CSV or Excel file containing case-level data and aggregates
#   repeated records into a single row per case identifier.
#
# Description:
#   - Automatically detects file type (CSV or Excel)
#   - Validates that the specified case identifier column exists
#   - Groups data by case ID
#   - Collapses duplicate values within each case into comma-separated strings
#   - Returns a cleaned, case-level aggregated data frame
#
# Parameters:
#   file_path   : Path to input file (CSV, XLS, XLSX)
#   case_col    : Name of the column representing unique case identifiers
#   sheet_name  : Excel sheet name or index (default = 1)
#   excel_skip  : Number of rows to skip when reading Excel (default = 1)
#
# Returns:
#   A data frame aggregated at the case level
# -----------------------------------------------------------------------------

process_file <- function(file_path,
                         case_col = "case_id",
                         sheet_name = 1,
                         excel_skip = 1) {

  # Determine file extension
  file_extension <- tools::file_ext(file_path)

  # ---------------------------------------------------------------------------
  # Read the file depending on its format
  # ---------------------------------------------------------------------------
  if (file_extension == "csv") {

    # Read CSV file without converting strings to factors
    df <- read.csv(file_path,
                   stringsAsFactors = FALSE,
                   check.names = FALSE)

  } else if (file_extension %in% c("xls", "xlsx")) {

    # Read Excel file, optionally skipping header metadata rows
    df <- read_excel(file_path,
                     sheet = sheet_name,
                     skip = excel_skip)

  } else {

    # Stop execution if unsupported file type is provided
    stop("Unsupported file type. Please provide a CSV or Excel file.")
  }

  # ---------------------------------------------------------------------------
  # Validate that the case identifier column exists
  # ---------------------------------------------------------------------------
  if (!case_col %in% names(df)) {
    stop(paste0("case_col not found in data: ", case_col))
  }

  # ---------------------------------------------------------------------------
  # Aggregate dataset to one row per case identifier
  # ---------------------------------------------------------------------------
  result <- df %>%
    group_by(across(all_of(case_col))) %>%  # Group rows by case ID
    summarize(
      across(
        everything(),
        ~ {
          # Convert values to character to ensure consistent collapse
          vals <- unique(as.character(.))

          # Remove missing or empty values
          vals <- vals[!is.na(vals) & vals != ""]

          # Combine remaining unique values into a single string
          paste(vals, collapse = ", ")
        }
      ),
      .groups = "drop"  # Remove grouping structure after summarization
    )

  # Return aggregated dataset
  return(result)
}
