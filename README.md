Overview

This project implements a reproducible R workflow for cleaning, aggregating, and visualizing retrospective infectious disease case data. The analysis converts record-level laboratory data into case-level datasets, generates year-level temporal features, and produces stratified annual trend visualizations.

The workflow emphasizes:

Flexible data ingestion (CSV/Excel support)

Case-level aggregation from repeated records

Temporal feature engineering (date parsing, year extraction)

Stratified case count summarization

Reproducible visualization outputs

No proprietary or client data included (code only)

Project Structure

case_data_processing.R – Reusable function for ingesting and aggregating multi-row case datasets into case-level format.

annual_case_trend_analysis.R – Performs year-level aggregation and visualization of case outcomes.

run_analysis.R – Optional driver script to execute the workflow.

Methods Used

dplyr for grouped summarization

Date parsing with lubridate

Zero-filling missing years

Tidyverse-based visualization (ggplot2)

Structured output tables for reporting
