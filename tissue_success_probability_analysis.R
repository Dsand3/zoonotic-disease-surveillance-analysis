# -----------------------------------------------------------------------------
# tissue_success_probability_analysis.R
# Purpose:
#   Compute tissue-level success probability (1 vs 0), Wilson CI, plot results,
#   and optionally run Fisher's exact pairwise tests + logistic regression.
# -----------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(binom)

# ---- INPUT ------------------------------------------------------------------
input_csv <- "tissue_freq.csv"
id_col <- "Case_id"   # ID column that is NOT a tissue

# ---- Read + reshape wide -> long --------------------------------------------
tissue_freq <- read.csv(input_csv, stringsAsFactors = FALSE, check.names = FALSE)

long_tissue <- tissue_freq %>%
  pivot_longer(
    cols = -all_of(id_col),
    names_to = "Tissue",
    values_to = "Result"
  )

# ---- Summarise counts per tissue --------------------------------------------
summary_stats <- long_tissue %>%
  group_by(Tissue) %>%
  summarise(
    Count_Not_Taken = sum(Result == "Not Taken", na.rm = TRUE),
    Count_0 = sum(Result == "0", na.rm = TRUE),
    Count_1 = sum(Result == "1", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Total_Taken = Count_0 + Count_1,
    Success_Probability = ifelse(Total_Taken > 0, Count_1 / Total_Taken, NA_real_),
    Success_Percentage = Success_Probability * 100
  )

# ---- Add Wilson confidence intervals ----------------------------------------
summary_stats <- summary_stats %>%
  rowwise() %>%
  mutate(
    CI = list(binom.confint(Count_1, Total_Taken, methods = "wilson"))
  ) %>%
  unnest(cols = CI) %>%
  ungroup()

print(summary_stats)

# ---- Plot (ordered by success probability) ----------------------------------
ggplot(summary_stats, aes(x = reorder(Tissue, -Success_Probability),
                          y = Success_Probability,
                          fill = Tissue)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = "Success Probability of Organism Growth by Tissue",
    x = "Tissue (Ordered by Success Probability)",
    y = "Success Probability"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# ---- OPTIONAL: Pairwise Fisher’s Exact Tests --------------------------------
# (Can get large fast if you have many tissues.)
run_pairwise_fisher <- FALSE

if (run_pairwise_fisher) {
  tissue_list <- unique(summary_stats$Tissue)

  results <- list()

  for (i in 1:(length(tissue_list) - 1)) {
    for (j in (i + 1):length(tissue_list)) {
      t1 <- tissue_list[i]
      t2 <- tissue_list[j]

      data_t1 <- summary_stats %>% filter(Tissue == t1)
      data_t2 <- summary_stats %>% filter(Tissue == t2)

      mat <- matrix(
        c(
          data_t1$Count_1, data_t1$Total_Taken - data_t1$Count_1,
          data_t2$Count_1, data_t2$Total_Taken - data_t2$Count_1
        ),
        nrow = 2,
        byrow = TRUE
      )

      test_res <- fisher.test(mat)

      results[[length(results) + 1]] <- data.frame(
        Tissue1 = t1,
        Tissue2 = t2,
        p_value = test_res$p.value,
        odds_ratio = unname(test_res$estimate),
        lower_CI = test_res$conf.int[1],
        upper_CI = test_res$conf.int[2]
      )
    }
  }

  fisher_results <- bind_rows(results) %>%
    mutate(adjusted_p_value = p.adjust(p_value, method = "bonferroni")) %>%
    arrange(adjusted_p_value)

  print(fisher_results)
}

# ---- OPTIONAL: Logistic regression Result ~ Tissue ---------------------------
# Uses only 0/1 outcomes, excludes "Not Taken"
run_logistic_regression <- FALSE

if (run_logistic_regression) {
  glm_data <- long_tissue %>%
    filter(Result %in% c("0", "1")) %>%
    mutate(
      Result = as.numeric(Result),
      Tissue = as.factor(Tissue)
    )

  model <- glm(Result ~ Tissue, data = glm_data, family = binomial)
  print(summary(model))
}
