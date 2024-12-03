library(rstatix)
library(effsize)

# Function to generate summary statistics
generate_summary_stats <- function(data, variable, group) {
  data %>%
    group_by(!!sym(group)) %>%
    summarise(
      n = n(),
      mean = mean(!!sym(variable), na.rm = TRUE),
      sd = sd(!!sym(variable), na.rm = TRUE),
      median = median(!!sym(variable), na.rm = TRUE),
      q1 = quantile(!!sym(variable), 0.25, na.rm = TRUE),
      q3 = quantile(!!sym(variable), 0.75, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      across(where(is.numeric), ~round(., 2))
    )
}

# Function to perform statistical tests
perform_statistical_test <- function(data, variable, group) {
  # Check number of groups
  n_groups <- length(unique(data[[group]]))
  
  if(n_groups == 2) {
    # t-test and wilcoxon
    t_test_result <- t.test(as.formula(paste(variable, "~", group)), data = data)
    wilcox_result <- wilcox.test(as.formula(paste(variable, "~", group)), data = data)
    effect_size <- cohens_d(data, as.formula(paste(variable, "~", group)))
    
    results <- list(
      test_type = "Two groups",
      t_test = list(
        statistic = t_test_result$statistic,
        p_value = t_test_result$p.value
      ),
      wilcox = list(
        statistic = wilcox_result$statistic,
        p_value = wilcox_result$p.value
      ),
      effect_size = effect_size$effsize
    )
  } else {
    # ANOVA and Kruskal-Wallis
    anova_result <- aov(as.formula(paste(variable, "~", group)), data = data)
    kw_result <- kruskal.test(as.formula(paste(variable, "~", group)), data = data)
    
    results <- list(
      test_type = "Multiple groups",
      anova = list(
        f_value = summary(anova_result)[[1]]$"F value"[1],
        p_value = summary(anova_result)[[1]]$"Pr(>F)"[1]
      ),
      kruskal = list(
        statistic = kw_result$statistic,
        p_value = kw_result$p.value
      ),
      effect_size = summary.lm(anova_result)$r.squared  # R-squared as effect size
    )
  }
  
  return(results)
}

# Function to format statistical results
format_statistical_results <- function(results) {
  if(results$test_type == "Two groups") {
    sprintf(
      "Statistical Analysis:\n\nParametric (t-test):\nt = %.2f, p = %.3f\n\nNon-parametric (Wilcoxon):\nW = %.2f, p = %.3f\n\nEffect size (Cohen's d) = %.2f",
      results$t_test$statistic,
      results$t_test$p_value,
      results$wilcox$statistic,
      results$wilcox$p_value,
      results$effect_size
    )
  } else {
    sprintf(
      "Statistical Analysis:\n\nParametric (ANOVA):\nF = %.2f, p = %.3f\n\nNon-parametric (Kruskal-Wallis):\nχ² = %.2f, p = %.3f\n\nEffect size (R²) = %.2f",
      results$anova$f_value,
      results$anova$p_value,
      results$kruskal$statistic,
      results$kruskal$p_value,
      results$effect_size
    )
  }
}

# Function to calculate correlations between multiple variables
calculate_correlations <- function(data, variables) {
  cors <- data %>%
    select(all_of(variables)) %>%
    cor(use = "pairwise.complete.obs")
  
  # Add p-values
  n <- nrow(data)
  p_values <- cors
  for(i in 1:ncol(cors)) {
    for(j in 1:ncol(cors)) {
      if(i != j) {
        test <- cor.test(data[[variables[i]]], data[[variables[j]]])
        p_values[i,j] <- test$p.value
      }
    }
  }
  
  list(
    correlations = cors,
    p_values = p_values
  )
}