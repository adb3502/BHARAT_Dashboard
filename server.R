# server.R - The Awesome Version
function(input, output, session) {
  
  # Helper function to get the right grouping
  get_group_var <- reactive({
    switch(input$grouping,
           "age" = "age_group",
           "sex" = "sex",
           "combined" = "group_combined")
  })
  
  # Blood parameter visualization
  output$blood_dist <- renderPlotly({
    req(input$blood_param)
    
    # Get current variable
    current_var <- input$blood_param
    group_var <- get_group_var()
    
    # Get reference ranges if available
    ref_range <- blood_metadata %>%
      filter(testname == str_replace_all(current_var, "_", "-")) %>%
      select(testminrangevalue, testmaxrangevalue)
    
    # Create clean plot data
    plot_df <- if(group_var == "group_combined") {
      blood_data %>%
        mutate(
          group = paste(age_group, sex),
          y_val = !!sym(current_var)
        ) %>%
        drop_na()
    } else {
      blood_data %>%
        mutate(
          group = !!sym(group_var),
          y_val = !!sym(current_var)
        ) %>%
        drop_na()
    }
    
    # Add ID for tooltips
    plot_df$id <- blood_data$participant_id[match(row.names(plot_df), row.names(blood_data))]
    
    # Y-axis limits
    y_min <- min(plot_df$y_val, na.rm = TRUE)
    y_max <- max(plot_df$y_val, na.rm = TRUE)
    
    if(nrow(ref_range) > 0) {
      if(!is.na(ref_range$testminrangevalue)) y_min <- min(y_min, ref_range$testminrangevalue)
      if(!is.na(ref_range$testmaxrangevalue)) y_max <- max(y_max, ref_range$testmaxrangevalue)
    }
    
    y_range <- y_max - y_min
    y_min <- y_min - 0.05 * y_range
    y_max <- y_max + 0.05 * y_range
    
    # Set up aesthetics
    if(input$color_by != "none" && input$color_by != group_var) {
      plot_df$color_group <- plot_df[[input$color_by]]
      n_colors <- length(unique(plot_df$color_group))
      aes_mapping <- aes(x = group, y = y_val, 
                         color = color_group,
                         text = paste("ID:", id, 
                                      "\nValue:", round(y_val, 2),
                                      "\nGroup:", group,
                                      "\nColor:", color_group))
    } else {
      plot_df$color_group <- plot_df$group
      n_colors <- length(unique(plot_df$group))
      aes_mapping <- aes(x = group, y = y_val, 
                         color = group,
                         text = paste("ID:", id, 
                                      "\nValue:", round(y_val, 2),
                                      "\nGroup:", group))
    }
    
    colors <- get_color_palette(input$color_scheme, n_colors)
    
    # Create base plot
    p <- ggplot(plot_df, aes_mapping) +
      theme_minimal(base_size = 14) +
      theme(
        text = element_text(family = "Manrope"),
        axis.title = element_text(family = "Manrope", size = 12),
        axis.text = element_text(family = "Manrope", size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)
      ) +
      scale_color_manual(values = colors) +
      labs(
        title = str_to_title(str_replace_all(current_var, "_", " ")),
        x = str_to_title(str_replace_all(group_var, "_", " ")),
        y = paste0(
          str_to_title(str_replace_all(current_var, "_", " ")),
          " (", blood_metadata$testmeasuringunit[blood_metadata$testname == str_replace_all(current_var, "_", "-")], ")"
        ),
        color = str_to_title(str_replace_all(
          if(input$color_by != "none") input$color_by else group_var, 
          "_", " "
        ))
      )
    
    # Add plot elements based on type
    p <- if(input$plot_type == "box") {
      if(input$color_by != "none" && input$color_by != group_var) {
        # Only boxplot by main group, color points by color_by
        p + 
          geom_boxplot(aes(color = NULL), alpha = 0.3, width = 0.7, outlier.shape = NA) +
          geom_point(position = position_jitter(width = 0.2), alpha = 0.7, size = 3)
      } else {
        p + 
          geom_boxplot(alpha = 0.3, width = 0.7, outlier.shape = NA) +
          geom_point(position = position_jitter(width = 0.2), alpha = 0.7, size = 3)
      }
    } else if(input$plot_type == "violin") {
      if(input$color_by != "none" && input$color_by != group_var) {
        p + 
          geom_violin(aes(color = NULL), alpha = 0.3, trim = FALSE) +
          geom_point(position = position_jitter(width = 0.2), alpha = 0.7, size = 3)
      } else {
        p + 
          geom_violin(alpha = 0.3, trim = FALSE) +
          geom_point(position = position_jitter(width = 0.2), alpha = 0.7, size = 3)
      }
    } else {
      p + 
        geom_point(position = position_jitter(width = 0.2), alpha = 0.7, size = 3)
    }
    
    # Add reference ranges if selected
    if(input$show_ref && nrow(ref_range) > 0) {
      if(!is.na(ref_range$testminrangevalue)) {
        p <- p + geom_hline(
          yintercept = ref_range$testminrangevalue,
          linetype = "dashed", 
          color = "grey50", 
          alpha = 0.5
        )
      }
      if(!is.na(ref_range$testmaxrangevalue)) {
        p <- p + geom_hline(
          yintercept = ref_range$testmaxrangevalue,
          linetype = "dashed", 
          color = "grey50", 
          alpha = 0.5
        )
      }
    }
    
    # Convert to plotly with custom tooltip
    ggplotly(p, tooltip = "text") %>%
      layout(
        font = list(family = "Manrope"),
        hoverlabel = list(font = list(family = "Manrope")),
        showlegend = TRUE,
        margin = list(b = 100)
      ) %>%
      config(displayModeBar = TRUE)
  })
  
  # Statistical analysis
  output$blood_stats <- renderText({
    req(input$blood_param, input$show_stats)
    
    current_var <- input$blood_param
    group_var <- get_group_var()
    
    # Create clean data for stats
    stats_df <- if(group_var == "group_combined") {
      blood_data %>%
        mutate(
          group = paste(age_group, sex),
          value = !!sym(current_var)
        ) %>%
        drop_na()
    } else {
      blood_data %>%
        mutate(
          group = !!sym(group_var),
          value = !!sym(current_var)
        ) %>%
        drop_na()
    }
    
    # Get descriptive stats
    desc_stats <- stats_df %>%
      group_by(group) %>%
      summarise(
        n = n(),
        mean = round(mean(value, na.rm = TRUE), 2),
        sd = round(sd(value, na.rm = TRUE), 2),
        median = round(median(value, na.rm = TRUE), 2),
        q1 = round(quantile(value, 0.25, na.rm = TRUE), 2),
        q3 = round(quantile(value, 0.75, na.rm = TRUE), 2)
      )
    
    # Format descriptive stats
    desc_text <- desc_stats %>%
      mutate(stats = sprintf(
        "%s (n=%d):\nMean ± SD: %0.2f ± %0.2f\nMedian [Q1, Q3]: %0.2f [%0.2f, %0.2f]",
        group, n, mean, sd, median, q1, q3
      )) %>%
      pull(stats) %>%
      paste(collapse = "\n\n")
    
    # Add inferential stats if applicable
    if(n_distinct(stats_df$group) == 2) {
      tryCatch({
        # T-test
        t_res <- t.test(value ~ group, data = stats_df)
        
        # Mann-Whitney test
        w_res <- wilcox.test(value ~ group, data = stats_df)
        
        # Effect size
        eff_size <- cohens_d(stats_df, value ~ group)$effsize
        
        # Combine all statistics
        paste("Descriptive Statistics:\n\n",
              desc_text,
              "\n\nInferential Statistics:\n",
              "\nParametric (t-test):",
              "\nt-statistic = ", round(t_res$statistic, 2),
              "\np-value = ", format.pval(t_res$p.value, digits = 3),
              "\n\nNon-parametric (Mann-Whitney):",
              "\nW-statistic = ", round(w_res$statistic, 2),
              "\np-value = ", format.pval(w_res$p.value, digits = 3),
              "\n\nEffect Size (Cohen's d) = ", round(eff_size, 2))
        
      }, error = function(e) {
        paste("Descriptive Statistics:\n\n",
              desc_text,
              "\n\nNote: Could not perform inferential statistics.",
              "\nError details: ", as.character(e))
      })
    } else if(n_distinct(stats_df$group) > 2) {
      tryCatch({
        # ANOVA
        aov_res <- aov(value ~ group, data = stats_df)
        aov_sum <- summary(aov_res)[[1]]
        
        # Kruskal-Wallis test
        kw_res <- kruskal.test(value ~ group, data = stats_df)
        
        # Effect size (eta-squared)
        eta_sq <- summary.lm(aov_res)$r.squared
        
        paste("Descriptive Statistics:\n\n",
              desc_text,
              "\n\nInferential Statistics:\n",
              "\nOne-way ANOVA:",
              "\nF-statistic = ", round(aov_sum$`F value`[1], 2),
              "\np-value = ", format.pval(aov_sum$`Pr(>F)`[1], digits = 3),
              "\n\nKruskal-Wallis test:",
              "\nChi-squared = ", round(kw_res$statistic, 2),
              "\np-value = ", format.pval(kw_res$p.value, digits = 3),
              "\n\nEffect Size (Eta-squared) = ", round(eta_sq, 2))
        
      }, error = function(e) {
        paste("Descriptive Statistics:\n\n",
              desc_text,
              "\n\nNote: Could not perform inferential statistics.",
              "\nError details: ", as.character(e))
      })
    } else {
      paste("Descriptive Statistics:\n\n", desc_text)
    }
  })
  
  # All stats calculation and volcano plot
  all_stats_table <- reactive({
    req(input$stats_grouping)
    
    group_var <- switch(input$stats_grouping,
                        "age" = "age_group",
                        "sex" = "sex",
                        "combined" = "group_combined")
    
    # Get numeric parameters
    param_list <- names(blood_data)[!names(blood_data) %in% 
                                      c("participant_id", "month", "age_group", 
                                        "sex", "group_combined", "sex_code")]
    
    # Calculate stats for each parameter
    stats_df <- map_df(param_list, function(param) {
      tryCatch({
        # Get clean data
        clean_data <- blood_data %>%
          select(!!sym(group_var), !!sym(param)) %>%
          drop_na()
        
        if(n_distinct(clean_data[[group_var]]) == 2) {
          # For two groups
          w_res <- wilcox.test(as.formula(sprintf("%s ~ %s", param, group_var)), 
                               data = clean_data)
          
          eff_size <- clean_data %>%
            cohens_d(as.formula(sprintf("%s ~ %s", param, group_var))) %>%
            pull(effsize)
          
          tibble(
            parameter = param,
            test_type = "Mann-Whitney",
            stat_value = w_res$statistic,
            p_value = w_res$p.value,
            effect_size = eff_size
          )
        } else {
          # For multiple groups
          kw_res <- kruskal.test(as.formula(sprintf("%s ~ %s", param, group_var)), 
                                 data = clean_data)
          
          # Eta-squared from ANOVA
          aov_res <- aov(as.formula(sprintf("%s ~ %s", param, group_var)), 
                         data = clean_data)
          eta_sq <- summary.lm(aov_res)$r.squared
          
          tibble(
            parameter = param,
            test_type = "Kruskal-Wallis",
            stat_value = kw_res$statistic,
            p_value = kw_res$p.value,
            effect_size = eta_sq
          )
        }
      }, error = function(e) {
        tibble(
          parameter = param,
          test_type = NA_character_,
          stat_value = NA_real_,
          p_value = NA_real_,
          effect_size = NA_real_
        )
      })
    })
    
    # Add FDR correction
    stats_df %>%
      mutate(
        fdr_pvalue = p.adjust(p_value, method = "BH"),
        significant = p_value < 0.05,
        sig_after_correction = fdr_pvalue < 0.05
      ) %>%
      arrange(p_value)
  })
  
  # Render the stats table
  output$stats_table <- renderDT({
    all_stats_table() %>%
      mutate(
        parameter = str_to_title(str_replace_all(parameter, "_", " ")),
        p_value = format.pval(p_value, digits = 3),
        fdr_pvalue = format.pval(fdr_pvalue, digits = 3),
        effect_size = round(effect_size, 3),
        stat_value = round(stat_value, 2)
      ) %>%
      datatable(
        options = list(
          pageLength = 15,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().container()).css({'font-family': 'Manrope'});",
            "}")
        ),
        extensions = 'Buttons',
        colnames = c(
          "Parameter", 
          "Test Type",
          "Test Statistic",
          "P-value", 
          "Effect Size",
          "FDR P-value",
          "Significant",
          "Significant after FDR"
        ),
        rownames = FALSE
      ) %>%
      formatStyle(
        'significant',
        backgroundColor = styleEqual(c(TRUE, FALSE), c('#90EE90', '#FFB6C6'))
      ) %>%
      formatStyle(
        'sig_after_correction',
        backgroundColor = styleEqual(c(TRUE, FALSE), c('#90EE90', '#FFB6C6'))
      )
  })
  
  # Volcano plot
  output$volcano_plot <- renderPlotly({
    stats_df <- all_stats_table()
    
    # Create volcano plot
    p <- ggplot(stats_df, 
                aes(x = effect_size, 
                    y = -log10(p_value),
                    color = sig_after_correction,
                    text = paste("Parameter:", parameter,
                                 "\nEffect Size:", round(effect_size, 3),
                                 "\np-value:", format.pval(p_value, digits = 3)))) +
      geom_point(size = 3) +
      geom_hline(yintercept = -log10(0.05), 
                 linetype = "dashed", 
                 color = "grey50", 
                 alpha = 0.5) +
      theme_minimal() +
      theme(
        text = element_text(family = "Manrope"),
        axis.title = element_text(family = "Manrope", size = 12),
        axis.text = element_text(family = "Manrope", size = 10),
        legend.position = "right",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
      ) +
      scale_color_manual(
        values = c("TRUE" = zissou_palette[1], "FALSE" = zissou_palette[5]),
        labels = c("TRUE" = "Significant", "FALSE" = "Not Significant"),
        name = "FDR Corrected"
      ) +
      labs(
        x = "Effect Size",
        y = "-log10(p-value)",
        title = paste("Volcano Plot by", str_to_title(input$stats_grouping))
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        font = list(family = "Manrope"),
        hoverlabel = list(font = list(family = "Manrope")),
        showlegend = TRUE
      )
  })
}