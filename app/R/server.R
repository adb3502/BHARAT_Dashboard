# server.R
function(input, output, session) {
  
  # Helper function to get the right grouping
  get_group_var <- reactive({
    switch(input$grouping,
           "age" = "age_group",
           "sex" = "sex",
           "combined" = "group_combined")
  })
  
  # Overview Tab ------------------------------------------------------------
  
  # Demographic pyramid plot
  output$demographics_plot <- renderPlotly({
    demo_data <- bharat_data %>%
      group_by(age_group, sex) %>%
      summarise(count = n(), .groups = "drop")
    
    p <- ggplot(demo_data) +
      geom_bar(data = filter(demo_data, sex == "Male"),
               aes(x = age_group, y = -count, fill = "Male"),
               stat = "identity") +
      geom_bar(data = filter(demo_data, sex == "Female"),
               aes(x = age_group, y = count, fill = "Female"),
               stat = "identity") +
      coord_flip() +
      scale_fill_manual(values = wes_palette("Zissou1")[c(1,5)]) +
      labs(title = "Age and Sex Distribution",
           y = "Count", x = "Age Group") +
      theme_minimal(base_size = 14) +
      theme(
        text = element_text(family = "Manrope"),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
      )
    
    ggplotly(p)
  })
  
  # Age distribution plot
  output$age_dist_plot <- renderPlotly({
    p <- ggplot(bharat_data, aes(x = age_years, fill = sex)) +
      geom_density(alpha = 0.6) +
      scale_fill_manual(values = wes_palette("Zissou1")[c(1,5)]) +
      labs(title = "Age Distribution by Sex",
           x = "Age (years)", y = "Density") +
      theme_minimal(base_size = 14) +
      theme(
        text = element_text(family = "Manrope"),
        plot.title = element_text(hjust = 0.5)
      )
    
    ggplotly(p)
  })
  
  # Study completion plot
  output$completion_plot <- renderPlotly({
    completion_data <- bharat_data %>%
      summarise(across(ends_with("_completed"), 
                       ~mean(. == 1, na.rm = TRUE) * 100)) %>%
      gather(assessment, completion_rate) %>%
      mutate(assessment = str_replace(assessment, "_completed", ""),
             assessment = str_replace_all(assessment, "_", " "),
             assessment = str_to_title(assessment))
    
    p <- ggplot(completion_data, 
                aes(x = reorder(assessment, completion_rate), 
                    y = completion_rate,
                    fill = completion_rate)) +
      geom_bar(stat = "identity") +
      scale_fill_gradientn(colors = wes_palette("Zissou1")) +
      coord_flip() +
      labs(title = "Assessment Completion Rates",
           x = "", y = "Completion Rate (%)") +
      theme_minimal(base_size = 14) +
      theme(
        text = element_text(family = "Manrope"),
        plot.title = element_text(hjust = 0.5)
      )
    
    ggplotly(p)
  })
  
  # Data completeness heatmap
  output$completeness_plot <- renderPlotly({
    # Calculate completeness for each category
    completeness_data <- bharat_data %>%
      select(-participant_id) %>%
      summarise(across(everything(), 
                       ~sum(!is.na(.))/n() * 100)) %>%
      gather(variable, completeness) %>%
      mutate(
        category = case_when(
          str_detect(variable, "mmse|cognitive") ~ "Cognitive",
          str_detect(variable, "frail|physical") ~ "Physical",
          str_detect(variable, "exercise|diet") ~ "Lifestyle",
          str_detect(variable, "bp|pulse|temp") ~ "Clinical",
          TRUE ~ "Other"
        )
      )
    
    plot_ly(
      data = completeness_data,
      x = ~variable,
      y = ~category,
      z = ~completeness,
      type = "heatmap",
      colors = wes_palette("Zissou1"),
      text = ~sprintf("%.1f%%", completeness),
      hoverongaps = FALSE
    ) %>%
      layout(
        title = list(
          text = "Data Completeness by Category",
          font = list(family = "Manrope", size = 16)
        ),
        xaxis = list(title = "", showticklabels = FALSE),
        yaxis = list(title = "")
      )
  })
  
  # Clinical Parameters Tab -------------------------------------------------
  
  output$param_dist <- renderPlotly({
    req(input$clinical_param)
    
    current_var <- input$clinical_param
    group_var <- get_group_var()
    
    plot_df <- bharat_data %>%
      select(!!sym(current_var), !!sym(group_var)) %>%
      drop_na()
    
    p <- ggplot(plot_df, 
                aes(x = !!sym(group_var), 
                    y = !!sym(current_var),
                    fill = !!sym(group_var))) +
      theme_minimal(base_size = 14) +
      theme(
        text = element_text(family = "Manrope"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)
      ) +
      scale_fill_manual(values = get_color_palette(input$color_scheme, 
                                                   n_distinct(plot_df[[group_var]]))) +
      labs(
        title = str_to_title(str_replace_all(current_var, "_", " ")),
        x = str_to_title(str_replace_all(group_var, "_", " ")),
        y = names(clinical_params)[clinical_params == current_var]
      )
    
    if(input$plot_type == "box") {
      p <- p + 
        geom_boxplot(alpha = 0.7) +
        geom_jitter(width = 0.2, alpha = 0.5, size = 2)
    } else if(input$plot_type == "violin") {
      p <- p + 
        geom_violin(alpha = 0.7) +
        geom_jitter(width = 0.2, alpha = 0.5, size = 2)
    } else {
      p <- p + 
        geom_jitter(width = 0.2, alpha = 0.7, size = 3)
    }
    
    ggplotly(p)
  })
  
  # Statistical summary
  output$param_stats <- renderText({
    req(input$clinical_param, input$show_stats)
    
    current_var <- input$clinical_param
    group_var <- get_group_var()
    
    stats_df <- bharat_data %>%
      select(!!sym(current_var), !!sym(group_var)) %>%
      group_by(!!sym(group_var)) %>%
      summarise(
        n = n(),
        mean = mean(!!sym(current_var), na.rm = TRUE),
        sd = sd(!!sym(current_var), na.rm = TRUE),
        median = median(!!sym(current_var), na.rm = TRUE),
        q1 = quantile(!!sym(current_var), 0.25, na.rm = TRUE),
        q3 = quantile(!!sym(current_var), 0.75, na.rm = TRUE)
      )
    
    paste(
      "Summary Statistics:\n\n",
      paste(
        mapply(
          function(g, n, m, s, med, q1, q3) {
            sprintf(
              "%s (n=%d):\nMean ± SD: %.2f ± %.2f\nMedian [Q1, Q3]: %.2f [%.2f, %.2f]\n",
              g, n, m, s, med, q1, q3
            )
          },
          stats_df[[group_var]], 
          stats_df$n,
          stats_df$mean,
          stats_df$sd,
          stats_df$median,
          stats_df$q1,
          stats_df$q3
        ),
        collapse = "\n"
      )
    )
  })
  
  # Cognitive Assessment Tab ------------------------------------------------
  
  # MMSE Distribution Plot
  output$mmse_dist <- renderPlotly({
    mmse_data <- bharat_data %>%
      select(age_group, sex, mmse_total_score) %>%
      drop_na()
    
    p <- ggplot(mmse_data, 
                aes(x = age_group, y = mmse_total_score, 
                    fill = sex)) +
      geom_violin(alpha = 0.7) +
      geom_boxplot(width = 0.2, alpha = 0.7, position = position_dodge(0.9)) +
      scale_fill_manual(values = wes_palette("Zissou1")[c(1,5)]) +
      theme_minimal(base_size = 14) +
      theme(
        text = element_text(family = "Manrope"),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
      ) +
      labs(
        title = "MMSE Score Distribution by Age and Sex",
        x = "Age Group",
        y = "MMSE Total Score",
        fill = "Sex"
      )
    
    ggplotly(p)
  })
  
  # MMSE Domain Analysis
  output$mmse_domains <- renderPlotly({
    domain_data <- bharat_data %>%
      select(orientation_time_score, orientation_place_score,
             registration_score, attention_calculation_score,
             recall_score) %>%
      gather(domain, score) %>%
      mutate(
        domain = str_replace_all(domain, "_score", ""),
        domain = str_replace_all(domain, "_", " "),
        domain = str_to_title(domain)
      ) %>%
      group_by(domain) %>%
      summarise(
        mean_score = mean(score, na.rm = TRUE),
        sd_score = sd(score, na.rm = TRUE)
      )
    
    plot_ly(
      type = 'scatterpolar',
      r = ~mean_score,
      theta = ~domain,
      name = 'Domain Scores',
      fill = 'toself',
      fillcolor = paste0(wes_palette("Darjeeling1")[1], "80"),
      line = list(color = wes_palette("Darjeeling1")[1])
    ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0, max(domain_data$mean_score) * 1.1)
          )
        ),
        showlegend = FALSE,
        title = list(
          text = "MMSE Domain Performance",
          font = list(family = "Manrope", size = 16)
        )
      )
  })
  
  # Physical Assessment Tab ------------------------------------------------
  
  # Frailty Score Distribution
  output$frailty_dist <- renderPlotly({
    frailty_data <- bharat_data %>%
      select(age_group, sex, frailty_total_score) %>%
      drop_na()
    
    p <- ggplot(frailty_data, 
                aes(x = age_group, y = frailty_total_score, 
                    fill = sex)) +
      geom_violin(alpha = 0.7) +
      geom_boxplot(width = 0.2, alpha = 0.7, position = position_dodge(0.9)) +
      scale_fill_manual(values = wes_palette("Royal1")[c(1,4)]) +
      theme_minimal(base_size = 14) +
      theme(
        text = element_text(family = "Manrope"),
        plot.title = element_text(hjust = 0.5)
      ) +
      labs(
        title = "Frailty Score Distribution by Age and Sex",
        x = "Age Group",
        y = "Frailty Score",
        fill = "Sex"
      )
    
    ggplotly(p)
  })
  
  # Frailty Component Analysis
  output$frailty_components <- renderPlotly({
    components <- bharat_data %>%
      select(fatigue_score, resistance_score, 
             ambulation_score, illness_score) %>%
      gather(component, score) %>%
      mutate(
        component = str_replace_all(component, "_score", ""),
        component = str_replace_all(component, "_", " "),
        component = str_to_title(component)
      ) %>%
      group_by(component) %>%
      summarise(
        positive_rate = mean(score == 1, na.rm = TRUE) * 100
      )
    
    p <- ggplot(components, 
                aes(x = reorder(component, positive_rate), 
                    y = positive_rate,
                    fill = component)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = wes_palette("Darjeeling1")) +
      coord_flip() +
      theme_minimal(base_size = 14) +
      theme(
        text = element_text(family = "Manrope"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none"
      ) +
      labs(
        title = "Frailty Components Analysis",
        x = "",
        y = "Positive Response Rate (%)"
      )
    
    ggplotly(p)
  })
  
  # Lifestyle Tab --------------------------------------------------------
  
  # Exercise Patterns
  output$exercise_patterns <- renderPlotly({
    exercise_data <- bharat_data %>%
      select(age_group, exercise_frequency) %>%
      drop_na() %>%
      group_by(age_group, exercise_frequency) %>%
      summarise(count = n(), .groups = "drop")
    
    p <- ggplot(exercise_data,
                aes(x = age_group, y = count, 
                    fill = exercise_frequency)) +
      geom_bar(stat = "identity", position = "fill") +
      scale_fill_manual(values = wes_palette("Zissou1")) +
      theme_minimal(base_size = 14) +
      theme(
        text = element_text(family = "Manrope"),
        plot.title = element_text(hjust = 0.5)
      ) +
      labs(
        title = "Exercise Frequency by Age Group",
        x = "Age Group",
        y = "Proportion",
        fill = "Exercise Frequency"
      )
    
    ggplotly(p)
  })
  
  # Dietary Patterns
  output$dietary_patterns <- renderPlotly({
    diet_data <- bharat_data %>%
      select(contains("frequency")) %>%
      select(-exercise_frequency) %>%
      gather(food_group, frequency) %>%
      mutate(
        food_group = str_replace(food_group, "_frequency", ""),
        food_group = str_replace_all(food_group, "_", " "),
        food_group = str_to_title(food_group)
      ) %>%
      group_by(food_group, frequency) %>%
      summarise(count = n(), .groups = "drop")
    
    p <- ggplot(diet_data,
                aes(x = food_group, y = count, 
                    fill = frequency)) +
      geom_bar(stat = "identity", position = "fill") +
      scale_fill_manual(values = wes_palette("Royal1")) +
      theme_minimal(base_size = 14) +
      theme(
        text = element_text(family = "Manrope"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      labs(
        title = "Dietary Patterns",
        x = "",
        y = "Proportion",
        fill = "Frequency"
      )
    
    ggplotly(p)
  })
}