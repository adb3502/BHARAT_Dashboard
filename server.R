# Replace the age_groups_dist section with:
  output$age_groups_dist <- renderPlotly({
    # Require necessary inputs
    req(length(input$demo_datasets) > 0, length(input$demo_age_groups) > 0)
    
    # Get current datasets from reactive value
    datasets <- integrated_datasets_rv()
    
    # Debug messages
    message("Creating age groups distribution plot")
    message("Selected datasets: ", paste(input$demo_datasets, collapse = ", "))
    
    # Safely combine and process data
    plot_data <- tryCatch({
      combined_data <- bind_rows(!!!lapply(input$demo_datasets, function(ds) {
        message(paste("Processing dataset:", ds))
        if (ds %in% names(datasets)) {
          data <- datasets[[ds]]
          message(paste("Dimensions:", nrow(data), "x", ncol(data)))
          data
        } else {
          message(paste("Dataset not found:", ds))
          NULL
        }
      }))
      
      if (is.null(combined_data) || nrow(combined_data) == 0) {
        return(NULL)
      }
      
      # Filter and prepare data
      combined_data %>%
        filter(.data$age_group %in% input$demo_age_groups) %>%
        mutate(age_group = factor(age_group, 
                               levels = c("18-29", "30-44", "45-59", "60-74", "75+")))
        
    }, error = function(e) {
      message("Error in data processing: ", e$message)
      NULL
    })
    
    # Check if we have valid data
    req(plot_data)
    req(nrow(plot_data) > 0)
    
    message("Final plot data dimensions: ", nrow(plot_data), "x", ncol(plot_data))
    
    # Create plot
    p <- ggplot(plot_data, aes(x = age, fill = sex)) +
      geom_histogram(binwidth = 1, position = "stack", alpha = 0.7) +
      scale_fill_manual(values = c("Male" = "#FFB6C1", "Female" = "#87CEEB")) +
      theme_minimal() +
      labs(
        title = "Age Distribution by Gender",
        x = "Age (years)",
        y = "Count",
        fill = "Gender"
      ) +
      theme(
        text = element_text(family = "Manrope"),
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
    
    ggplotly(p)
  })
  # Age groups distribution
output$age_groups_dist <- renderPlotly({
  # Require necessary inputs
  req(length(input$demo_datasets) > 0, length(input$demo_age_groups) > 0)
    
  # Get current datasets from reactive value
  datasets <- integrated_datasets_rv()
    
  # Debug messages
  message("Creating age groups distribution plot")
  message("Selected datasets: ", paste(input$demo_datasets, collapse = ", "))
    
  # Safely combine and process data
  plot_data <- tryCatch({
    combined_data <- bind_rows(!!!lapply(input$demo_datasets, function(ds) {
      message(paste("Processing dataset:", ds))
      if (ds %in% names(datasets)) {
        data <- datasets[[ds]]
        message(paste("Dimensions:", nrow(data), "x", ncol(data)))
        data
      } else {
        message(paste("Dataset not found:", ds))
        NULL
      }
    }))
      
    if (is.null(combined_data) || nrow(combined_data) == 0) {
      return(NULL)
    }
      
    # Filter and prepare data
    combined_data %>%
      filter(.data$age_group %in% input$demo_age_groups) %>%
      mutate(age_group = factor(age_group, 
                             levels = c("18-29", "30-44", "45-59", "60-74", "75+")))
        
  }, error = function(e) {
    message("Error in data processing: ", e$message)
    NULL
  })
    
  # Check if we have valid data
  req(plot_data)
  req(nrow(plot_data) > 0)
    
  message("Final plot data dimensions: ", nrow(plot_data), "x", ncol(plot_data))
    
  # Create plot
  p <- ggplot(plot_data, aes(x = age_group, fill = sex)) +
    geom_bar(position = "dodge", alpha = 0.7) +
    scale_fill_manual(values = c("Male" = "#FFB6C1", "Female" = "#87CEEB")) +
    theme_minimal() +
    labs(
      title = "Distribution by Age Group and Gender",
      x = "Age Group",
      y = "Count",
      fill = "Gender"
    ) +
    theme(
      text = element_text(family = "Manrope"),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
    
  ggplotly(p)
})

  # Volcano plot and stats table
  output$volcano_plot <- renderPlotly({
    req(length(input$stats_datasets) > 0,
        length(input$stats_age_groups) > 0,
        input$stats_grouping)
    
    # Prepare data
    plot_data <- bind_rows(!!!integrated_datasets[input$stats_datasets]) %>%
      filter(age_group %in% input$stats_age_groups)
    
    if(input$stats_grouping == "custom") {
      plot_data <- apply_custom_group(plot_data, custom_groups())
    }
    
    # Get parameters
    params <- names(plot_data)[!names(plot_data) %in% 
                                c("participant_id", "month", "age", "sex", "age_group", 
                                  "dataset", "group_combined", "custom_group")]
    
    # Calculate volcano plot data
    volcano_data <- calculate_volcano_data(plot_data, params, input$stats_grouping)
    
    # Create volcano plot
    p <- ggplot(volcano_data, 
                aes(x = effect_size, 
                    y = -log10(p_value),
                    color = significant,
                    text = paste("Parameter:", parameter_label,
                               "\nEffect Size:", round(effect_size, 3),
                               "\np-value:", format.pval(p_value, digits = 3)))) +
      geom_point(size = 3) +
      geom_hline(yintercept = -log10(0.05), 
                 linetype = "dashed", 
                 color = "grey50", 
                 alpha = 0.5) +
      scale_color_manual(values = c("TRUE" = "#FF9642", "FALSE" = "#87CEEB")) +
      theme_minimal() +
      labs(
        title = paste("Volcano Plot by", str_to_title(input$stats_grouping)),
        x = "Effect Size",
        y = "-log10(p-value)",
        color = "Significant"
      ) +
      theme(
        text = element_text(family = "Manrope"),
        plot.title = element_text(hjust = 0.5)
      )
    
    ggplotly(p, tooltip = "text")
  })

  # ...existing code...

  # Update stats table
  output$stats_table <- renderDT({
    req(length(input$stats_datasets) > 0,
        length(input$stats_age_groups) > 0,
        input$stats_grouping)
    
    # Prepare data
    plot_data <- bind_rows(!!!integrated_datasets[input$stats_datasets]) %>%
      filter(age_group %in% input$stats_age_groups)
    
    if(input$stats_grouping == "custom") {
      plot_data <- apply_custom_group(plot_data, custom_groups())
    }
    
    # Get parameters
    params <- names(plot_data)[!names(plot_data) %in% 
                                c("participant_id", "month", "age", "sex", "age_group", 
                                  "dataset", "group_combined", "custom_group")]
    
    # Calculate statistics
    stats_data <- calculate_volcano_data(plot_data, params, input$stats_grouping)
    
    # Format for display
    stats_data %>%
      mutate(
        p_value = format.pval(p_value, digits = 3),
        effect_size = round(effect_size, 3),
        fdr_p_value = format.pval(fdr_p_value, digits = 3)
      ) %>%
      datatable(
        options = list(
          pageLength = 15,
          scrollX = TRUE
        ),
        rownames = FALSE
      ) %>%
      formatStyle(
        'significant',
        backgroundColor = styleEqual(c(TRUE, FALSE), c('#90EE90', '#FFB6C6'))
      )
  })
}

