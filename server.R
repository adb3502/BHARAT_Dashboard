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
