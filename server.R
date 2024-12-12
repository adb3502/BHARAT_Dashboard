# Section 1: Core State Management and Setup -------------------------
function(input, output, session) {
  # Initialize reactive values for state management
  integration_state <- reactiveValues(
    external_data = NULL,
    mapping = NULL,
    validation_result = NULL,
    processed_data = NULL
  )
  
  # Reactive value for custom groups
  custom_groups <- reactiveVal(list())
  
  # Initialize reactive value for integrated datasets
  integrated_datasets_rv <- reactiveVal(list(
    "BHARAT" = blood_data_with_demo
  ))
  
  # Helper function to get the right grouping
  get_group_var <- reactive({
    switch(input$grouping,
           "age" = "age_group",
           "sex" = "sex",
           "combined" = "group_combined",
           "custom" = "custom_group")
  })
  
  # Core data preparation function
  prepare_plot_data <- reactive({
    req(input$selected_datasets, input$selected_age_groups)
    
    # Get current datasets
    datasets <- integrated_datasets_rv()
    
    # Debug print
    message("Preparing plot data")
    message("Selected datasets: ", paste(input$selected_datasets, collapse=", "))
    
    # Safely combine datasets
    plot_data <- tryCatch({
      combined_data <- bind_rows(!!!lapply(input$selected_datasets, function(ds) {
        if (ds %in% names(datasets)) {
          data <- datasets[[ds]]
          message(paste("Processing dataset:", ds))
          message(paste("Dimensions:", nrow(data), "x", ncol(data)))
          data
        } else {
          message(paste("Dataset not found:", ds))
          NULL
        }
      }))
      
      if(is.null(combined_data) || nrow(combined_data) == 0) {
        return(NULL)
      }
      
      # Filter and prepare data
      filtered_data <- combined_data %>%
        filter(.data$age_group %in% input$selected_age_groups)
      
      # Apply custom grouping if selected
      if(input$grouping == "custom" && length(custom_groups()) > 0) {
        filtered_data <- apply_custom_group(filtered_data, custom_groups())
      }
      
      filtered_data
      
    }, error = function(e) {
      message("Error in prepare_plot_data: ", e$message)
      NULL
    })
    
    # Debug print final dimensions
    if(!is.null(plot_data)) {
      message(paste("Final data dimensions:", nrow(plot_data), "x", ncol(plot_data)))
    }
    
    return(plot_data)
  })

  # Section 2: UI Observers and Event Handlers -------------------------
  # Update dataset selection inputs
  observe({
    datasets <- integrated_datasets_rv()
    choices <- setNames(
      names(datasets),
      paste(names(datasets), "Data")
    )
    
    updateCheckboxGroupInput(session, "selected_datasets", 
                           choices = choices,
                           selected = names(datasets)[1])
    
    updateCheckboxGroupInput(session, "demo_datasets", 
                           choices = choices,
                           selected = names(datasets)[1])
    
    updateCheckboxGroupInput(session, "stats_datasets", 
                           choices = choices,
                           selected = names(datasets)[1])
  })
  
  # Custom group management
  observeEvent(input$add_group, {
    req(input$group_name, input$group_ranges)
    
    current_groups <- custom_groups()
    current_groups[[input$group_name]] <- input$group_ranges
    custom_groups(current_groups)
    
    # Reset inputs
    updateTextInput(session, "group_name", value = "")
    updateSelectInput(session, "group_ranges", selected = character(0))
  })
  
  observeEvent(input$remove_selected_groups, {
    req(input$current_groups_rows_selected)
    
    current_groups <- custom_groups()
    group_to_remove <- names(current_groups)[input$current_groups_rows_selected]
    current_groups[[group_to_remove]] <- NULL
    custom_groups(current_groups)
  })

# Section 3: Plot Outputs -------------------------
  # Blood parameter visualization
  output$blood_dist <- renderPlotly({
    req(input$blood_param)
    
    # Get plot data
    plot_df <- prepare_plot_data()
    req(plot_df)
    
    # Get current variable and grouping
    current_var <- input$blood_param
    group_var <- get_group_var()
    
    # Create plotting data
    plot_df <- plot_df %>%
      mutate(
        group = !!sym(group_var),
        y_val = !!sym(current_var)
      ) %>%
      drop_na()
    
    # Get reference ranges if available
    ref_range <- blood_metadata %>%
      filter(testname == str_replace_all(current_var, "_", "-")) %>%
      select(testminrangevalue, testmaxrangevalue)
    
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
                        text = paste("ID:", participant_id, 
                                   "\nValue:", round(y_val, 2),
                                   "\nGroup:", group,
                                   "\nColor:", color_group))
    } else {
      plot_df$color_group <- plot_df$group
      n_colors <- length(unique(plot_df$group))
      aes_mapping <- aes(x = group, y = y_val, 
                        color = group,
                        text = paste("ID:", participant_id, 
                                   "\nValue:", round(y_val, 2),
                                   "\nGroup:", group))
    }
    
    # Get colors
    if(input$color_by == "dataset") {
      colors <- dataset_colors[unique(plot_df$dataset)]
    } else {
      colors <- get_color_palette(input$color_scheme, n_colors)
    }
    
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
    
    # Add plot elements based on type with better group handling
    p <- if(input$plot_type == "box") {
      if(input$color_by != "none" && input$color_by != group_var) {
        p + 
          geom_boxplot(
            data = plot_df %>% 
              group_by(group) %>% 
              filter(n() >= 2) %>% 
              ungroup(),
            aes(color = NULL), 
            alpha = 0.3, 
            width = 0.7, 
            outlier.shape = NA
          ) +
          geom_point(position = position_jitter(width = 0.2), alpha = 0.7, size = 3)
      } else {
        p + 
          geom_boxplot(
            data = plot_df %>% 
              group_by(group) %>% 
              filter(n() >= 2) %>% 
              ungroup(),
            alpha = 0.3, 
            width = 0.7, 
            outlier.shape = NA
          ) +
          geom_point(position = position_jitter(width = 0.2), alpha = 0.7, size = 3)
      }
    } else if(input$plot_type == "violin") {
      if(input$color_by != "none" && input$color_by != group_var) {
        p + 
          geom_violin(
            data = plot_df %>% 
              group_by(group) %>% 
              filter(n() >= 2) %>% 
              ungroup(),
            aes(color = NULL), 
            alpha = 0.3, 
            trim = FALSE
          ) +
          geom_point(position = position_jitter(width = 0.2), alpha = 0.7, size = 3)
      } else {
        p + 
          geom_violin(
            data = plot_df %>% 
              group_by(group) %>% 
              filter(n() >= 2) %>% 
              ungroup(),
            alpha = 0.3, 
            trim = FALSE
          ) +
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
    
    # Convert to plotly
    ggplotly(p, tooltip = "text") %>%
      layout(
        font = list(family = "Manrope"),
        hoverlabel = list(font = list(family = "Manrope")),
        showlegend = TRUE,
        margin = list(b = 100)
      ) %>%
      config(displayModeBar = TRUE)
  })
  
  # Age distribution plot
  output$age_distribution <- renderPlotly({
    req(input$demo_datasets, input$demo_age_groups)
    
    # Get current datasets
    datasets <- integrated_datasets_rv()
    
    # Print debug info
    message("Creating age distribution plot")
    message("Selected datasets: ", paste(input$demo_datasets, collapse = ", "))
    
    # Safely combine datasets
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
      
      # Filter by selected age groups
      combined_data %>%
        filter(.data$age_group %in% input$demo_age_groups)
    }, error = function(e) {
      message("Error in age distribution plot: ", e$message)
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

  # Section 4: Integration Workflow -------------------------
  # Data preview handler
  observeEvent(input$preview_data, {
    req(input$external_data, input$dataset_name)
    
    tryCatch({
      # Read and store data with proper error handling
      data <- read_csv(input$external_data$datapath, show_col_types = FALSE)
      integration_state$external_data <- data
      
      # Generate column info
      col_info <- lapply(names(data), function(col) {
        list(
          type = class(data[[col]])[1],
          sample = paste(head(data[[col]], 3), collapse = ", ")
        )
      })
      names(col_info) <- names(data)
      
      # Update info UI
      output$data_info <- renderUI({
        div(
          h4("Dataset Information:"),
          p(paste("Number of rows:", nrow(data))),
          p(paste("Number of columns:", ncol(data))),
          h4("Column Types:"),
          tags$ul(
            lapply(names(col_info), function(col) {
              tags$li(
                strong(col), ": ",
                paste("Type:", col_info[[col]]$type),
                tags$br(),
                "Sample values: ", col_info[[col]]$sample
              )
            })
          )
        )
      })
      
      # Show preview
      output$data_preview <- renderDT({
        datatable(
          head(data, 100),
          options = list(
            scrollX = TRUE,
            pageLength = 5
          )
        )
      })
    }, error = function(e) {
      showNotification(
        paste("Error reading data:", e$message),
        type = "error",
        duration = NULL
      )
    })
  })

  # Mapping file handler
  observeEvent(input$mapping_file, {
    req(input$mapping_file)
    
    tryCatch({
      mapping <- read_csv(input$mapping_file$datapath, show_col_types = FALSE)
      integration_state$mapping <- mapping
      
      output$mapping_preview <- renderDT({
        datatable(mapping, options = list(scrollX = TRUE, pageLength = 10))
      })
      
      output$mapping_status <- renderUI({
        div(class = "alert alert-success",
            icon("check-circle"),
            "Mapping file loaded successfully")
      })
    }, error = function(e) {
      output$mapping_status <- renderUI({
        div(class = "alert alert-danger",
            icon("exclamation-circle"),
            paste("Error loading mapping file:", e$message))
      })
    })
  })

  # Validation handler
  observeEvent(input$validate_integration, {
    req(integration_state$external_data,
        integration_state$mapping,
        input$dataset_name)
    
    tryCatch({
      processed_data <- process_external_data(
        input$external_data$datapath,
        integration_state$mapping,
        input$dataset_name
      )
      
      integration_state$processed_data <- processed_data
      validation <- validate_dataset(processed_data)
      integration_state$validation_result <- validation
      
      output$validation_status <- renderUI({
        div(
          class = if(validation$valid) "alert alert-success" else "alert alert-danger",
          icon(if(validation$valid) "check-circle" else "exclamation-circle"),
          validation$message
        )
      })
    }, error = function(e) {
      output$validation_status <- renderUI({
        div(
          class = "alert alert-danger",
          icon("exclamation-circle"),
          paste("Error processing data:", e$message)
        )
      })
    })
  })

  # Integration handler
  observeEvent(input$integrate_data, {
    req(integration_state$processed_data,
        integration_state$validation_result$valid,
        input$dataset_name)
    
    withProgress(message = 'Integrating dataset...', value = 0, {
      tryCatch({
        message("Current integrated datasets:")
        message(paste(names(integrated_datasets_rv()), collapse = ", "))
        
        # Update datasets
        current_datasets <- integrated_datasets_rv()
        current_datasets[[input$dataset_name]] <- integration_state$processed_data
        integrated_datasets_rv(current_datasets)
        
        # Update UI
        choices <- setNames(
          names(current_datasets),
          paste(names(current_datasets), "Data")
        )
        
        updateCheckboxGroupInput(session, "selected_datasets", 
                               choices = choices,
                               selected = c("BHARAT", input$dataset_name))
        
        updateCheckboxGroupInput(session, "demo_datasets", 
                               choices = choices,
                               selected = c("BHARAT", input$dataset_name))
        
        updateCheckboxGroupInput(session, "stats_datasets", 
                               choices = choices,
                               selected = c("BHARAT", input$dataset_name))
        
        showNotification(
          paste("Successfully integrated dataset:", input$dataset_name),
          type = "message",
          duration = 5
        )
      }, error = function(e) {
        message("Integration error:", e$message)
        showNotification(
          paste("Error during integration:", e$message),
          type = "error",
          duration = NULL
        )
      })
    })
  })

# Section 5: Additional Plot & Table Outputs -------------------------
  # Age groups distribution
  output$age_groups_dist <- renderPlotly({
    req(length(input$demo_datasets) > 0, length(input$demo_age_groups) > 0)
    
    # Get current datasets
    datasets <- integrated_datasets_rv()
    
    plot_data <- tryCatch({
      combined_data <- bind_rows(!!!lapply(input$demo_datasets, function(ds) {
        if (ds %in% names(datasets)) {
          datasets[[ds]]
        } else {
          NULL
        }
      })) %>%
        filter(.data$age_group %in% input$demo_age_groups) %>%
        mutate(age_group = factor(age_group, 
                                 levels = c("18-29", "30-44", "45-59", "60-74", "75+")))
      
      if (is.null(combined_data) || nrow(combined_data) == 0) {
        return(NULL)
      }
      
      combined_data
    }, error = function(e) {
      message("Error in age groups distribution: ", e$message)
      NULL
    })
    
    req(plot_data)
    
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

  # Volcano plot
  output$volcano_plot <- renderPlotly({
    req(length(input$stats_datasets) > 0,
        length(input$stats_age_groups) > 0,
        input$stats_grouping)
    
    # Get current datasets
    datasets <- integrated_datasets_rv()
    
    # Prepare data with proper error handling
    plot_data <- tryCatch({
      combined_data <- bind_rows(!!!lapply(input$stats_datasets, function(ds) {
        if (ds %in% names(datasets)) {
          datasets[[ds]]
        } else {
          NULL
        }
      }))
      
      if(is.null(combined_data) || nrow(combined_data) == 0) {
        return(NULL)
      }
      
      filtered_data <- combined_data %>%
        filter(.data$age_group %in% input$stats_age_groups)
      
      if(input$stats_grouping == "custom") {
        filtered_data <- apply_custom_group(filtered_data, custom_groups())
      }
      
      filtered_data
    }, error = function(e) {
      message("Error in volcano plot data preparation: ", e$message)
      NULL
    })
    
    req(plot_data)
    
    # Get parameters for analysis
    params <- names(plot_data)[!names(plot_data) %in% 
      c("participant_id", "month", "age", "sex", "age_group", 
        "dataset", "group_combined", "custom_group")]
    
    # Calculate volcano plot data
    volcano_data <- calculate_volcano_data(plot_data, params, input$stats_grouping)
    req(volcano_data)
    
    # Create volcano plot with symmetrical layout
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
      # Add symmetric x-axis
      scale_x_continuous(limits = function(x) {
        max_abs <- max(abs(x))
        c(-max_abs, max_abs)
      }) +
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

  # Stats table
  output$stats_table <- renderDT({
    req(length(input$stats_datasets) > 0,
        length(input$stats_age_groups) > 0,
        input$stats_grouping)
    
    # Get current datasets
    datasets <- integrated_datasets_rv()
    
    # Prepare data with proper error handling
    plot_data <- tryCatch({
      combined_data <- bind_rows(!!!lapply(input$stats_datasets, function(ds) {
        if (ds %in% names(datasets)) {
          datasets[[ds]]
        } else {
          NULL
        }
      }))
      
      filtered_data <- combined_data %>%
        filter(.data$age_group %in% input$stats_age_groups)
      
      if(input$stats_grouping == "custom") {
        filtered_data <- apply_custom_group(filtered_data, custom_groups())
      }
      
      filtered_data
    }, error = function(e) {
      message("Error in stats table data preparation: ", e$message)
      NULL
    })
    
    req(plot_data)
    
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

  # Dataset information table
  output$integrated_datasets_table <- renderDT({
    datasets <- integrated_datasets_rv()
    req(length(datasets) > 0)
    
    map_df(names(datasets), ~{
      data <- datasets[[.x]]
      tibble(
        Dataset = .x,
        Rows = nrow(data),
        Columns = ncol(data),
        "Sample Size" = length(unique(data$participant_id)),
        "Age Range" = sprintf("%d-%d", min(data$age), max(data$age)),
        "Integration Date" = as.character(Sys.Date())
      )
    }) %>%
      datatable(options = list(
        pageLength = 5,
        searching = FALSE,
        lengthChange = FALSE
      ))
  })
}