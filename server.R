function(input, output, session) {
  
  # Reactive values for state management
  integration_state <- reactiveValues(
    external_data = NULL,
    mapping = NULL,
    validation_result = NULL,
    processed_data = NULL
  )
  
  # Reactive value for custom groups
  custom_groups <- reactiveVal(list())
  
  # Initialize reactive value for integrated datasets at the top level
  integrated_datasets_rv <- reactiveVal(list(
    "BHARAT" = blood_data_with_demo
  ))

  # Update observers to use the reactive value
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
  
  # Display current custom groups
  output$current_groups <- renderDT({
    groups_df <- data.frame(
      Group = names(custom_groups()),
      Ranges = sapply(custom_groups(), paste, collapse = ", ")
    )
    
    datatable(
      groups_df,
      options = list(
        dom = 't',
        pageLength = -1,
        ordering = FALSE
      ),
      selection = 'single'
    )
  })
  
  # Remove selected custom group
  observeEvent(input$remove_selected_groups, {
    req(input$current_groups_rows_selected)
    
    current_groups <- custom_groups()
    group_to_remove <- names(current_groups)[input$current_groups_rows_selected]
    current_groups[[group_to_remove]] <- NULL
    custom_groups(current_groups)
  })
  
  # Helper function to get the right grouping
  get_group_var <- reactive({
    switch(input$grouping,
           "age" = "age_group",
           "sex" = "sex",
           "combined" = "group_combined",
           "custom" = "custom_group")
  })
  
  # Function to filter and prepare data based on selections
  prepare_plot_data <- reactive({
  req(input$selected_datasets, input$selected_age_groups)
  
  # Print debug info
  print("Selected datasets:")
  print(input$selected_datasets)
  print("Selected age groups:")
  print(input$selected_age_groups)
  
  # Combine selected datasets
  combined_data <- bind_rows(!!!lapply(input$selected_datasets, function(ds) {
    data <- integrated_datasets[[ds]]
    print(paste("Columns in", ds, ":"))
    print(names(data))
    return(data)
  }))
  
  # Filter by selected age groups
  filtered_data <- combined_data %>%
    filter(.data$age_group %in% input$selected_age_groups)
  
  # Apply custom grouping if selected
  if(input$grouping == "custom" && length(custom_groups()) > 0) {
    filtered_data <- apply_custom_group(filtered_data, custom_groups())
  }
  
  return(filtered_data)
})

  # Blood parameter visualization
  output$blood_dist <- renderPlotly({
    req(input$blood_param)
    
    # Get plot data
    plot_df <- prepare_plot_data()
    
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
  
  # Statistical tests and summaries
  output$blood_stats <- renderText({
    req(input$blood_param, input$show_stats)
    
    # Get current variable and plotting data
    current_var <- input$blood_param
    plot_df <- prepare_plot_data()
    group_var <- get_group_var()
    
    # Create stats data
    stats_df <- plot_df %>%
      mutate(
        group = !!sym(group_var),
        value = !!sym(current_var)
      ) %>%
      drop_na()
    
    # Calculate descriptive stats
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

# Integration workflow handlers
  observeEvent(input$preview_data, {
    req(input$external_data, input$dataset_name)
    
    tryCatch({
      # Read and store data
      data <- read_csv(input$external_data$datapath, show_col_types = FALSE)
      integration_state$external_data <- data
      
      # Get column info
      col_info <- lapply(names(data), function(col) {
        list(
          type = class(data[[col]])[1],
          sample = paste(head(data[[col]], 3), collapse = ", ")
        )
      })
      names(col_info) <- names(data)
      
      # Update data info UI
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
      
      # Show data preview
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
        type = "error"
      )
    })
  })

  output$download_template <- downloadHandler(
    filename = function() {
      paste0("mapping_template_", tools::file_path_sans_ext(input$dataset_name), ".csv")
    },
    content = function(file) {
      template <- generate_mapping_template()
      write.csv(template, file, row.names = FALSE, na = "")
    },
    contentType = "text/csv"
  )

  # Process mapping file
  observeEvent(input$mapping_file, {
    req(input$mapping_file)
    
    tryCatch({
      mapping <- read_csv(input$mapping_file$datapath, show_col_types = FALSE)
      integration_state$mapping <- mapping
      
      output$mapping_preview <- renderDT({
        datatable(
          mapping,
          options = list(
            scrollX = TRUE,
            pageLength = 10
          )
        )
      })
      
      output$mapping_status <- renderUI({
        div(
          class = "alert alert-success",
          icon("check-circle"),
          "Mapping file loaded successfully"
        )
      })
      
    }, error = function(e) {
      output$mapping_status <- renderUI({
        div(
          class = "alert alert-danger",
          icon("exclamation-circle"),
          paste("Error loading mapping file:", e$message)
        )
      })
    })
  })

  # Validate integration
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

  # ...existing code...

  # Update age distribution plot to handle errors gracefully
  output$age_distribution <- renderPlotly({
    # Require input validation
    req(input$demo_datasets)
    req(input$demo_age_groups)
    
    # Get current datasets from the reactive value
    datasets <- integrated_datasets_rv()
    
    # Safely combine selected datasets
    plot_data <- tryCatch({
      combined_data <- bind_rows(!!!lapply(input$demo_datasets, function(ds) {
        if (ds %in% names(datasets)) {
          datasets[[ds]]
        } else {
          NULL
        }
      }))
      
      # Filter by selected age groups
      combined_data %>%
        filter(age_group %in% input$demo_age_groups)
    }, error = function(e) {
      # Log error and return NULL
      warning("Error in age distribution plot: ", e$message)
      NULL
    })
    
    # Check if we have valid data
    req(plot_data)
    req(nrow(plot_data) > 0)
    
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

  # ...existing code...

  # Integrate data
  # Update this in server.R

  # Add a reactiveVal to store integrated datasets
  integrated_datasets_rv <- reactiveVal(list(
    "BHARAT" = blood_data_with_demo
  ))

  observeEvent(input$integrate_data, {
    req(integration_state$processed_data,
        integration_state$validation_result$valid,
        input$dataset_name)
    
    withProgress(message = 'Integrating dataset...', value = 0, {
      
      tryCatch({
        # Print initial state
        print("Current integrated datasets:")
        print(names(integrated_datasets_rv()))
        
        # Store data
        new_dataset <- integration_state$processed_data
        print("New dataset dimensions:")
        print(dim(new_dataset))
        
        # Update integrated datasets
        current_datasets <- integrated_datasets_rv()
        current_datasets[[input$dataset_name]] <- new_dataset
        integrated_datasets_rv(current_datasets)
        
        print("Updated integrated datasets:")
        print(names(integrated_datasets_rv()))
        
        # Update UI selections
        choices <- setNames(
          names(integrated_datasets_rv()),
          paste(names(integrated_datasets_rv()), "Data")
        )
        print("Updating choices:")
        print(choices)
        
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
        print("Integration error:")
        print(e$message)
        showNotification(
          paste("Error during integration:", e$message),
          type = "error",
          duration = NULL
        )
      })
    })
  })

  # Update the dataset table using the reactive value
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

  # ...existing code...

  # Update age distribution plot to use the reactive value
  output$age_distribution <- renderPlotly({
    req(input$demo_datasets, input$demo_age_groups)
    
    # Get current datasets
    datasets <- integrated_datasets_rv()
    
    # Print debug info properly
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
        filter(age_group %in% input$demo_age_groups)
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
