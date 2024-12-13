# server.R

function(input, output, session) {
  # Section 1: Core State Management and Setup -------------------------
  
  # Initialize reactive values
  integration_state <- reactiveValues(
    external_data = NULL,
    mapping = NULL,
    validation_result = NULL,
    processed_data = NULL
  )
  
  custom_groups <- reactiveVal(list())
  integrated_datasets_rv <- reactiveVal(loadSavedDatasets())
  
  # Core helper functions
  get_group_var <- reactive({
    switch(input$grouping,
           "age" = "age_group",
           "sex" = "sex",
           "combined" = "group_combined",
           "custom" = "custom_group")
  })
  
  # Common parameters reactive
  filtered_params <- reactive({
    req(input$selected_datasets)
    
    if(!input$show_common_only || length(input$selected_datasets) <= 1) {
      return(param_choices)
    }
    
    # Get current datasets
    datasets <- integrated_datasets_rv()
    
    # Find common parameters across selected datasets
    common_params <- Reduce(intersect, 
      lapply(input$selected_datasets, function(ds) {
        names(datasets[[ds]])
      })
    )
    
    param_choices[names(param_choices) %in% common_params]
  })
  
  # Data preparation function
  prepare_plot_data <- reactive({
    req(input$selected_datasets, input$selected_age_groups)
    
    datasets <- integrated_datasets_rv()
    
    message("Preparing plot data")
    message("Selected datasets:", paste(input$selected_datasets, collapse=", "))
    
    plot_data <- tryCatch({
      all_data <- lapply(input$selected_datasets, function(ds) {
        if (ds %in% names(datasets)) {
          data <- datasets[[ds]]
          message(paste("Processing dataset:", ds))
          message(paste("Dimensions before filtering:", nrow(data), "x", ncol(data)))
          
          filtered <- data %>%
            filter(.data$age_group %in% input$selected_age_groups)
          
          message(paste("Dimensions after filtering:", nrow(filtered), "x", ncol(filtered)))
          return(filtered)
        } else {
          message(paste("Dataset not found:", ds))
          return(NULL)
        }
      })
      
      all_data <- all_data[!sapply(all_data, is.null)]
      
      if(length(all_data) == 0) {
        message("No valid datasets to combine")
        return(NULL)
      }
      
      combined_data <- bind_rows(all_data)
      
      message("Final combined dimensions:", nrow(combined_data), "x", ncol(combined_data))
      message("Columns in combined data:", paste(names(combined_data), collapse=", "))
      
      return(combined_data)
      
    }, error = function(e) {
      message("Error in prepare_plot_data: ", e$message)
      return(NULL)
    })
    
    return(plot_data)
  })

  # Section 2: Core UI Updates -------------------------
  
  # Update parameter selection based on common parameters filter
  observe({
    updateSelectizeInput(session, "blood_param",
                        choices = filtered_params())
  })
  
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
    
    # Update dataset deletion choices
    updateSelectInput(session, "dataset_to_delete",
                     choices = setdiff(names(datasets), "BHARAT"))
  })
  
  # Template download handler
  output$download_template <- downloadHandler(
    filename = function() {
      paste0("mapping_template_", format(Sys.time(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      write.csv(generate_mapping_template(), file, row.names = FALSE)
    }
  )

  # Custom group management
  observeEvent(input$add_group, {
    req(input$group_name, input$group_ranges)
    
    if(input$group_name == "" || length(input$group_ranges) == 0) {
      showNotification("Please provide both group name and ranges", type = "error")
      return()
    }
    
    # Get current groups while maintaining order
    current_groups <- custom_groups()
    
    # Add new group while preserving order
    current_groups[[input$group_name]] <- input$group_ranges
    custom_groups(current_groups)
    
    # Update UI
    updateTextInput(session, "group_name", value = "")
    updateSelectInput(session, "group_ranges", selected = character(0))
    
    # Update table
    output$current_groups <- renderDT({
      groups_df <- data.frame(
        Group = names(current_groups),
        Ranges = sapply(current_groups, paste, collapse = ", "),
        stringsAsFactors = FALSE
      )
      datatable(groups_df, 
                selection = 'single',
                options = list(
                  pageLength = 5,
                  lengthChange = FALSE,
                  searching = FALSE,
                  order = list(0, 'asc')  # Order by first column ascending
                ))
    })
  })
  
  # Remove selected groups handler
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
    req(input$blood_param %in% names(plot_df))
    
    # Get current variable and grouping
    current_var <- input$blood_param
    group_var <- get_group_var()
    
    # Create plotting data with dataset tracking and proper group ordering
    plot_df <- plot_df %>%
      mutate(
        group = !!sym(group_var),
        y_val = !!sym(current_var),
        point_color = if(input$color_by == "dataset") dataset else if(input$color_by != "none") !!sym(input$color_by) else group
      ) %>%
      drop_na(group, y_val)
    
    # If using custom groups, set the factor levels based on order of creation
    if(input$grouping == "custom" && !is.null(custom_groups())) {
      plot_df$group <- factor(plot_df$group, levels = names(custom_groups()))
    }
    
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
    aes_mapping <- aes(
      x = group, 
      y = y_val,
      color = point_color,
      text = paste(
        "ID:", participant_id,
        "\nValue:", round(y_val, 2),
        "\nGroup:", group,
        "\nDataset:", dataset
      )
    )
    
    # Get colors based on grouping
    n_colors <- length(unique(plot_df$point_color))
    colors <- if(input$color_by == "dataset") {
      dataset_colors[unique(plot_df$dataset)]
    } else {
      get_color_palette(input$color_scheme, n_colors)
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
        y = NULL,  # Remove y-axis label
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
    
    # Safely combine datasets
    plot_data <- tryCatch({
      combined_data <- bind_rows(!!!lapply(input$demo_datasets, function(ds) {
        if (ds %in% names(datasets)) {
          datasets[[ds]]
        } else {
          NULL
        }
      })) %>%
        filter(.data$age_group %in% input$demo_age_groups)
      
      if (is.null(combined_data) || nrow(combined_data) == 0) {
        return(NULL)
      }
      
      combined_data
    }, error = function(e) {
      message("Error in age distribution plot: ", e$message)
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
  
  # Age groups distribution
  output$age_groups_dist <- renderPlotly({
    req(length(input$demo_datasets) > 0, length(input$demo_age_groups) > 0)
    
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
  
  # Volcano plot comparison observer
  observe({
    req(input$stats_grouping)
    
    # Get the grouped data
    plot_data <- prepare_plot_data()
    req(plot_data)
    
    # Get groups based on selected grouping
    groups <- switch(input$stats_grouping,
      "age" = input$stats_age_groups,
      "sex" = c("Male", "Female"),
      "combined" = unique(plot_data$group_combined),
      "custom" = names(custom_groups())
    )
    
    # Generate pairwise combinations
    if(length(groups) >= 2) {
      comparisons <- utils::combn(groups, 2, paste, collapse=" vs ")
      updateSelectInput(session, "volcano_comparison",
                       choices = comparisons)
    }
  })

  # Section 4: Data Integration Workflow -------------------------
  
  # Data preview handler
  observeEvent(input$preview_data, {
    req(input$external_data, input$dataset_name)
    
    tryCatch({
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
  
  # Dataset deletion handler
  observeEvent(input$delete_dataset, {
    req(input$dataset_to_delete)
    
    if(input$dataset_to_delete == "BHARAT") {
      showNotification("Cannot delete the BHARAT dataset", type = "error")
      return()
    }
    
    current_datasets <- integrated_datasets_rv()
    current_datasets[[input$dataset_to_delete]] <- NULL
    integrated_datasets_rv(current_datasets)
    
    # Save updated datasets
    saveDatasets(current_datasets)
    
    # Update UI
    updateSelectInput(session, "dataset_to_delete",
                     choices = setdiff(names(current_datasets), "BHARAT"))
    
    showNotification(paste("Deleted dataset:", input$dataset_to_delete), 
                    type = "message")
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
        current_datasets <- integrated_datasets_rv()
        current_datasets[[input$dataset_name]] <- integration_state$processed_data
        integrated_datasets_rv(current_datasets)
        
        # Save datasets to persistent storage
        saveDatasets(current_datasets)
        
        showNotification(
          paste("Successfully integrated dataset:", input$dataset_name),
          type = "message"
        )
      }, error = function(e) {
        showNotification(
          paste("Error during integration:", e$message),
          type = "error",
          duration = NULL
        )
      })
    })
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