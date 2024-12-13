# server.R

function(input, output, session) {
  # Section 1: Core State Management -------------------------
  
  # Initialize reactive values
  integration_state <- reactiveValues(
    external_data = NULL,
    mapping = NULL,
    validation_result = NULL,
    processed_data = NULL
  )
  
  custom_groups <- reactiveVal(list())
  integrated_datasets_rv <- reactiveVal(loadSavedDatasets())
  
  # PCA state management
  pca_state <- reactiveValues(
    results = NULL,
    selected_pc = NULL,
    selected_params = NULL
  )
  
  # Section 2: Core Helper Functions -------------------------
  
  # Get grouping variable
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
    
    # Only show checkbox when multiple datasets are selected
    if(length(input$selected_datasets) <= 1) {
      return(param_choices)
    }
    
    if(!input$show_common_only) {
      return(param_choices)
    }
    
    # Get current datasets
    datasets <- integrated_datasets_rv()
    
    # Find common parameters
    common_params <- Reduce(intersect, 
      lapply(input$selected_datasets, function(ds) {
        names(datasets[[ds]])
      })
    )
    
    # Filter to blood parameters only
    common_blood_params <- common_params[common_params %in% blood_params]
    
    # Create named vector
    setNames(
      common_blood_params,
      str_to_title(str_replace_all(common_blood_params, "_", " "))
    )
  })
  
  # Data preparation function
  prepare_plot_data <- reactive({
    req(input$selected_datasets, input$selected_age_groups)
    
    datasets <- integrated_datasets_rv()
    
    tryCatch({
      # Combine datasets
      all_data <- lapply(input$selected_datasets, function(ds) {
        if(ds %in% names(datasets)) {
          data <- datasets[[ds]]
          
          # Filter by age groups
          filtered <- data %>%
            filter(age_group %in% input$selected_age_groups)
          
          # Apply custom groups if selected
          if(input$grouping == "custom") {
            filtered <- apply_custom_group(filtered, custom_groups())
          }
          
          return(filtered)
        } else {
          return(NULL)
        }
      })
      
      # Remove NULL entries and combine
      all_data <- all_data[!sapply(all_data, is.null)]
      
      if(length(all_data) == 0) return(NULL)
      
      bind_rows(all_data)
    }, error = function(e) {
      showNotification(paste("Error preparing data:", e$message), type = "error")
      NULL
    })
  })
  
  # Section 3: UI Updates -------------------------
  
  # Show/hide common parameters checkbox
  observe({
    if(length(input$selected_datasets) > 1) {
      shinyjs::show("show_common_only")
    } else {
      shinyjs::hide("show_common_only")
      updateCheckboxInput(session, "show_common_only", value = FALSE)
    }
  })
  
  # Update parameter selection
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
    
    for(input_id in c("selected_datasets", "demo_datasets", "stats_datasets", "pca_datasets")) {
      updateCheckboxGroupInput(session, input_id,
                             choices = choices,
                             selected = if(input_id == "pca_datasets") "BHARAT" else names(datasets)[1])
    }
  })
  
  # Update dataset deletion choices (only in Validation tab)
  observe({
    req(input$integration_tabs == "3")  # Only when in Validation tab
    datasets <- integrated_datasets_rv()
    updateSelectInput(session, "dataset_to_delete",
                     choices = setdiff(names(datasets), "BHARAT"))
  })
  
  # Custom group management
  observeEvent(input$add_group, {
    req(input$group_name, input$group_ranges)
    
    if(input$group_name == "" || length(input$group_ranges) == 0) {
      showNotification("Please provide both group name and ranges", type = "error")
      return()
    }
    
    # Get current groups while maintaining order
    current_groups <- custom_groups()
    current_groups[[input$group_name]] <- input$group_ranges
    custom_groups(current_groups)
    
    # Update UI
    updateTextInput(session, "group_name", value = "")
    updateSelectInput(session, "group_ranges", selected = character(0))
  })
  
  # Update custom groups table
  observe({
    current_groups <- custom_groups()
    
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
                  order = list(0, 'asc')
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
  
  # Template download handler
  output$download_template <- downloadHandler(
    filename = function() {
      paste0("mapping_template_", format(Sys.time(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      write.csv(generate_mapping_template(), file, row.names = FALSE)
    }
  )

  # Section 4: Plot Outputs -------------------------
  
  # Overview plots
  output$total_samples_plot <- renderPlotly({
    datasets <- integrated_datasets_rv()
    
    data <- map_df(names(datasets), ~{
      data <- datasets[[.x]]
      tibble(
        Dataset = .x,
        Samples = n_distinct(data$participant_id)
      )
    })
    
    p <- ggplot(data, aes(x = Dataset, y = Samples, fill = Dataset)) +
      geom_bar(stat = "identity", alpha = 0.8) +
      scale_fill_manual(values = dataset_colors) +
      theme_minimal() +
      theme(
        text = element_text(family = "Manrope"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)
      ) +
      labs(title = "Total Samples per Dataset")
    
    ggplotly(p)
  })
  
  # Blood parameter visualization
  output$blood_dist <- renderPlotly({
    req(input$blood_param)
    
    plot_df <- prepare_plot_data()
    req(plot_df)
    req(input$blood_param %in% names(plot_df))
    
    current_var <- input$blood_param
    group_var <- get_group_var()
    
    # Create plotting data
    plot_df <- plot_df %>%
      mutate(
        group = !!sym(group_var),
        y_val = !!sym(current_var),
        point_color = case_when(
          input$color_by == "dataset" ~ dataset,
          input$color_by == "age" ~ as.character(age),
          input$color_by != "none" ~ as.character(!!sym(input$color_by)),
          TRUE ~ as.character(group)
        )
      ) %>%
      drop_na(group, y_val)
    
    # Get reference ranges
    ref_range <- blood_metadata %>%
      filter(testname == str_replace_all(current_var, "_", "-")) %>%
      select(testminrangevalue, testmaxrangevalue)
    
    # Set up aesthetics
    aes_mapping <- aes(
      x = group, 
      y = y_val,
      color = point_color,
      text = paste(
        "ID:", participant_id,
        "\nValue:", round(y_val, 2),
        "\nGroup:", group,
        "\nDataset:", dataset,
        if(input$color_by == "age") paste("\nAge:", age) else ""
      )
    )
    
    # Get colors
    n_colors <- length(unique(plot_df$point_color))
    colors <- if(input$color_by == "dataset") {
      dataset_colors[unique(plot_df$dataset)]
    } else if(input$color_by == "age") {
      colorRampPalette(c("#FFB6C1", "#4682B4"))(100)
    } else {
      get_color_palette(input$color_scheme, n_colors)
    }
    
    # Create base plot
    p <- ggplot(plot_df, aes_mapping) +
      theme_minimal(base_size = 14) +
      theme(
        text = element_text(family = "Manrope"),
        axis.text = element_text(family = "Manrope", size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)
      ) +
      labs(
        title = str_to_title(str_replace_all(current_var, "_", " ")),
        x = str_to_title(str_replace_all(group_var, "_", " ")),
        y = NULL,
        color = str_to_title(str_replace_all(
          if(input$color_by == "age") "Age" else 
            if(input$color_by != "none") input$color_by else group_var, 
          "_", " "
        ))
      )
    
    # Add appropriate scale
    if(input$color_by == "age") {
      p <- p + scale_color_gradientn(colors = colors)
    } else {
      p <- p + scale_color_manual(values = colors)
    }
    
    # Add plot elements based on type
    p <- switch(input$plot_type,
      "box" = p + 
        {if(input$color_by != "none" && input$color_by != group_var)
          geom_boxplot(aes(color = NULL), alpha = 0.3, width = 0.7, outlier.shape = NA)
        else
          geom_boxplot(alpha = 0.3, width = 0.7, outlier.shape = NA)} +
        geom_jitter(width = 0.2, alpha = 0.7, size = 3),
      
      "violin" = p + 
        {if(input$color_by != "none" && input$color_by != group_var)
          geom_violin(aes(color = NULL), alpha = 0.3, trim = FALSE)
        else
          geom_violin(alpha = 0.3, trim = FALSE)} +
        geom_jitter(width = 0.2, alpha = 0.7, size = 3),
      
      # Default to scatter
      p + geom_jitter(width = 0.2, alpha = 0.7, size = 3)
    )
    
    # Add reference ranges if selected
    if(input$show_ref && nrow(ref_range) > 0) {
      if(!is.na(ref_range$testminrangevalue)) {
        p <- p + geom_hline(yintercept = ref_range$testminrangevalue,
                           linetype = "dashed", color = "grey50", alpha = 0.5)
      }
      if(!is.na(ref_range$testmaxrangevalue)) {
        p <- p + geom_hline(yintercept = ref_range$testmaxrangevalue,
                           linetype = "dashed", color = "grey50", alpha = 0.5)
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
  
  # Demographic plots
  output$age_distribution <- renderPlotly({
    plot_data <- prepare_plot_data()
    req(plot_data)
    
    if(input$grouping == "custom") {
      plot_data <- apply_custom_group(plot_data, custom_groups())
    }
    
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

  # Section 5: PCA Analysis -------------------------
  
  # Perform PCA analysis
  observe({
    req(input$pca_datasets, input$pca_params)
    
    # Get data
    datasets <- integrated_datasets_rv()
    
    # Combine selected datasets
    data <- bind_rows(lapply(input$pca_datasets, function(ds) datasets[[ds]]))
    
    # Compute PCA
    tryCatch({
      pca_results <- compute_pca(data, input$pca_params, scale = TRUE)
      pca_state$results <- pca_results
      
      # Update PC selection choices
      n_pcs <- ncol(pca_results$scores)
      pc_choices <- setNames(
        paste0("PC", 1:n_pcs),
        paste0("PC", 1:n_pcs, " (", round(pca_results$var_explained * 100, 1), "%)")
      )
      updateSelectInput(session, "pc_x", choices = pc_choices, selected = "PC1")
      updateSelectInput(session, "pc_y", choices = pc_choices, selected = "PC2")
      
    }, error = function(e) {
      showNotification(paste("Error in PCA:", e$message), type = "error")
    })
  })
  
  # PCA Score Plot
  output$pca_scores <- renderPlotly({
    req(pca_state$results, input$pc_x, input$pc_y)
    
    # Get data
    scores_df <- as.data.frame(pca_state$results$scores)
    plot_data <- prepare_plot_data()
    req(plot_data)
    
    # Add metadata
    scores_df <- bind_cols(
      scores_df,
      plot_data %>% select(age, sex, age_group, dataset)
    )
    
    # Create score plot
    p <- ggplot(scores_df, 
                aes_string(x = input$pc_x, y = input$pc_y, 
                          color = input$pca_color)) +
      geom_point(alpha = 0.7, size = 3) +
      theme_minimal() +
      labs(
        title = "PCA Score Plot",
        color = str_to_title(input$pca_color)
      ) +
      theme(
        text = element_text(family = "Manrope"),
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
    
    # Add appropriate color scale
    if(input$pca_color == "age") {
      p <- p + scale_color_viridis_c()
    } else if(input$pca_color == "dataset") {
      p <- p + scale_color_manual(values = dataset_colors)
    } else {
      p <- p + scale_color_brewer(palette = "Set1")
    }
    
    ggplotly(p)
  })
  
  # Scree Plot
  output$pca_scree <- renderPlotly({
    req(pca_state$results)
    
    var_explained <- pca_state$results$var_explained
    df <- data.frame(
      PC = factor(paste0("PC", 1:length(var_explained))),
      Variance = var_explained * 100
    )
    
    p <- ggplot(df, aes(x = PC, y = Variance)) +
      geom_bar(stat = "identity", fill = "#4682B4", alpha = 0.7) +
      theme_minimal() +
      labs(
        title = "Scree Plot",
        y = "Variance Explained (%)"
      ) +
      theme(
        text = element_text(family = "Manrope"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    ggplotly(p)
  })
  
  # Loadings Plot
  output$pca_loadings <- renderPlotly({
    req(pca_state$results, input$selected_pc)
    
    # Get top loadings
    loadings <- get_top_loadings(pca_state$results$loadings, 
                                input$selected_pc, 
                                n = input$n_loadings)
    
    p <- ggplot(loadings, aes_string(x = "reorder(parameter, !!input$selected_pc)", 
                                    y = input$selected_pc)) +
      geom_bar(stat = "identity", fill = "#4682B4", alpha = 0.7) +
      coord_flip() +
      theme_minimal() +
      labs(
        title = paste("Top Loadings for", input$selected_pc),
        x = "Parameter",
        y = "Loading Value"
      ) +
      theme(
        text = element_text(family = "Manrope"),
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
    
    ggplotly(p)
  })
  
  # Section 6: Data Integration Workflow -------------------------
  
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
      showNotification(paste("Error reading data:", e$message), type = "error")
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
    
    saveDatasets(current_datasets)
    
    showNotification(paste("Deleted dataset:", input$dataset_to_delete), 
                    type = "message")
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
  
} # End of server function