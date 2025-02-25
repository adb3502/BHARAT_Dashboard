# server.R

library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(shinyBS)
library(shinyjs)
library(dplyr)
library(readr)
library(stringr)
library(effsize)  # For Cohen's d

# Define any necessary helper functions and variables here
# For example:
# - loadSavedDatasets()
# - saveDatasets()
# - apply_custom_group()
# - generate_mapping_template()
# - process_external_data()
# - validate_dataset()
# - param_choices
# - blood_params
# - standard_age_groups
# - grouping_choices
# - color_by_choices
# - color_schemes
# - dataset_colors
# - blood_metadata

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
  
  # Group comparison state
  group_comparison <- reactiveValues(
    group1 = NULL,
    group2 = NULL
  )
  
  # Section 2: Core Helper Functions -------------------------
  
  # Get grouping variable
  get_group_var <- reactive({
    switch(input$grouping,
           "age" = "age_group",
           "sex" = "sex",
           "age_sex" = "group_combined",
           "custom_age" = "custom_group",
           "custom_age_sex" = "custom_group_combined")
  })
  
  # Common parameters reactive
  filtered_params <- reactive({
    req(input$selected_datasets)
    
    # Only filter if multiple datasets and checkbox is checked
    if(length(input$selected_datasets) <= 1 || !input$show_common_only) {
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
          if(input$grouping %in% c("custom_age", "custom_age_sex")) {
            current_groups <- custom_groups()
            if(length(current_groups) > 0) {
              filtered <- apply_custom_group(filtered, current_groups)
              
              # Add combined grouping for custom groups and sex if needed, but only if custom_group exists
              if(input$grouping == "custom_age_sex" && "custom_group" %in% names(filtered)) {
                filtered <- filtered %>%
                  mutate(custom_group_combined = paste(custom_group, sex))
              } else if(input$grouping == "custom_age_sex") {
                # If custom_group doesn't exist, log a warning and revert to a simpler grouping
                warning("Custom groups not properly applied to the data.")
                updateSelectInput(session, "grouping", selected = "age")
              }
            } else {
              # If no custom groups exist, revert to a simpler grouping
              updateSelectInput(session, "grouping", selected = "age")
              showNotification("No custom groups defined. Reverting to standard age groups.", type = "warning")
            }
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
  
  # Update color_by choices when custom groups are modified
  observe({
    current_groups <- custom_groups()
    color_choices <- if(length(current_groups) > 0) {
      c(
        "None" = "none",
        "Age Groups" = "age_group",
        "Custom Age Groups" = "custom_age_group",
        "Sex" = "sex",
        "Dataset" = "dataset",
        "Age (continuous)" = "age"
      )
    } else {
      c(
        "None" = "none",
        "Age Groups" = "age_group",
        "Sex" = "sex",
        "Dataset" = "dataset",
        "Age (continuous)" = "age"
      )
    }
    
    updateSelectInput(session, "color_by",
                     choices = color_choices,
                     selected = if(input$color_by == "custom_age_group" && length(current_groups) == 0) 
                                "dataset" else input$color_by)
  })

  # Show/hide common parameters checkbox
  observe({
    if(length(input$selected_datasets) > 1) {
      shinyjs::show("common_params_div")
    } else {
      shinyjs::hide("common_params_div")
      updateCheckboxInput(session, "show_common_only", value = FALSE)
    }
  })
  
  # Update parameter selection
  observe({
    updateSelectizeInput(session, "blood_param",
                        choices = filtered_params(),
                        server = TRUE)
  })
  
  # Update dataset selection inputs
  observe({
    datasets <- integrated_datasets_rv()
    choices <- setNames(
      names(datasets),
      paste(names(datasets), "Data")
    )
    
    input_ids <- c("selected_datasets", "demo_datasets", 
                   "stats_datasets", "pca_datasets")
    
    for(id in input_ids) {
      updateCheckboxGroupInput(session, id,
                             choices = choices,
                             selected = if(id == "pca_datasets") "BHARAT" 
                                      else if(id == "selected_datasets") names(datasets)[1]
                                      else "BHARAT")
    }
  })
  
  # Group selection updates for volcano plot
  observe({
    req(input$stats_grouping)
    
    plot_data_local <- prepare_plot_data()
    req(plot_data_local)
    
    choices <- switch(input$stats_grouping,
      "age" = as.character(plot_data_local$age_group),
      "sex" = as.character(plot_data_local$sex),
      "combined" = unique(plot_data_local$group_combined),
      "custom" = names(custom_groups()),
      NULL
    )
    
    if(!is.null(choices)) {
      choices <- sort(unique(choices))
      updateSelectInput(session, "group1_select", choices = choices)
      updateSelectInput(session, "group2_select", choices = choices)
    }
  })
  
  # Section 4: Custom Groups Management -------------------------
  
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
      if(length(current_groups) == 0) {
        return(datatable(data.frame(Group = character(0), Ranges = character(0)),
                         options = list(dom = 't'),
                         rownames = FALSE))
      }
      
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
                  ordering = FALSE,
                  paging = FALSE
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
  
  # Handle custom group modal triggers
  observeEvent(input$show_group_modal, {
    toggleModal(session, "groupModal", toggle = "show")
  })
  
  observeEvent(input$show_group_modal_demo, {
    toggleModal(session, "groupModal", toggle = "show")
  })
  
  observeEvent(input$show_group_modal_stats, {
    toggleModal(session, "groupModal", toggle = "show")
  })

  # Synchronize grouping and color selection for custom groups
  observeEvent(input$grouping, {
    if(input$grouping %in% c("custom_age", "custom_age_sex")) {
      # Check if we have custom groups
      current_groups <- custom_groups()
      if(length(current_groups) == 0) {
        showNotification("Please create custom age groups first", type = "warning")
        updateSelectInput(session, "grouping", selected = "age")
      }
    }
  })

  # Update color choices when grouping changes
  observeEvent(input$grouping, {
    if(input$grouping %in% c("custom_age", "custom_age_sex")) {
      updateSelectInput(session, "color_by",
                       selected = "custom_age_group")
    } else if(input$color_by == "custom_age_group") {
      updateSelectInput(session, "color_by",
                       selected = "dataset")
    }
  })

  # Update grouping when custom groups are added or removed
  observeEvent(custom_groups(), {
    current_groups <- custom_groups()
    if(length(current_groups) == 0 && input$grouping %in% c("custom_age", "custom_age_sex")) {
      updateSelectInput(session, "grouping", selected = "age")
    }
  })

  # Section 5: Overview Tab Outputs -------------------------
  
  # Overview statistics
  output$dataset_stats <- renderUI({
    datasets <- integrated_datasets_rv()
    
    if(length(datasets) == 0) {
      return(HTML("<p>No datasets integrated yet.</p>"))
    }
    
    total_samples <- sum(sapply(datasets, function(x) n_distinct(x$participant_id)))
    total_params <- length(blood_params)
    
    # Age range across all datasets
    all_ages <- do.call(c, lapply(datasets, function(x) x$age))
    age_range <- sprintf("%d-%d", floor(min(all_ages, na.rm = TRUE)), ceiling(max(all_ages, na.rm = TRUE)))
    
    # Gender distribution
    all_sex <- do.call(c, lapply(datasets, function(x) x$sex))
    sex_dist <- table(all_sex)
    
    tagList(
      div(class = "info-box",
          h4("Dataset Summary"),
          p(sprintf("Total Samples: %d", total_samples)),
          p(sprintf("Parameters: %d", total_params)),
          p(sprintf("Age Range: %s years", age_range)),
          p(sprintf("Gender Distribution: %d Male, %d Female", 
                   ifelse("Male" %in% names(sex_dist), sex_dist["Male"], 0),
                   ifelse("Female" %in% names(sex_dist), sex_dist["Female"], 0)))
      )
    )
  })
  
  # Total samples plot
  output$total_samples_plot <- renderPlotly({
    datasets <- integrated_datasets_rv()
    
    if(length(datasets) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = "No datasets available for plotting."))
    }
    
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
      labs(title = "Total Samples per Dataset",
           x = "Dataset",
           y = "Number of Samples",
           fill = "Dataset")
    
    ggplotly(p)
  })
  

# Dataset overview table
output$overview_datasets_table <- renderDT({
  datasets <- integrated_datasets_rv()
  
  if(length(datasets) == 0) {
    return(datatable(data.frame(Message = "No datasets integrated yet."),
                     options = list(dom = 't'),
                     rownames = FALSE))
  }
  
  # Create summary table
  summary_table <- map_df(names(datasets), ~{
    data <- datasets[[.x]]
    tibble(
      Dataset = .x,
      `Total Samples` = n_distinct(data$participant_id),
      `Age Range` = sprintf("%d-%d", floor(min(data$age, na.rm = TRUE)), ceiling(max(data$age, na.rm = TRUE))),
      `Male` = sum(data$sex == "Male", na.rm = TRUE),
      `Female` = sum(data$sex == "Female", na.rm = TRUE),
      Parameters = ncol(data) - 6  # Assuming 6 metadata columns
    )
  })
  
  # Compute the maximum counts for 'Male' and 'Female' across all datasets
  max_male <- max(summary_table$Male, na.rm = TRUE)
  max_female <- max(summary_table$Female, na.rm = TRUE)
  
  # Render the datatable with formatting
  datatable(summary_table, options = list(
    pageLength = 10,
    scrollX = TRUE,
    dom = 'tp'
  )) %>%
    formatStyle(
      columns = c("Male", "Female"),
      backgroundColor = styleColorBar(c(0, max(c(max_male, max_female))), 'lightblue'),
      background = "white",
      color = 'black'
    )
})

  
  # Section 6: Blood Parameter Visualization -------------------------


# Reactive expression to prepare plot and stats data
plot_data <- reactive({
  req(input$blood_param, input$selected_datasets, input$selected_age_groups)
  prepare_plot_data()
})

# Add validation for custom groups
validate_custom_groups <- reactive({
  if(input$color_by == "custom_age_group") {
    current_groups <- custom_groups()
    if(length(current_groups) == 0) {
      showNotification("No custom age groups defined. Please create custom groups first.", 
                      type = "warning")
      updateSelectInput(session, "color_by", selected = "dataset")
      return(FALSE)
    }
    
    data <- plot_data()
    if(!("custom_group" %in% names(data))) {
      showNotification("Custom groups not properly applied to the data. Please try creating the groups again.", 
                      type = "warning")
      updateSelectInput(session, "color_by", selected = "dataset")
      return(FALSE)
    }
  }
  return(TRUE)
})

# Render the Blood Parameters plot
# Render the Blood Parameters plot
output$blood_dist <- renderPlotly({
  req(plot_data(), input$color_by)
  
  # Get the data
  data <- plot_data()
  
  # Check if the selected parameter exists
  if(!input$blood_param %in% names(data)) {
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = "Selected parameter not available in current dataset selection."))
  }
  
  # Determine grouping variable
  group_var <- get_group_var()
  
  # Ensure grouping variable exists
  if(!group_var %in% names(data)) {
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = "Grouping variable not found in the data."))
  }
  
  # Convert selected parameter to numeric and create initial grouping
  data <- data %>% 
    mutate(
      y_val = as.numeric(!!sym(input$blood_param)),
      group = factor(!!sym(group_var))
    )
  
  # Check if conversion was successful
  if(all(is.na(data$y_val))) {
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = "Selected parameter has no numeric data."))
  }

  # Now handle coloring separately using base R to avoid NSE issues
  data$point_color <- as.character(data$group)  # Default coloring

  # Only attempt to use a column for coloring if it actually exists in the data
  if (input$color_by == "dataset") {
    data$point_color <- as.character(data$dataset)
  } else if (input$color_by == "age_group") {
    data$point_color <- as.character(data$age_group)
  } else if (input$color_by == "sex") {
    data$point_color <- as.character(data$sex)
  } else if (input$color_by == "age") {
    data$point_color <- as.character(data$age)
  } else if (input$color_by == "custom_age_group") {
    if ("custom_group" %in% names(data)) {
      data$point_color <- as.character(data$custom_group)
    } else {
      # Fallback if custom_group doesn't exist
      data$point_color <- as.character(data$dataset)
      showNotification("Custom groups not available. Using dataset colors instead.", type = "warning")
    }
  }
  
  # Remove rows with missing values in essential columns
  data <- data %>%
    drop_na(group, y_val, point_color)
  
  # Check if we have any data after filtering
  if(nrow(data) == 0) {
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = "No data available after filtering."))
  }

  # Define color palette based on user selection
  if(input$color_by == "age") {
    # For continuous age, use gradient
    colors <- colorRampPalette(c("#FFB6C1", "#4682B4"))(100)
  } else if(input$color_by == "dataset") {
    # For datasets, use predefined colors
    colors <- dataset_colors[levels(as.factor(data$point_color))]
  } else {
    # For categorical variables, use selected color scheme
    n_colors <- length(levels(as.factor(data$point_color)))
    colors <- get_color_palette(input$color_scheme, n_colors)
    names(colors) <- levels(as.factor(data$point_color))
  }
  
  # Define tooltip content
  data <- data %>% mutate(
    tooltip = paste(
      "ID:", participant_id,
      "\nValue:", round(y_val, 2),
      "\nGroup:", group,
      "\nDataset:", dataset,
      if(input$color_by == "age") paste("\nAge:", age) else ""
    )
  )
  
  # Create the base plot
  p <- ggplot(data, aes(x = group, y = y_val, text = tooltip)) +
    theme_minimal(base_size = 14) +
    theme(
      text = element_text(family = "sans-serif"),
      axis.text = element_text(size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank(),
      legend.position = "right",
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10)
    ) +
    labs(
      title = str_to_title(str_replace_all(input$blood_param, "_", " ")),
      x = str_to_title(str_replace_all(group_var, "_", " ")),
      y = NULL,
      color = str_to_title(
        if (input$color_by == "age") "Age" else 
          if (input$color_by != "none") str_replace_all(input$color_by, "_", " ") else 
            str_replace_all(group_var, "_", " ")
      )
    )
  
  # Add color aesthetics based on the type of color variable
  if(input$color_by == "age") {
    p <- p + aes(color = point_color) + 
      scale_color_gradientn(colors = colors)
  } else if(input$color_by != "none") {
    p <- p + aes(color = point_color) + 
      scale_color_manual(values = colors)
  }
  
  # Add plot type specific layers
  p <- switch(input$plot_type,
    "box" = p + 
      geom_boxplot(alpha = 0.3, width = 0.7, outlier.shape = NA) +
      geom_jitter(width = 0.2, alpha = 0.7, size = 3),
    "violin" = p + 
      geom_violin(alpha = 0.3, trim = FALSE) +
      geom_jitter(width = 0.2, alpha = 0.7, size = 3),
    # Default to scatter plot
    p + geom_jitter(width = 0.2, alpha = 0.7, size = 3)
  )
  
  # Add reference lines if requested and available
  ref_range <- blood_metadata %>%
    filter(testname == str_replace_all(input$blood_param, "_", "-")) %>%
    select(testminrangevalue, testmaxrangevalue)
  
  if (input$show_ref && nrow(ref_range) > 0) {
    if (!is.na(ref_range$testminrangevalue)) {
      p <- p + geom_hline(yintercept = ref_range$testminrangevalue,
                          linetype = "dashed", color = "grey50", alpha = 0.5)
    }
    if (!is.na(ref_range$testmaxrangevalue)) {
      p <- p + geom_hline(yintercept = ref_range$testmaxrangevalue,
                          linetype = "dashed", color = "grey50", alpha = 0.5)
    }
  }
  
  # Convert ggplot to plotly
  ggplotly(p, tooltip = "text") %>%
    layout(
      font = list(family = "sans-serif"),
      hoverlabel = list(font = list(family = "sans-serif")),
      showlegend = TRUE,
      margin = list(b = 100, t = 50, r = 100, l = 80)
    ) %>%
    config(displayModeBar = TRUE)
})

# Render statistics below the Blood Parameters plot
output$stats_info <- renderUI({
  # Check if statistics should be shown
  req(input$show_stats)
  
  # Get the data used for plotting
  data <- plot_data()
  
  # Check if data is available
  req(data)
  
  # Determine the grouping variable
  group_var <- get_group_var()
  
  # Verify that the grouping variable exists
  if(!group_var %in% names(data)) {
    return(HTML("<p>Grouping variable not found in the data.</p>"))
  }
  
  # Ensure the grouping variable is a factor
  data <- data %>%
    mutate(group = factor(!!sym(group_var)))
  
  # Compute basic statistics: count, mean, median, SD per group
  stats <- data %>%
    group_by(group) %>%
    summarise(
      count = n(),
      mean = mean(y_val, na.rm = TRUE),
      median = median(y_val, na.rm = TRUE),
      sd = sd(y_val, na.rm = TRUE)
    )
  
  # Perform ANOVA if there are more than one group
  if(n_distinct(data$group) > 1) {
    aov_result <- tryCatch({
      aov_formula <- as.formula(paste("y_val ~ group"))
      aov(aov_formula, data = data)
    }, error = function(e) {
      return(NULL)
    })
    
    if(!is.null(aov_result)) {
      anova_summary <- summary(aov_result)
      p_value <- anova_summary[[1]]["group", "Pr(>F)"]
      aov_text <- sprintf("ANOVA: F(%d, %d) = %.2f, p = %.3e",
                          anova_summary[[1]]["group", "Df"],
                          anova_summary[[1]]["Residuals", "Df"],
                          anova_summary[[1]]["group", "F value"],
                          anova_summary[[1]]["group", "Pr(>F)"])
    } else {
      aov_text <- "ANOVA could not be performed."
    }
  } else {
    aov_text <- "Only one group present. ANOVA not applicable."
  }
  
  # Create HTML content
  html_content <- paste(
    "<h4>Basic Statistics:</h4>",
    renderTable(stats, rownames = FALSE, striped = TRUE, bordered = TRUE, hover = TRUE),
    "<h4>ANOVA Result:</h4>",
    aov_text,
    sep = "<br>"
  )
  
  HTML(html_content)
})

  
  # Section 7: Demographic Plots -------------------------
  
  # Demographics data preparation
  demographics_data <- reactive({
    req(input$demo_datasets)
    
    # Get current datasets
    datasets <- integrated_datasets_rv()
    selected_data <- datasets[input$demo_datasets]
    
    # Combine selected datasets
    combined_data <- bind_rows(selected_data) %>%
      mutate(
        age = as.numeric(age),
        age_group = factor(age_group, levels = c("18-29", "30-44", "45-59", "60-74", "75+")),
        sex = factor(sex, levels = c("Male", "Female"))
      )
    
    # Filter by age groups if selected
    if(!is.null(input$demo_age_groups) && length(input$demo_age_groups) > 0) {
      combined_data <- combined_data %>% 
        filter(age_group %in% input$demo_age_groups)
    }
    
    combined_data
  })
  
  # Age distribution plot
  output$age_distribution <- renderPlotly({
    plot_data_demo <- demographics_data()
    req(plot_data_demo)
    
    # Check if we have any data
    if(nrow(plot_data_demo) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = "No data available for the current selection."))
    }
    
    # Create histogram
    p <- ggplot(plot_data_demo, aes(x = age, fill = sex)) +
      geom_histogram(binwidth = 1, position = "stack", alpha = 0.7,
                     aes(text = paste(
                       "Age:", age,
                       "\nSex:", sex,
                       "\nCount:", after_stat(count)
                     ))) +
      scale_fill_manual(values = c("Male" = "#FFB6C1", "Female" = "#87CEEB")) +
      theme_minimal() +
      theme(
        text = element_text(family = "sans-serif"),
        plot.title = element_text(hjust = 0.5, face = "bold")
      ) +
      labs(
        title = "Age Distribution by Gender",
        x = "Age (years)",
        y = "Count",
        fill = "Gender"
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        showlegend = TRUE,
        legend = list(x = 1.05, y = 0.5),
        margin = list(r = 100)
      )
  })
  
  # Age groups distribution plot
  output$age_groups_dist <- renderPlotly({
    plot_data_demo <- demographics_data()
    req(plot_data_demo)
    
    # Check if we have any data
    if(nrow(plot_data_demo) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = "No data available for the current selection."))
    }
    
    # Create bar plot
    p <- ggplot(plot_data_demo, aes(x = age_group, fill = sex)) +
      geom_bar(position = "dodge", alpha = 0.7,
               aes(text = paste(
                 "Age Group:", age_group,
                 "\nSex:", sex,
                 "\nCount:", after_stat(count)
               ))) +
      scale_fill_manual(values = c("Male" = "#FFB6C1", "Female" = "#87CEEB")) +
      theme_minimal() +
      theme(
        text = element_text(family = "sans-serif"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      labs(
        title = "Distribution by Age Group and Gender",
        x = "Age Group",
        y = "Count",
        fill = "Gender"
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        showlegend = TRUE,
        legend = list(x = 1.05, y = 0.5),
        margin = list(r = 100)
      )
  })
  
  # Section 8: PCA Analysis -------------------------
  
  # Perform PCA analysis
  observeEvent(input$pca_datasets, {
    req(input$pca_datasets)
    
    # Get data
    datasets <- integrated_datasets_rv()
    selected_data <- datasets[input$pca_datasets]
    
    # Find common numeric parameters across all selected datasets
    common_params <- Reduce(intersect, 
                          lapply(selected_data, function(x) {
                            names(x)[sapply(x, is.numeric)]
                          }))
    param_cols <- setdiff(common_params, c("age", "month"))
    
    # Show number of parameters
    showNotification(
      sprintf("Found %d common parameters across datasets", length(param_cols)),
      type = "message"
    )
    
    # Combine datasets and prepare data
    combined_data <- bind_rows(lapply(names(selected_data), function(ds) {
      data <- selected_data[[ds]]
      data$dataset <- ds  # Add dataset identifier
      data
    }))
    
    # Update color by choices with parameters
    updateSelectInput(session, "pca_color",
                     choices = c(
                       "None" = "none",
                       "Age Groups" = "age_group",
                       "Sex" = "sex",
                       "Dataset" = "dataset",
                       "Age" = "age",
                       setNames(param_cols, str_to_title(str_replace_all(param_cols, "_", " ")))
                     ))
    
    # Compute PCA
    tryCatch({
      # Remove rows with any NA in parameter columns
      complete_data <- combined_data %>%
        drop_na(all_of(param_cols))
      
      if(nrow(complete_data) == 0) {
        showNotification("No complete cases found for PCA analysis", type = "error")
        return(NULL)
      }
      
      # Store metadata separately
      metadata <- complete_data %>%
        select(participant_id, dataset, age, sex, age_group)
      
      # Perform PCA on parameter columns only
      pca_data <- complete_data %>%
        select(all_of(param_cols)) %>%
        scale()  # Scale the data before PCA
      
      # Compute PCA
      pca_results <- prcomp(pca_data, scale. = FALSE)  # Already scaled
      
      # Format results
      formatted_results <- list(
        scores = pca_results$x,
        loadings = pca_results$rotation,
        var_explained = pca_results$sdev^2 / sum(pca_results$sdev^2),
        pca_obj = pca_results
      )
      
      # Store results and metadata
      pca_state$results <- formatted_results
      pca_state$metadata <- metadata
      pca_state$common_params <- param_cols
      
      # Update PC selection choices
      n_pcs <- ncol(formatted_results$scores)
      pc_choices <- setNames(
        paste0("PC", 1:n_pcs),
        paste0("PC", 1:n_pcs, " (", round(formatted_results$var_explained * 100, 1), "%)")
      )
      updateSelectInput(session, "pc_x", choices = pc_choices, selected = "PC1")
      updateSelectInput(session, "pc_y", choices = pc_choices, selected = "PC2")
      updateSelectInput(session, "selected_pc", choices = pc_choices, selected = "PC1")
      
      # Show success message with details
      showNotification(sprintf(
        "PCA computed successfully with %d samples (%s) using %d common parameters", 
        nrow(complete_data),
        paste(sapply(unique(metadata$dataset), function(ds) {
          sprintf("%s: %d", ds, sum(metadata$dataset == ds))
        }), collapse = ", "),
        length(param_cols)
      ), type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error in PCA:", e$message), type = "error")
    })
  }, ignoreInit = FALSE)
  
  # PCA Score Plot
  output$pca_scores <- renderPlotly({
    req(pca_state$results, pca_state$metadata, input$pc_x, input$pc_y, input$pca_color)
    
    # Get scores and metadata
    scores_df <- as.data.frame(pca_state$results$scores)
    metadata <- pca_state$metadata
    
    # Combine scores with metadata
    plot_data <- bind_cols(scores_df, metadata) %>%
      mutate(dataset = factor(dataset))  # Ensure dataset is a factor
    
    # Get variance explained for axis labels
    var_exp <- pca_state$results$var_explained
    
    # Create color mapping
    if(input$pca_color %in% pca_state$common_params) {
      # For numeric parameters
      color_values <- plot_data[[input$pca_color]]
      color_title <- str_to_title(str_replace_all(input$pca_color, "_", " "))
      color_scale <- scale_color_viridis_c()
    } else {
      # For categorical variables
      color_values <- plot_data[[input$pca_color]]
      color_title <- str_to_title(str_replace_all(input$pca_color, "_", " "))
      
      if(input$pca_color == "dataset") {
        # Create a subset of dataset colors for only the selected datasets
        selected_datasets <- unique(plot_data$dataset)
        dataset_colors_subset <- dataset_colors[as.character(selected_datasets)]
        color_scale <- scale_color_manual(values = dataset_colors_subset)
      } else if(input$pca_color == "age") {
        color_scale <- scale_color_viridis_c()
      } else {
        # Get unique values for proper color mapping
        unique_values <- unique(color_values)
        n_colors <- length(unique_values)
        if(n_colors <= 8){
          color_palette <- RColorBrewer::brewer.pal(n_colors, "Set2")
        } else {
          color_palette <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(n_colors)
        }
        color_scale <- scale_color_manual(values = setNames(color_palette, unique_values))
      }
    }
    
    # Create score plot
    p <- ggplot(plot_data, 
                aes(x = !!sym(input$pc_x), 
                    y = !!sym(input$pc_y),
                    color = color_values,
                    text = paste(
                      "ID:", participant_id,
                      "\nDataset:", dataset,
                      "\nAge:", age,
                      "\nSex:", sex,
                      "\nAge Group:", age_group,
                      if(input$pca_color %in% pca_state$common_params)
                        paste("\n", color_title, ":", round(color_values, 2))
                      else
                        paste("\n", color_title, ":", color_values)
                    ))) +
      geom_point(alpha = 0.7, size = 3) +
      theme_minimal() +
      theme(
        text = element_text(family = "sans-serif"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "right"
      ) +
      labs(
        title = "PCA Score Plot",
        x = sprintf("%s (%.1f%% explained var.)", input$pc_x, var_exp[as.numeric(gsub("PC", "", input$pc_x))] * 100),
        y = sprintf("%s (%.1f%% explained var.)", input$pc_y, var_exp[as.numeric(gsub("PC", "", input$pc_y))] * 100),
        color = color_title
      ) +
      color_scale
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        showlegend = TRUE,
        legend = list(x = 1.05, y = 0.5),
        margin = list(r = 100),
        hoverlabel = list(font = list(family = "sans-serif"))
      ) %>%
      config(displayModeBar = TRUE)
  })
  
  # Scree Plot
  output$pca_scree <- renderPlotly({
    req(pca_state$results)
    
    var_explained <- pca_state$results$var_explained
    df <- data.frame(
      PC = factor(paste0("PC", 1:length(var_explained)), 
                 levels = paste0("PC", 1:length(var_explained))),  # Force correct ordering
      Variance = var_explained * 100
    )
    
    p <- ggplot(df, aes(x = PC, y = Variance)) +
      geom_bar(stat = "identity", fill = "#4682B4", alpha = 0.7) +
      theme_minimal() +
      theme(
        text = element_text(family = "sans-serif"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      labs(
        title = "Scree Plot",
        y = "Variance Explained (%)",
        x = "Principal Components"
      )
    
    ggplotly(p)
  })
  
  # Loadings Plot
  output$pca_loadings <- renderPlotly({
    req(pca_state$results, input$selected_pc, input$n_loadings)
    
    # Get loadings for selected PC
    pc_num <- as.numeric(gsub("PC", "", input$selected_pc))
    loadings <- pca_state$results$loadings[, pc_num]
    
    # Create data frame with parameter names and loadings
    loadings_df <- data.frame(
      parameter = rownames(pca_state$results$loadings),
      loading = loadings
    ) %>%
      arrange(desc(abs(loading))) %>%
      head(input$n_loadings) %>%
      mutate(
        param_name = str_to_title(str_replace_all(parameter, "_", " ")),
        direction = ifelse(loading > 0, "Positive", "Negative")
      )
    
    # Create loadings plot
    p <- ggplot(loadings_df, 
                aes(x = reorder(param_name, abs(loading)),
                    y = loading,
                    fill = direction,
                    text = paste("Parameter:", param_name,
                               "\nLoading:", round(loading, 3)))) +
      geom_bar(stat = "identity", alpha = 0.7) +
      scale_fill_manual(values = c("Positive" = "#4CAF50", "Negative" = "#FF5722")) +
      coord_flip() +
      theme_minimal() +
      theme(
        text = element_text(family = "sans-serif"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "right"
      ) +
      labs(
        title = paste("Top", input$n_loadings, "Loadings for", input$selected_pc),
        x = "Parameter",
        y = "Loading Value",
        fill = "Direction"
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        margin = list(l = 150),  # Increase left margin for parameter names
        showlegend = TRUE
      )
  })
  
  # Section 9: Data Integration Workflow -------------------------
  
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
      showNotification(paste("Error reading data:", e$message), type = "error")
    })
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
  
  # Data validation handler
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
        # Update progress
        incProgress(0.3, detail = "Validating data...")
        
        # Get current datasets
        current_datasets <- integrated_datasets_rv()
        
        # Check if dataset name already exists
        if(input$dataset_name %in% names(current_datasets)) {
          showNotification(
            paste("Dataset name", input$dataset_name, "already exists. Please choose a different name."),
            type = "error"
          )
          return()
        }
        
        # Add new dataset to the list
        incProgress(0.3, detail = "Saving data...")
        current_datasets[[input$dataset_name]] <- integration_state$processed_data
        
        # Update reactive value
        integrated_datasets_rv(current_datasets)
        
        # Save to disk
        saveDatasets(current_datasets)
        
        # Success message
        showNotification(
          paste("Dataset", input$dataset_name, "successfully integrated!"),
          type = "message"
        )
        
        # Update dataset choices in UI
        updateCheckboxGroupInput(session, "selected_datasets",
                               choices = setNames(names(current_datasets),
                                                paste(names(current_datasets), "Data")),
                               selected = input$dataset_name)
        
        # Reset states
        integration_state$processed_data <- NULL
        integration_state$validation_result <- NULL
        
      }, error = function(e) {
        showNotification(
          paste("Integration failed:", e$message),
          type = "error"
        )
      })
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
  
  # Section 10: Statistical Analysis -------------------------
  
  # Update comparison groups
  observeEvent(input$update_comparison, {
    req(input$group1_select, input$group2_select)
    
    group_comparison$group1 <- input$group1_select
    group_comparison$group2 <- input$group2_select
    
    # Trigger stats calculation
    stats_data <- calculate_statistics()
    if(!is.null(stats_data)) {
      output$stats_table <- renderDT({
        datatable(stats_data,
                 options = list(scrollX = TRUE, pageLength = 15)) %>%
          formatSignif(columns = c("p_value", "effect_size"), digits = 3)
      })
    }
  })
  
  # Calculate statistics
  calculate_statistics <- reactive({
    req(group_comparison$group1, group_comparison$group2)
    
    plot_data_local <- prepare_plot_data()
    req(plot_data_local)
    
    group_var <- get_group_var()
    
    # Filter data for selected groups
    analysis_data <- plot_data_local %>%
      filter(!!sym(group_var) %in% c(group_comparison$group1, group_comparison$group2))
    
    # Check if there are at least two groups
    if(n_distinct(analysis_data[[group_var]]) < 2) {
      showNotification("At least two groups are required for statistical comparison.", type = "error")
      return(NULL)
    }
    
    # Get numeric parameters
    numeric_cols <- names(analysis_data)[sapply(analysis_data, is.numeric)]
    numeric_cols <- setdiff(numeric_cols, c("age"))  # Exclude 'age' if not needed
    
    # Calculate statistics for each parameter
    results <- map_df(numeric_cols, function(param) {
      tryCatch({
        # Perform t-test
        t_test <- t.test(
          analysis_data[[param]][analysis_data[[group_var]] == group_comparison$group1],
          analysis_data[[param]][analysis_data[[group_var]] == group_comparison$group2]
        )
        
        # Calculate effect size (Cohen's d)
        ef_size <- cohen.d(analysis_data[[param]] ~ analysis_data[[group_var]], 
                          data = analysis_data)
        
        tibble(
          Parameter = param,
          Mean_Group1 = mean(analysis_data[[param]][analysis_data[[group_var]] == group_comparison$group1], na.rm = TRUE),
          Mean_Group2 = mean(analysis_data[[param]][analysis_data[[group_var]] == group_comparison$group2], na.rm = TRUE),
          Difference = Mean_Group2 - Mean_Group1,
          P_value = t_test$p.value,
          Effect_size = ef_size$estimate,
          Significant = P_value < 0.05
        )
      }, error = function(e) NULL)
    })
    
    if(nrow(results) == 0) return(NULL)
    results
  })
  
  # Update statistics table
  output$stats_table <- renderDT({
    stats_data <- calculate_statistics()
    req(stats_data)
    
    datatable(stats_data,
              options = list(scrollX = TRUE, pageLength = 15)) %>%
      formatRound(columns = c("Mean_Group1", "Mean_Group2", "Difference", "Effect_size"), 
                 digits = 3) %>%
      formatSignif(columns = c("P_value"), digits = 3) %>%
      formatStyle(
        'Significant',
        backgroundColor = styleEqual(c(TRUE, FALSE), c('#a8e6cf', '#ffd3b6'))
      )
  })
  
  # Update volcano plot
  output$volcano_plot <- renderPlotly({
    stats_data <- calculate_statistics()
    req(stats_data)
    
    p <- ggplot(stats_data, 
                aes(x = Effect_size, 
                    y = -log10(P_value),
                    color = Significant,
                    text = paste("Parameter:", Parameter,
                               "\nEffect Size:", round(Effect_size, 3),
                               "\np-value:", format.pval(P_value, digits = 3)))) +
      geom_point(size = 3) +
      geom_hline(yintercept = -log10(0.05), 
                 linetype = "dashed", 
                 color = "grey50", 
                 alpha = 0.5) +
      scale_color_manual(values = c("TRUE" = "#FF9642", "FALSE" = "#87CEEB")) +
      theme_minimal() +
      theme(
        text = element_text(family = "sans-serif"),
        plot.title = element_text(hjust = 0.5, family = "sans-serif")
      ) +
      labs(
        title = paste("Volcano Plot:",
                     group_comparison$group2, "vs", group_comparison$group1),
        x = "Effect Size",
        y = "-log10(p-value)",
        color = "Significant"
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  # Update dataset management dropdowns
  observe({
    datasets <- integrated_datasets_rv()
    updateSelectInput(session, "dataset_to_delete",
                     choices = setNames(names(datasets), 
                                      paste(names(datasets), "Data")))
  })
  
  # Maintain parameter selection when datasets change
  observeEvent(input$selected_datasets, {
    # Store current parameter selection
    current_param <- isolate(input$blood_param)
    new_choices <- filtered_params()
    
    # Update choices but keep current selection if possible
    updateSelectizeInput(session, "blood_param",
                        choices = new_choices,
                        selected = if(current_param %in% new_choices) current_param else new_choices[1],
                        server = TRUE)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  # Update PCA parameters automatically
  observe({
    req(input$pca_datasets)
    
    # Get common parameters across selected datasets
    datasets <- integrated_datasets_rv()
    selected_data <- datasets[input$pca_datasets]
    
    common_params <- Reduce(intersect, 
                          lapply(selected_data, function(x) {
                            names(x)[sapply(x, is.numeric)]
                          }))
    
    # Remove metadata columns
    param_cols <- setdiff(common_params, 
                         c("age", "month"))
    
    # Update PCA parameter selection
    updateSelectizeInput(session, "pca_params",
                        choices = setNames(param_cols, 
                                         str_to_title(str_replace_all(param_cols, "_", " "))),
                        selected = param_cols)
  })
}
