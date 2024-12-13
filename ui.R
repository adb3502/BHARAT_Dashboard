# ui.R
dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = span("BHARAT Study", style = "font-family: 'Manrope', sans-serif;")
  ),
  
  dashboardSidebar(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css2?family=Manrope:wght@400;600&display=swap"),
      tags$style(HTML("
        .main-header .logo { font-family: 'Manrope', sans-serif; }
        .content-wrapper { font-family: 'Manrope', sans-serif; }
        .box-title { font-family: 'Manrope', sans-serif; }
        body { font-family: 'Manrope', sans-serif; }
        .selectize-input { font-family: 'Manrope', sans-serif !important; }
        .selectize-dropdown { font-family: 'Manrope', sans-serif !important; }
        
        /* Custom group modal styling */
        .modal-content { font-family: 'Manrope', sans-serif; }
        .group-item { 
          padding: 8px;
          margin: 4px 0;
          border: 1px solid #ddd;
          border-radius: 4px;
        }
        .group-item:hover { background-color: #f5f5f5; }
        
        /* Delete button styling */
        .btn-danger { margin-top: 10px; }
        
        /* Dataset management spacing */
        #dataset_to_delete { margin-bottom: 10px; }
      "))
    ),
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Blood Parameters", tabName = "blood", icon = icon("tint")),
      menuItem("Data Integration", tabName = "integration", icon = icon("database")),
      menuItem("Demographics", tabName = "demographics", icon = icon("users")),
      menuItem("Statistical Summary", tabName = "stats", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    # Modal for custom group creation
    bsModal(
      "groupModal",
      "Create Custom Age Groups",
      "show_group_modal",
      size = "large",
      
      fluidRow(
        column(6,
               textInput("group_name", "Group Name"),
               selectInput("group_ranges", "Select Age Ranges",
                          choices = c(
                            "18-29" = "18-29",
                            "30-44" = "30-44",
                            "45-59" = "45-59",
                            "60-74" = "60-74",
                            "75+" = "75+"
                          ),
                          multiple = TRUE),
               actionButton("add_group", "Add Group", class = "btn-success")
        ),
        column(6,
               h4("Current Custom Groups"),
               DTOutput("current_groups"),
               br(),
               actionButton("remove_selected_groups", "Remove Selected", class = "btn-danger")
        )
      )
    ),
    
    tabItems(
      # Demographics tab
      tabItem(tabName = "demographics",
              fluidRow(
                box(
                  width = 12,
                  title = "Population Demographics",
                  status = "primary",
                  solidHeader = TRUE,
                  
                  # Dataset selection
                  checkboxGroupInput("demo_datasets", "Select Datasets",
                                   choices = NULL,
                                   selected = "BHARAT"),
                  
                  # Age range selection for demographics
                  checkboxGroupInput("demo_age_groups", "Select Age Groups",
                                   choices = c(
                                     "18-29" = "18-29",
                                     "30-44" = "30-44",
                                     "45-59" = "45-59",
                                     "60-74" = "60-74",
                                     "75+" = "75+"
                                   ),
                                   selected = c("18-29", "30-44", "45-59", "60-74", "75+")),
                  
                  # Visualization tabs
                  tabsetPanel(
                    tabPanel("Age Distribution",
                            fluidRow(
                              column(8, 
                                     plotlyOutput("age_distribution", height = "500px")
                              ),
                              column(4,
                                     uiOutput("age_summary")
                              )
                            )
                    ),
                    tabPanel("Age Groups",
                            fluidRow(
                              column(8,
                                     plotlyOutput("age_groups_dist", height = "500px")
                              ),
                              column(4,
                                     uiOutput("group_summary")
                              )
                            )
                    )
                  )
                )
              )),
      
      # Blood Parameters tab
      tabItem(tabName = "blood",
              fluidRow(
                box(
                  width = 3,
                  title = "Controls",
                  status = "primary",
                  solidHeader = TRUE,
                  
                  # Dataset selection
                  checkboxGroupInput("selected_datasets", "Select Datasets",
                                   choices = NULL,
                                   selected = "BHARAT"),
                  
                  # Age range selection
                  checkboxGroupInput("selected_age_groups", "Select Age Groups",
                                   choices = c(
                                     "18-29" = "18-29",
                                     "30-44" = "30-44",
                                     "45-59" = "45-59",
                                     "60-74" = "60-74",
                                     "75+" = "75+"
                                   ),
                                   selected = c("18-29", "30-44", "45-59", "60-74", "75+")),
                  
                  # Custom group creation
                  actionButton("show_group_modal", "Create Custom Groups",
                             icon = icon("object-group"),
                             class = "btn-info btn-sm"),
                  
                  # Parameter selection with common parameters filter
                  checkboxInput("show_common_only", "Show Common Parameters Only", FALSE),
                  selectizeInput("blood_param", "Select Parameter",
                               choices = param_choices,
                               multiple = FALSE),
                  
                  # Grouping selection
                  radioButtons("grouping", "Group By",
                             choices = c(
                               "Age Groups" = "age",
                               "Sex" = "sex",
                               "Age and Sex Combined" = "combined",
                               "Custom Groups" = "custom"
                             )),
                  
                  # Secondary color option
                  selectInput("color_by", "Color Points By",
                            choices = c(
                              "None" = "none",
                              "Age Groups" = "age_group",
                              "Sex" = "sex",
                              "Dataset" = "dataset"
                            )),
                  
                  # Color scheme selection
                  selectInput("color_scheme", "Color Scheme",
                            choices = color_schemes),
                  
                  # Plot controls
                  checkboxInput("show_ref", "Show Reference Ranges", TRUE),
                  checkboxInput("show_stats", "Show Statistics", TRUE),
                  
                  # Plot type
                  radioButtons("plot_type", "Plot Type",
                             choices = c(
                               "Box Plot" = "box",
                               "Violin Plot" = "violin",
                               "Scatter Plot" = "scatter"
                             ))
                ),
                box(
                  width = 9,
                  title = "Distribution Analysis",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("blood_dist", height = "600px"),
                  verbatimTextOutput("blood_stats")
                )
              )
      ),
      
      # Data Integration tab
      tabItem(tabName = "integration",
              fluidRow(
                box(
                  width = 12,
                  title = "Data Integration Workflow",
                  status = "primary",
                  solidHeader = TRUE,
                  
                  tabsetPanel(
                    id = "integration_tabs",
                    
                    # Step 1: Upload Data
                    tabPanel(
                      "1. Upload Data",
                      br(),
                      fluidRow(
                        column(
                          width = 6,
                          textInput("dataset_name", "Dataset Name",
                                   placeholder = "Enter a unique identifier for this dataset"),
                          fileInput("external_data", "Upload External Dataset (CSV)",
                                   accept = c("text/csv", ".csv")),
                          actionButton("preview_data", "Preview Data",
                                     icon = icon("eye"), 
                                     class = "btn-primary")
                        ),
                        column(
                          width = 6,
                          div(
                            style = "margin-top: 20px;",
                            uiOutput("data_info")
                          )
                        )
                      ),
                      br(),
                      DTOutput("data_preview")
                    ),
                    
                    # Step 2: Column Mapping
                    tabPanel(
                      "2. Column Mapping",
                      br(),
                      fluidRow(
                        column(
                          width = 4,
                          downloadButton("download_template", "Download Mapping Template",
                                       class = "btn-info"),
                          br(), br(),
                          fileInput("mapping_file", "Upload Column Mapping (CSV)",
                                   accept = c("text/csv", ".csv"))
                        ),
                        column(
                          width = 8,
                          uiOutput("mapping_status")
                        )
                      ),
                      br(),
                      DTOutput("mapping_preview")
                    ),
                    
                    # Step 3: Validation and Integration
                    tabPanel(
                      "3. Validate & Integrate",
                      br(),
                      fluidRow(
                        column(
                          width = 6,
                          actionButton("validate_integration", "Validate Data",
                                     icon = icon("check-circle"),
                                     class = "btn-warning")
                        ),
                        column(
                          width = 6,
                          uiOutput("validation_status")
                        )
                      ),
                      br(),
                      actionButton("integrate_data", "Integrate Dataset",
                                 icon = icon("plus-circle"),
                                 class = "btn-success"),
                      br(), br(),
                      h4("Integrated Datasets"),
                      DTOutput("integrated_datasets_table")
                    )
                  )
                )),
              # Dataset Management Box
              fluidRow(
                box(
                  width = 12,
                  title = "Manage Datasets",
                  status = "warning",
                  solidHeader = TRUE,
                  
                  selectInput("dataset_to_delete", "Select Dataset to Delete",
                            choices = NULL),
                  actionButton("delete_dataset", "Delete Dataset",
                             class = "btn-danger",
                             icon = icon("trash"))
                )
              )
      ),
      
      # Statistical Summary tab
      tabItem(tabName = "stats",
              fluidRow(
                box(
                  width = 12,
                  title = "Statistical Summary",
                  status = "primary",
                  solidHeader = TRUE,
                  
                  # Dataset selection for stats
                  checkboxGroupInput("stats_datasets", "Select Datasets for Analysis",
                                   choices = NULL,
                                   selected = "BHARAT"),
                  
                  # Age group selection for stats
                  checkboxGroupInput("stats_age_groups", "Select Age Groups",
                                   choices = c(
                                     "18-29" = "18-29",
                                     "30-44" = "30-44",
                                     "45-59" = "45-59",
                                     "60-74" = "60-74",
                                     "75+" = "75+"
                                   ),
                                   selected = c("18-29", "30-44", "45-59", "60-74", "75+")),
                  
                  # Analysis grouping
                  radioButtons("stats_grouping", "Compare By",
                             choices = c(
                               "Age Groups" = "age",
                               "Sex" = "sex",
                               "Age and Sex Combined" = "combined",
                               "Custom Groups" = "custom"
                             ),
                             inline = TRUE),
                  
                  # Group comparison selection for volcano plot
                  selectInput("volcano_comparison", "Select Comparison",
                            choices = NULL,
                            width = "100%"),
                  
                  DTOutput("stats_table")
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  title = "Volcano Plot",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("volcano_plot", height = "600px")
                )
              )
      )
    )
  )
)