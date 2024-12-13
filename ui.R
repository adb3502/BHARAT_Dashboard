# ui.R
dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(
    title = span("BHARAT Study", style = "font-family: 'Manrope', sans-serif;")
  ),
  
  # Sidebar
  dashboardSidebar(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", 
               href = "https://fonts.googleapis.com/css2?family=Manrope:wght@400;600&display=swap"),
      tags$style(HTML("
        /* Global Styles */
        .main-header .logo { font-family: 'Manrope', sans-serif; }
        .content-wrapper { font-family: 'Manrope', sans-serif; }
        .box-title { font-family: 'Manrope', sans-serif; }
        body { font-family: 'Manrope', sans-serif; }
        .selectize-input { font-family: 'Manrope', sans-serif !important; }
        .selectize-dropdown { font-family: 'Manrope', sans-serif !important; }
        
        /* Custom Styling */
        .box.box-solid.box-primary {
          border: 1px solid #3c8dbc;
        }
        
        .box.box-solid.box-primary > .box-header {
          background: #3c8dbc;
          background-color: #3c8dbc;
        }
        
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
        
        /* Control panel scrolling */
        .control-panel {
          max-height: calc(100vh - 100px);
          overflow-y: auto;
        }
        
        /* Info box styling */
        .info-box {
          min-height: 100px;
          margin-bottom: 15px;
        }
        
        /* Tab styling */
        .nav-tabs-custom > .nav-tabs > li.active {
          border-top-color: #3c8dbc;
        }
        
        /* Plot container */
        .plot-container {
          background: white;
          padding: 15px;
          border-radius: 4px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.12);
        }
      "))
    ),
    
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Blood Parameters", tabName = "blood", icon = icon("tint")),
      menuItem("Data Integration", tabName = "integration", icon = icon("database")),
      menuItem("Demographics", tabName = "demographics", icon = icon("users")),
      menuItem("PCA Analysis", tabName = "pca", icon = icon("chart-scatter")),
      menuItem("Statistical Summary", tabName = "stats", icon = icon("table"))
    )
  ),
  
  # Body
  dashboardBody(
    # Enable shinyjs
    shinyjs::useShinyjs(),
    
    # Custom group creation modal
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
               actionButton("add_group", "Add Group", 
                          class = "btn-success",
                          icon = icon("plus"))
        ),
        column(6,
               h4("Current Custom Groups"),
               DTOutput("current_groups"),
               br(),
               actionButton("remove_selected_groups", "Remove Selected", 
                          class = "btn-danger",
                          icon = icon("trash"))
        )
      )
    ),
    
    tabItems(
      # Overview tab
      tabItem(tabName = "overview",
              fluidRow(
                box(
                  width = 12,
                  title = "Dataset Overview",
                  status = "primary",
                  solidHeader = TRUE,
                  
                  fluidRow(
                    column(8,
                           plotlyOutput("total_samples_plot", height = "400px")
                    ),
                    column(4,
                           h4("Quick Statistics"),
                           uiOutput("dataset_stats")
                    )
                  )
                )
              ),
              
              fluidRow(
                box(
                  width = 12,
                  title = "Dataset Information",
                  status = "primary",
                  solidHeader = TRUE,
                  DTOutput("overview_datasets_table")
                )
              )),

    # PCA Analysis tab
      tabItem(tabName = "pca",
              fluidRow(
                box(
                  width = 3,
                  title = "PCA Controls",
                  status = "primary",
                  solidHeader = TRUE,
                  
                  # Dataset selection
                  checkboxGroupInput("pca_datasets", "Select Datasets",
                                   choices = NULL,
                                   selected = "BHARAT"),
                  
                  # Parameter selection
                  selectizeInput("pca_params", "Select Parameters",
                               choices = param_choices,
                               multiple = TRUE),
                  
                  # PC selection for plotting
                  selectInput("pc_x", "X-axis PC",
                            choices = NULL),
                  selectInput("pc_y", "Y-axis PC",
                            choices = NULL),
                  
                  # Coloring options
                  selectInput("pca_color", "Color Points By",
                            choices = c(
                              "Age Groups" = "age_group",
                              "Sex" = "sex",
                              "Dataset" = "dataset",
                              "Age" = "age"
                            )),
                  
                  # Loading plot controls
                  selectInput("selected_pc", "Select PC for Loadings",
                            choices = NULL),
                  numericInput("n_loadings", "Number of Top Loadings",
                             value = 10, min = 5, max = 20)
                ),
                box(
                  width = 9,
                  title = "PCA Visualization",
                  status = "primary",
                  solidHeader = TRUE,
                  
                  tabsetPanel(
                    tabPanel("Score Plot",
                            div(class = "plot-container",
                                plotlyOutput("pca_scores", height = "500px"))
                    ),
                    tabPanel("Scree Plot",
                            div(class = "plot-container",
                                plotlyOutput("pca_scree", height = "500px"))
                    ),
                    tabPanel("Loadings Plot",
                            div(class = "plot-container",
                                plotlyOutput("pca_loadings", height = "500px"))
                    )
                  )
                )
              )),
      
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
                      
                      # Dataset Management (only visible in this tab)
                      conditionalPanel(
                        condition = "input.integration_tabs == '3'",
                        box(
                          width = 12,
                          title = "Manage Datasets",
                          status = "warning",
                          solidHeader = TRUE,
                          
                          fluidRow(
                            column(4,
                                   selectInput("dataset_to_delete", "Select Dataset to Delete",
                                             choices = NULL)
                            ),
                            column(4,
                                   actionButton("delete_dataset", "Delete Dataset",
                                              class = "btn-danger",
                                              icon = icon("trash"))
                            )
                          ),
                          br(),
                          DTOutput("integrated_datasets_table")
                        )
                      )
                    )
                  )
                )
              )),
      
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
                  fluidRow(
                    column(6,
                           radioButtons("stats_grouping", "Compare By",
                                      choices = c(
                                        "Age Groups" = "age",
                                        "Sex" = "sex",
                                        "Age and Sex Combined" = "combined",
                                        "Custom Groups" = "custom"
                                      ),
                                      inline = TRUE)
                    ),
                    column(6,
                           actionButton("show_group_modal_stats", "Use Custom Groups",
                                      icon = icon("object-group"),
                                      class = "btn-info btn-sm")
                    )
                  ),
                  
                  # Group selection for volcano plot
                  fluidRow(
                    column(6,
                           selectInput("group1_select", "Select Group 1",
                                     choices = NULL,
                                     multiple = TRUE)
                    ),
                    column(6,
                           selectInput("group2_select", "Select Group 2",
                                     choices = NULL,
                                     multiple = TRUE)
                    )
                  ),
                  
                  actionButton("update_comparison", "Update Comparison",
                             icon = icon("refresh"),
                             class = "btn-primary"),
                  
                  br(), br(),
                  
                  tabsetPanel(
                    tabPanel("Summary Table",
                            DTOutput("stats_table")
                    ),
                    tabPanel("Volcano Plot",
                            div(class = "plot-container",
                                plotlyOutput("volcano_plot", height = "600px")
                            )
                    )
                  )
                )
              ))
    )
  )
)