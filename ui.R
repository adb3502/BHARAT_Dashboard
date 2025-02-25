# ui.R

library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(shinyBS)
library(shinyjs)

# Define any necessary variables here
# For example:
# - param_choices
# - blood_params
# - standard_age_groups
# - grouping_choices
# - color_by_choices
# - color_schemes
# - dataset_colors

dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(
    title = span("BHARAT Study", style = "font-family: 'Manrope', sans-serif;")
  ),
  
  # Sidebar
  dashboardSidebar(
    tags$head(
      tags$style(HTML("
        /* Global Styles */
        .main-header .logo { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif; }
        .content-wrapper { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif; }
        .box-title { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif; }
        body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif; }
        .selectize-input { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif !important; }
        .selectize-dropdown { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif !important; }
        
        /* Box styling */
        .box.box-solid.box-primary {
          border: 1px solid #3c8dbc;
        }
        .box.box-solid.box-primary > .box-header {
          background: #3c8dbc;
        }
        
        /* Custom styling */
        .info-box { min-height: 100px; margin-bottom: 15px; }
        .plot-container {
          background: white;
          padding: 15px;
          border-radius: 4px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.12);
        }
        .control-panel {
          max-height: calc(100vh - 100px);
          overflow-y: auto;
          padding-right: 10px;
        }
        
        /* Button styling */
        .btn-custom {
          margin: 5px;
          min-width: 100px;
        }
        .action-button { margin: 5px 0; }
        
        /* Parameters visibility */
        #common_params_div {
          margin-top: 10px;
          padding: 5px;
          border-radius: 4px;
        }
      "))
    ),
    
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Blood Parameters", tabName = "blood", icon = icon("tint")),
      menuItem("Demographics", tabName = "demographics", icon = icon("users")),
      menuItem("Data Integration", tabName = "integration", icon = icon("database")),
      menuItem("PCA Analysis", tabName = "pca", icon = icon("chart-line")),
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
               textInput("group_name", "Group Name", 
                        placeholder = "Enter a name for this group"),
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
                          icon = icon("plus")),
               tags$div(
                 style = "margin-top: 15px;",
                 tags$small(class = "text-muted",
                          "Select multiple age ranges to combine into a custom group")
               )
        ),
        column(6,
               h4("Current Custom Groups"),
               DTOutput("current_groups"),
               br(),
               actionButton("remove_selected_groups", "Remove Selected", 
                          class = "btn-danger",
                          icon = icon("trash")),
               tags$div(
                 style = "margin-top: 15px;",
                 tags$small(class = "text-muted",
                          "Select a group and click 'Remove Selected' to delete it")
               )
        )
      )
    ),
    
    tabItems(
      # Overview tab
      tabItem(tabName = "overview",
              fluidRow(
                box(width = 12,
                    title = "Dataset Summary",
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
                box(width = 12,
                    title = "Integrated Datasets",
                    status = "primary",
                    solidHeader = TRUE,
                    DTOutput("overview_datasets_table")
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
                  class = "control-panel",
                  
                  # Dataset selection
                  checkboxGroupInput("selected_datasets", "Select Datasets",
                                   choices = NULL,
                                   selected = "BHARAT"),
                  
                  # Common parameters filter (initially hidden)
                  div(id = "common_params_div",
                      style = "display: none;",
                      checkboxInput("show_common_only", "Show Common Parameters Only", FALSE)
                  ),
                  
                  # Parameter selection
                  selectizeInput("blood_param", "Select Parameter",
                                 choices = NULL,
                                 multiple = FALSE),
                  
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
                  
                  hr(),
                  
                  # Grouping selection
                  selectInput("grouping", "Group By",
                              choices = c(
                                "Age Groups" = "age",
                                "Sex" = "sex",
                                "Age and Sex Combined" = "age_sex",
                                "Custom Age Groups" = "custom_age",
                                "Custom Age Groups and Sex" = "custom_age_sex"
                              ),
                              selected = "age"),
                  
                  # Secondary color option
                  selectInput("color_by", "Color Points By",
                              choices = c(
                                "None" = "none",
                                "Age Groups" = "age_group",
                                "Custom Age Groups" = "custom_age_group",
                                "Sex" = "sex",
                                "Dataset" = "dataset",
                                "Age (continuous)" = "age"
                              ),
                              selected = "dataset"),
                  
                  # Color scheme selection
                  selectInput("color_scheme", "Color Scheme",
                              choices = c(
                                "Zissou" = "zissou",
                                "Darjeeling" = "darjeeling",
                                "Royal" = "royal"
                              ),
                              selected = "zissou"),
                  
                  # Show reference lines
                  checkboxInput("show_ref", "Show Reference Ranges", TRUE),
                  
                  # Show statistics
                  checkboxInput("show_stats", "Show Statistics", FALSE),
                  
                  # Plot type
                  radioButtons("plot_type", "Plot Type",
                             choices = c(
                               "Box Plot" = "box",
                               "Violin Plot" = "violin",
                               "Scatter Plot" = "scatter"
                             ),
                             selected = "box")
                ),
                
                box(
                  width = 9,
                  title = "Distribution Analysis",
                  status = "primary",
                  solidHeader = TRUE,
                  div(class = "plot-container",
                      plotlyOutput("blood_dist", height = "600px")),
                  # Display stats below the plot
                  uiOutput("stats_info")
                )
              )),
      
      # Demographics tab
      tabItem(tabName = "demographics",
              fluidRow(
                box(width = 12,
                    title = "Population Demographics",
                    status = "primary",
                    solidHeader = TRUE,
                    
                    fluidRow(
                      column(3,
                             # Controls
                             checkboxGroupInput("demo_datasets", "Select Datasets",
                                              choices = NULL,
                                              selected = "BHARAT"),
                             
                             checkboxGroupInput("demo_age_groups", "Select Age Groups",
                                              choices = c(
                                                "18-29" = "18-29",
                                                "30-44" = "30-44",
                                                "45-59" = "45-59",
                                                "60-74" = "60-74",
                                                "75+" = "75+"
                                              ),
                                              selected = c("18-29", "30-44", "45-59", "60-74", "75+")),
                             
                             actionButton("show_group_modal_demo", "Use Custom Groups",
                                        icon = icon("object-group"),
                                        class = "btn-info btn-sm")
                      ),
                      column(9,
                             # Visualization tabs
                             tabsetPanel(
                               tabPanel("Age Distribution",
                                        div(class = "plot-container",
                                            plotlyOutput("age_distribution", height = "500px"))
                               ),
                               tabPanel("Age Groups Distribution",
                                        div(class = "plot-container",
                                            plotlyOutput("age_groups_dist", height = "500px"))
                               )
                             )
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
                        column(6,
                               textInput("dataset_name", "Dataset Name",
                                        placeholder = "Enter a unique identifier for this dataset"),
                               fileInput("external_data", "Upload External Dataset (CSV)",
                                        accept = c("text/csv", ".csv")),
                               actionButton("preview_data", "Preview Data",
                                          icon = icon("eye"), 
                                          class = "btn-primary")
                        ),
                        column(6,
                               div(style = "margin-top: 20px;",
                                   uiOutput("data_info"))
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
                        column(4,
                               downloadButton("download_template", "Download Mapping Template",
                                            class = "btn-info"),
                               br(), br(),
                               fileInput("mapping_file", "Upload Column Mapping (CSV)",
                                        accept = c("text/csv", ".csv"))
                        ),
                        column(8,
                               uiOutput("mapping_status"))
                      ),
                      br(),
                      DTOutput("mapping_preview")
                    ),
                    
                    # Step 3: Validation and Integration
                    tabPanel(
                      "3. Validate & Integrate",
                      br(),
                      fluidRow(
                        column(6,
                               actionButton("validate_integration", "Validate Data",
                                          icon = icon("check-circle"),
                                          class = "btn-warning")
                        ),
                        column(6,
                               uiOutput("validation_status"))
                      ),
                      br(),
                      actionButton("integrate_data", "Integrate Dataset",
                                 icon = icon("plus-circle"),
                                 class = "btn-success"),
                      br(), br(),
                      
                      box(width = 12,
                          title = "Manage Datasets",
                          status = "warning",
                          solidHeader = TRUE,
                          
                          fluidRow(
                            column(4,
                                   selectInput("dataset_to_delete", "Select Dataset to Delete",
                                             choices = NULL)),
                            column(4,
                                   actionButton("delete_dataset", "Delete Dataset",
                                              class = "btn-danger",
                                              icon = icon("trash")))
                          ),
                          br(),
                          DTOutput("integrated_datasets_table")
                      )
                    )
                  )
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
                  
                  checkboxGroupInput("pca_datasets", "Select Datasets",
                                   choices = NULL,
                                   selected = "BHARAT"),
                  
                  # PCA controls
                  selectInput("pc_x", "X-axis",
                            choices = NULL,
                            selected = NULL),
                  
                  selectInput("pc_y", "Y-axis",
                            choices = NULL,
                            selected = NULL),
                  
                  selectInput("pca_color", "Color Points By",
                            choices = c(
                              "None" = "none",
                              "Age Groups" = "age_group",
                              "Sex" = "sex",
                              "Dataset" = "dataset",
                              "Age" = "age"
                            ),
                            selected = "dataset"),
                  
                  # Number of loadings to show
                  sliderInput("n_loadings", "Number of Top Loadings",
                            min = 5, max = 30, value = 15, step = 5),
                  
                  # PC selection for loadings
                  selectInput("selected_pc", "Select PC for Loadings",
                            choices = NULL,
                            selected = NULL)
                ),
                
                box(
                  width = 9,
                  title = "PCA Visualization",
                  status = "primary",
                  solidHeader = TRUE,
                  
                  tabsetPanel(
                    tabPanel("Score Plot",
                             div(class = "plot-container",
                                 plotlyOutput("pca_scores", height = "500px"))),
                    tabPanel("Scree Plot",
                             div(class = "plot-container",
                                 plotlyOutput("pca_scree", height = "500px"))),
                    tabPanel("Loadings Plot",
                             div(class = "plot-container",
                                 plotlyOutput("pca_loadings", height = "500px")))
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
                  
                  fluidRow(
                    column(4,
                           # Dataset selection
                           checkboxGroupInput("stats_datasets", "Select Datasets for Analysis",
                                            choices = NULL,
                                            selected = "BHARAT")
                    ),
                    column(4,
                           # Age groups selection
                           checkboxGroupInput("stats_age_groups", "Select Age Groups",
                                            choices = c(
                                              "18-29" = "18-29",
                                              "30-44" = "30-44",
                                              "45-59" = "45-59",
                                              "60-74" = "60-74",
                                              "75+" = "75+"
                                            ),
                                            selected = c("18-29", "30-44", "45-59", "60-74", "75+"))
                    ),
                    column(4,
                           # Grouping controls
                           radioButtons("stats_grouping", "Compare By",
                                      choices = c(
                                        "Age Groups" = "age",
                                        "Sex" = "sex",
                                        "Age and Sex Combined" = "combined",
                                        "Custom Age Groups" = "custom"
                                      ),
                                      inline = TRUE),
                           actionButton("show_group_modal_stats", "Use Custom Groups",
                                      icon = icon("object-group"),
                                      class = "btn-info btn-sm")
                    )
                  ),
                  
                  hr(),
                  
                  fluidRow(
                    column(5,
                           selectInput("group1_select", "Select Group 1",
                                     choices = NULL,
                                     multiple = TRUE)
                    ),
                    column(5,
                           selectInput("group2_select", "Select Group 2",
                                     choices = NULL,
                                     multiple = TRUE)
                    ),
                    column(2,
                           actionButton("update_comparison", "Update Comparison",
                                        icon = icon("refresh"),
                                        class = "btn-primary btn-custom")
                    )
                  ),
                  
                  br(),
                  
                  tabsetPanel(
                    tabPanel("Summary Table",
                             div(class = "plot-container",
                                 DTOutput("stats_table"))
                    ),
                    tabPanel("Volcano Plot",
                             div(class = "plot-container",
                                 plotlyOutput("volcano_plot", height = "600px"))
                    )
                  )
                )
              ))
    )
  )
)
