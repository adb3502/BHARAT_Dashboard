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
     "))
    ),
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Clinical Parameters", tabName = "clinical", icon = icon("heartbeat")),
      menuItem("Cognitive Assessment", tabName = "cognitive", icon = icon("brain")),
      menuItem("Physical Assessment", tabName = "physical", icon = icon("dumbbell")),
      menuItem("Lifestyle", tabName = "lifestyle", icon = icon("walking"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Overview Tab ----------------------------------------------------------
      tabItem(tabName = "overview",
              fluidRow(
                box(
                  width = 12,
                  title = "Study Demographics",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("demographics_plot", height = "400px")
                )
              ),
              fluidRow(
                box(
                  width = 6,
                  title = "Age Distribution",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("age_dist_plot", height = "300px")
                ),
                box(
                  width = 6,
                  title = "Study Completion",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("completion_plot", height = "300px")
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  title = "Data Completeness",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("completeness_plot", height = "400px")
                )
              )
      ),
      
      # Clinical Parameters Tab ----------------------------------------------
      tabItem(tabName = "clinical",
              fluidRow(
                box(
                  width = 3,
                  title = "Controls",
                  status = "primary",
                  solidHeader = TRUE,
                  
                  # Parameter selection
                  selectizeInput("clinical_param", "Select Parameter",
                                 choices = clinical_params,
                                 multiple = FALSE),
                  
                  # Grouping selection
                  radioButtons("grouping", "Group By",
                               choices = grouping_choices),
                  
                  # Color scheme selection
                  selectInput("color_scheme", "Color Scheme",
                              choices = color_schemes),
                  
                  # Plot controls
                  checkboxInput("show_stats", "Show Statistics", TRUE),
                  
                  # Plot type
                  radioButtons("plot_type", "Plot Type",
                               choices = c("Box Plot" = "box",
                                           "Violin Plot" = "violin",
                                           "Scatter Plot" = "scatter"))
                ),
                box(
                  width = 9,
                  title = "Distribution Analysis",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("param_dist", height = "600px"),
                  verbatimTextOutput("param_stats")
                )
              )
      ),
      
      # Cognitive Assessment Tab ---------------------------------------------
      tabItem(tabName = "cognitive",
              fluidRow(
                box(
                  width = 12,
                  title = "MMSE Score Distribution",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("mmse_dist", height = "400px")
                )
              ),
              fluidRow(
                box(
                  width = 6,
                  title = "Domain Scores",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("mmse_domains", height = "300px")
                ),
                box(
                  width = 6,
                  title = "Age-Related Trends",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("mmse_age_trends", height = "300px")
                )
              )
      ),
      
      # Physical Assessment Tab ---------------------------------------------
      tabItem(tabName = "physical",
              fluidRow(
                box(
                  width = 12,
                  title = "Frailty Score Distribution",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("frailty_dist", height = "400px")
                )
              ),
              fluidRow(
                box(
                  width = 6,
                  title = "Component Analysis",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("frailty_components", height = "300px")
                ),
                box(
                  width = 6,
                  title = "Physical Measurements",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("physical_measures", height = "300px")
                )
              )
      ),
      
      # Lifestyle Tab ------------------------------------------------------
      tabItem(tabName = "lifestyle",
              fluidRow(
                box(
                  width = 6,
                  title = "Exercise Patterns",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("exercise_patterns", height = "300px")
                ),
                box(
                  width = 6,
                  title = "Dietary Habits",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("dietary_patterns", height = "300px")
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  title = "Lifestyle Correlations",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("lifestyle_correlations", height = "400px")
                )
              )
      )
    )
  )
)