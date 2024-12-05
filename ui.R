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
      menuItem("Blood Parameters", tabName = "blood", icon = icon("tint")),
      menuItem("Statistical Summary", tabName = "stats", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "blood",
              fluidRow(
                box(
                  width = 3,
                  title = "Controls",
                  status = "primary",
                  solidHeader = TRUE,
                  
                  # Parameter selection
                  selectizeInput("blood_param", "Select Parameter",
                                 choices = param_choices,
                                 multiple = FALSE),
                  
                  # Grouping selection
                  radioButtons("grouping", "Group By",
                               choices = grouping_choices),
                  
                  # Secondary color option
                  selectInput("color_by", "Color Points By",
                              choices = color_by_choices),
                  
                  # Color scheme selection
                  selectInput("color_scheme", "Color Scheme",
                              choices = color_schemes),
                  
                  # Plot controls
                  checkboxInput("show_ref", "Show Reference Ranges", TRUE),
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
                  plotlyOutput("blood_dist", height = "600px"),
                  verbatimTextOutput("blood_stats")
                )
              )
      ),
      
      tabItem(tabName = "stats",
              fluidRow(
                box(
                  width = 12,
                  title = "Statistical Summary",
                  status = "primary",
                  solidHeader = TRUE,
                  
                  # Add grouping selection for stats
                  radioButtons("stats_grouping", "Compare By",
                               choices = grouping_choices,
                               inline = TRUE),
                  
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