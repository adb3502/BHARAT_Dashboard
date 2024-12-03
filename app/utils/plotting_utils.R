library(wesanderson)
library(plotly)
library(scales)

# Color palettes
zissou_palette <- wes_palette("Zissou1")
darjeeling_palette <- wes_palette("Darjeeling1")
royal_palette <- wes_palette("Royal1")

# Function to get colors based on scheme and groups needed
get_color_palette <- function(scheme, n) {
  colors <- switch(scheme,
                   "zissou" = wes_palette("Zissou1"),
                   "darjeeling" = wes_palette("Darjeeling1"),
                   "royal" = wes_palette("Royal1"),
                   wes_palette("Zissou1")  # default
  )
  
  if(n <= length(colors)) {
    return(colors[1:n])
  } else {
    colorRampPalette(colors)(n)
  }
}

# Common theme for ggplot
theme_bharat <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(
      text = element_text(family = "Manrope"),
      plot.title = element_text(hjust = 0.5, size = base_size + 2, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = base_size - 2),
      axis.title = element_text(size = base_size - 2),
      axis.text = element_text(size = base_size - 4),
      legend.title = element_text(size = base_size - 2),
      legend.text = element_text(size = base_size - 4),
      strip.text = element_text(size = base_size - 2)
    )
}

# Function for creating distribution plots
create_distribution_plot <- function(data, x, fill, title) {
  ggplot(data, aes(x = !!sym(x), fill = !!sym(fill))) +
    geom_density(alpha = 0.7) +
    scale_fill_manual(values = darjeeling_palette) +
    theme_bharat() +
    labs(title = title)
}

# Function for creating box plots
create_box_plot <- function(data, x, y, fill, title) {
  ggplot(data, aes(x = !!sym(x), y = !!sym(y), fill = !!sym(fill))) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    geom_jitter(width = 0.2, alpha = 0.5, size = 1) +
    scale_fill_manual(values = zissou_palette) +
    theme_bharat() +
    labs(title = title)
}

# Function for creating bar plots
create_bar_plot <- function(data, x, y, fill, title) {
  ggplot(data, aes(x = !!sym(x), y = !!sym(y), fill = !!sym(fill))) +
    geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
    scale_fill_manual(values = royal_palette) +
    theme_bharat() +
    labs(title = title)
}

# Function for correlation heatmaps
create_correlation_heatmap <- function(data, variables, title) {
  cor_matrix <- cor(data[variables], use = "pairwise.complete.obs")
  
  plot_ly(
    x = variables,
    y = variables,
    z = cor_matrix,
    type = "heatmap",
    colors = zissou_palette,
    colorbar = list(title = "Correlation")
  ) %>%
    layout(
      title = list(
        text = title,
        font = list(family = "Manrope", size = 16)
      ),
      xaxis = list(tickangle = -45),
      margin = list(b = 100)
    )
}

# Function for radar charts
create_radar_chart <- function(data, variables, values, title) {
  plot_ly(
    type = 'scatterpolar',
    r = values,
    theta = variables,
    fill = 'toself',
    line = list(color = zissou_palette[1]),
    fillcolor = paste0(zissou_palette[1], "80")
  ) %>%
    layout(
      polar = list(
        radialaxis = list(
          visible = TRUE,
          range = c(0, max(values) * 1.1)
        )
      ),
      showlegend = FALSE,
      title = list(
        text = title,
        font = list(family = "Manrope", size = 16)
      )
    )
}