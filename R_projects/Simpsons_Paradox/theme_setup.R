

# Theme Utility Functions & Set-Up ----------------------------------------


# Default & Dynamic/Customizable Color Specifications
## Uses list to tie core plot elements to specified default colors
### -> adds "palette" list item for dynamic palette input as well
get_theme_colors <- function(palette = NULL) {
  list(
    background = "#f5f5f2",
    title      = "gray20",
    subtitle   = "gray30",
    text       = "gray30",
    caption    = "gray40",
    palette    = palette  
  )
}




# Base Theme
## -> Core elements that are common across iterations/contexts
### -> Args for Colors and Fonts List Item References
create_base_theme <- function(colors = get_theme_colors()) {
  
  theme_minimal(base_family = fonts$text) +
    theme(
      
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      legend.position       = "top",
      
      # Background elements filled using specified color palette for each element
      plot.background  = element_rect(fill = colors$background, color = colors$background),
      panel.background = element_rect(fill = colors$background, color = colors$background),
      
      # Panel Grid
      panel.grid.major = element_line(color = "grey88"),
      panel.grid.minor = element_line(color = "grey88"),
      
      # Facet Panels/Strips
      strip.background = element_roundrect(fill = "#1a318b",color = NA, r = 0.15),
      strip.text = element_text(color = "white", face = "bold"),
      
      # Set Plot margins
      # plot.margin = margin(t = 10, r = 20, b = 10, l = 20),
      
      # Fixed axis text & title elements tied to specified colors and fonts
      axis.text  = element_text(
        family = fonts$text,
        size   = rel(0.79),
        color  = colors$text
      ),
      axis.title = element_text(
        family = fonts$subtitle,
        size   = rel(0.93),
        face   = "bold",
        color  = colors$text
      ),
      plot.title = element_text(
        family = fonts$title,
        face = "bold",
        color = colors$title
      ),
      plot.subtitle = element_text(
        family = fonts$title,
        color = colors$subtitle
      )
      
      
    )
}




# Extend Base Theme - allows for specifying non-core theme adjustments
extend_base_theme <- function(base_theme, extended_elements) {
  
  # Returns Base Theme & Adjustments/Additions
  base_theme + extended_elements
}

