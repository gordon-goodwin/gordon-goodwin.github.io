

# Font Utility Functions & Set-Up -----------------------------------------

# 2 main utility functions for fonts
## a) setup_fonts: Fonts Added + Settings Config
## b) get_font_fams: Ties specified font family names to list items/names

library(showtext) 

# Font Config Function
## Adds Fonts from Google Repo & Specifies FAmily Name & Settings
setup_fonts <- function() {
  
  # Add Google Fonts
  font_add_google("Oswald", family = "oswald", db_cache = FALSE)
  font_add_google("Roboto Slab", family = "roboto_slab", db_cache = FALSE)
  font_add_google("Roboto Condensed", family = "roboto_condensed", db_cache = FALSE)
  font_add_google("Roboto Serif", family = "roboto_serif", db_cache = FALSE)
  font_add_google("Roboto", family = "roboto", db_cache = FALSE)
  font_add_google("Noto Sans", family = "noto_sans", db_cache = FALSE)
  font_add_google("Open Sans", family = "open_sans", db_cache = FALSE)
  font_add_google("Lato", family = "lato", db_cache = FALSE)
  
  # Enable showtext
  showtext_auto(enable = TRUE)
  showtext_opts(dpi = 320, regular.wt = 400, bold.wt = 700)
}



# Font Accessory Function
## Facilitates for usability in plotting
### -> ties each font family name to a list object/item
get_font_families <- function() {
  list(
    title = "roboto_condensed",
    subtitle = "roboto_condensed",
    text = "roboto",
    caption = "noto_sans"
  )
}
