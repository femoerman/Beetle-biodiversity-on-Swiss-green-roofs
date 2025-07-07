# 1. Theme definition for HTML / Bootstrap
# 2. Theme definition for plots

# 1. HTML/Bootstrap Theme definition
# to use either variant 1 or 2, comment the other variant

# Variant 1: just define some colours ------
# define your colours here:
# bg -> background
# fg -> foreground
# primary -> primary colour for buttons etc.
# more on this topic: 
if (USE_BOOTSTRAP) {
  # light mode
  light <- bs_theme(version = 5) 
  # dark mode
  dark <- bs_theme(version = 5, bg = "black", fg = "white", primary = "purple")
}

# Variant 2: use a bootswatch theme. ------
# See what is available here: https://bootswatch.com/
# “cerulean”, “cosmo”, “cyborg”, “darkly”, “flatly”, “journal”, “litera”, 
# “lumen”, “lux”, “materia”, “minty”, “morph”, “pulse”, “quartz”, “sandstone”, 
# “simplex”, “sketchy”, “slate”, “solar”, “spacelab”, “superhero”, “united”, 
# “vapor”, “yeti”, “zephyr”
if (USE_BOOTSTRAP) {
  # light mode
  light <- bs_theme(version = 5, bootswatch = "cosmo") 
  # dark mode
  dark <- bs_theme(version = 5, bootswatch = "superhero")
}


# 2. ggplot theme -----
# define here your standard theme how you want plots to look like
ggplot_theme <- theme_minimal() + theme(axis.title = element_text(size = 20), axis.text = element_text(size=15), legend.text=element_text(size=15),
                                        legend.title=element_text(size=20), legend.position = "bottom")
theme_set(ggplot_theme)


# Call thematic_shiny() prior to launching the app, to change 
# R plot theming defaults for all the plots generated in the app
thematic_shiny(font = "auto")