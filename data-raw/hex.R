library(hexSticker)
library(stringr)
library(rsvg)
## read in file
file <- "data-raw/catlogo.svg"
# Render with rsvg into png
svgdata <- readLines(file)
svg_string <- paste(svgdata, collapse = "\n")


nold_colors <- c(
  "#CCE4ED", # st0_color
  "#5f1905", # st1_color
  "#D9EBF4", # st2_color
  "#ce370b", # st3_color
  "#F45F34", # st4_color
  #  "#C15B65", # st6_color
  #  "#A54653", # st7_color
  "#842307" # st9_color
)
# make a purpley color scheme
new_colors <- c(
  "#E0BBE4", # st0_color
  "#957DAD", # st1_color
  "#D291BC", # st2_color
  "#FEC8D8", # st3_color
  "#FFDFD3", # st4_color
  #  "#C15B65", # st6_color
  #  "#A54653", # st7_color
  "#F67280" # st9_color
)

color_replacements <- setNames(new_colors, nold_colors)
# Use str_replace_all to replace all occurrences of the old color
modified_svg_string <- str_replace_all(svg_string, color_replacements)
writeLines(modified_svg_string, "data-raw/recoloredcat.svg")

Sys.sleep(1)

rsvg::rsvg_png("data-raw/recoloredcat.svg",
  "data-raw/recoloredcat.png",
  width = 800
)


file.remove("man/figures/hex.png")
Sys.sleep(1)

sticker(
  subplot = "data-raw/recoloredcat.png",
  package = "pedigreedata", p_size = 20, s_x = 1, s_y = .84, s_width = .6,
  h_fill = "#0fa1e0",
  h_color = "#333333", p_color = "white",
  filename = "man/figures/hex.png"
)
