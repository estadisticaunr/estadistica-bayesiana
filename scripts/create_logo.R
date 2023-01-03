library(here)
library(hexSticker)
library(magick)
library(showtext)

img <- image_read(here("utils", "imgs", "bayes-02.png"))
font_add("Roboto Slab", here("utils", "fonts", "roboto_slab_variable.ttf"))
showtext_auto()

colors <- c("#1e3d59", "#ff6e40", "#ffc13b", "#f5f0e1")

sticker(
  img, 
  package="Estadística Bayesiana", 
  s_x = 1, 
  s_y = 0.95, 
  s_width = 1.5,
  s_height = 1.5,
  p_x = 1,
  p_y = 1.5,
  p_family = "Roboto Slab",
  p_size = 11,
  p_color = "#444444",
  h_size = 0.9,
  h_fill = colors[4],
  h_color = colors[3],
  url = "Escuela de Estadística - UNR",
  u_size = 5,
  u_family = "Roboto Slab",
  u_x = 1,
  u_color = "#555555",
  dpi = 300,
  asp = 1,
  filename = here("utils", "imgs", "logo.png")
) |> print()
