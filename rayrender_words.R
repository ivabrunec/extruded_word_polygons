# Extruding word polygons using rayshader/rayrender

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)
library(ggplot2)
library(rayshader)
library(rayrender)
library(raster)
library(showtext)

font_add_google(name = 'Roboto', family = 'Roboto',regular.wt = 900)
showtext_auto()

# logic:
# plot words & save as .tif
# read in as raster
# plot using rayshader & then render_highquality

# plot word & save as tif
# plot on black background so the background height is 0
# the rgb codes look very weird but they actually contain the height
# repeating same number 3 times means it's interpreted as that value when the raster is read in

# THIS IS THE MANUAL PART: 
# you have to put together the words in a way that makes sense to you. 
# you could also do a wordcloud here

tiff("cities.tiff", units="in", width=3, height=3, res=300)
ggplot() +
  annotate("text", x = 50, y = 80, label = "HAMILTON", 
           color = rgb(26.4, 26.4, 26.4, maxColorValue = 255), family = 'Roboto', size = 18, angle = 0) +
  annotate("text", x = 48, y = 70, label = "VANCOUVER", 
           color = rgb(25.2, 25.2, 25.2, maxColorValue = 255), family = 'Roboto', size = 18, angle = 0) +
  annotate("text", x = 80, y = 63, label = "TORONTO", 
           color = rgb(23.7, 23.7, 23.7, maxColorValue = 255), family = 'Roboto', size = 18, angle = 90) +
  annotate("text", x = 56, y = 60, label = "LONDON", 
           color = rgb(19.72, 19.72, 19.72, maxColorValue = 255), family = 'Roboto', size = 18, angle = 0) +
  annotate("text", x = 88, y = 50, label = "OTTAWA", 
           color = rgb(19.67, 19.67, 19.67, maxColorValue = 255), family = 'Roboto', size = 18, angle = 90) +
  annotate("text", x = 21, y = 40, label = "MONTREAL", 
           color = rgb(19.1, 19.1, 19.1, maxColorValue = 255), family = 'Roboto', size = 18, angle = 90) +
  annotate("text", x = 50, y = 50, label = "EDMONTON", 
           color = rgb(17.4, 17.4, 17.4, maxColorValue = 255), family = 'Roboto', size = 18, angle = 0) +
  annotate("text", x = 12, y = 30, label = "CALGARY", 
           color = rgb(16.75, 16.75, 16.75, maxColorValue = 255), family = 'Roboto', size = 18, angle = 90) +
  annotate("text", x = 54, y = 38, label = "CANADA AVG.", 
           color = rgb(15.9, 15.9, 15.9, maxColorValue = 255), family = 'Roboto', size = 18, angle = 0) +
  annotate("text", x = 60, y = 27, label = "WINNIPEG", 
           color = rgb(15.1, 15.1, 15.1, maxColorValue = 255), family = 'Roboto', size = 18, angle = 0) +
  
  theme_void() +
  theme(plot.background = element_rect(fill = 'black', color = NA),
        panel.background = element_rect(fill = 'black', color = NA)) +
  xlim(c(0,100)) +
  ylim(c(0,100))
dev.off()

# read in as raster
word_raster <- raster('cities.tiff')
plot(word_raster)
# color codes are converted to height map, which makes sense
# we just have to convert to matrix 
word_mat <- raster_to_matrix(word_raster)
# after testing: you have to remove the last row because it's tall for some reason?
word_mat <- word_mat[,-(nrow(word_mat))]

word_mat |>
  height_shade(  texture = (grDevices::colorRampPalette(c("#f1e9e7", "#f1e9e7", "#eb8a0d","#dc2723")))(256),) |>
  plot_3d(word_mat, zscale = .05, solid = F,
          windowsize = c(1000,1000))

render_highquality('highres_test_green_bg.png',
                   ground_material = rayrender::diffuse(color = 'grey90'),
                   min_variance = 0,
                   lightdirection = 320,
                   lightaltitude = 65,
                   lightintensity = 700,
                   clamp_value = 5,
                   samples = 390)

# ta-da

# additional attempt ####
# convert raster to points using raster's built in function & plot with rayrender directly
# this DOES NOT WORK. it's here in case I ever figure it out and come back to it.

# follow logic from: https://www.rayrender.net/reference/extruded_polygon.html
# just need x & y coordinates
word_points <- as.data.frame(rasterToPoints(word_raster))
# same as before, for some reason there's a line at the bottom so we remove that first,
# then remove all coordinates with height ('test_1') = 0
word_points <- word_points |> 
  filter(y > 100) |>
  filter(test_1 != 0) |>
  select(x, y)

# center roughly at 0,0
word_points$x <- (word_points$x - 450) * .1
word_points$y <- (word_points$y - 440) * .1

# this DOES NOT work! the points have to all be connected & at this point, I can't figure out how to plot the letters separately
plot(word_points)

generate_ground(depth=-0.01,
                material = diffuse(color="grey50",checkercolor="grey20")) %>%
  add_object(extruded_polygon(word_points, top = .5, bottom = 0,holes = nrow(word_points) + 1,
                              material=diffuse(color="red",sigma=90))) %>%
  #add_object(sphere(y=4,x=-3,z=-3,material=light(intensity=30))) %>%
  render_scene(parallel=TRUE,lookfrom = c(0,2,4),samples=128,lookat=c(0,0,0),fov=30)

