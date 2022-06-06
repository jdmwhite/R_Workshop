#### Install packages ----
install.packages('magick')

#### Load libraries ----
library(magick)

#### Load in the files ----
imgs <- list.files('output/figs/animation/', full.names = TRUE)
img_list <- lapply(imgs, image_read)

#### Join the images together ----
img_joined <- image_join(img_list)

#### Animate the images ----
img_animated <- image_animate(img_joined, fps = 2)
img_animated

#### Save the animation to file as a gif ----
image_write(image = img_animated, path = 'output/gif/ggplot_animation.gif')
