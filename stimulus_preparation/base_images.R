

# Images were obtained from three sources: 
# (I) MultiPic (Duñabeitia et al., 2018,
#   https://doi.org/10.1080/17470218.2017.1310261);
# (II) https://www.flaticon.com;
# (III) https://www.freepik.com

# All images were stored in the directory 'session_materials/images/', 
# and further distributed in the appropriate folders corresponding to 
# each study site.

# The code below is used to frame every image on a white, square 
# background to facilitate the integration of images with other 
# images, and the final composition in OpenSesame.

library(dplyr)
library(stringr)
library(magick)

# Norway site

image_dir = 'session_materials/images/Norway site/'

base_images = 
  list.files(image_dir) %>% 
  data.frame() %>% 
  # Exclude background image, happy/sad faces and 
  # compound images (i.e., those with an underscore).
  filter(!str_detect(pattern = '^background.png$|happy-face|sad-face|_', .)) %>% 
  pull(1)

# Iterate over images to trim off their backgrounds 
# and to overlay them on white backgrounds. 

for(i in 1:length(base_images)) {
  
  # Read in image
  base_image = image_read(paste0(image_dir, base_images[i]))
  
  # Trim background if possible
  try({ base_image = base_image %>% image_trim() }, silent = TRUE)
  
  # Resize
  base_image = base_image %>% image_scale('720x720')
  
  # Place image in the center of the white background
  image_read(paste0(image_dir, 'background.png')) %>%
    image_composite(base_image, gravity = 'center') %>%
    
    # Save
    image_write(paste0(image_dir, base_images[i]))
}


# Spain site

image_dir = 'session_materials/images/Spain site/'

base_images = 
  list.files(image_dir) %>% 
  data.frame() %>% 
  # Exclude background image, happy/sad faces and 
  # compound images (i.e., those with an underscore).
  filter(!str_detect(pattern = '^background.png$|happy-face|sad-face|_', .)) %>% 
  pull(1)

# Iterate over images to trim off their backgrounds 
# and to overlay them on white backgrounds. 

for(i in 1:length(base_images)) {
  
  # Read in image
  base_image = image_read(paste0(image_dir, base_images[i]))
  
  # Trim background if possible
  try({ base_image = base_image %>% image_trim() }, silent = TRUE)
  
  # Resize
  base_image = base_image %>% image_scale('720x720')
  
  # Place image in the center of the white background
  image_read(paste0(image_dir, 'background.png')) %>%
    image_composite(base_image, gravity = 'center') %>%
    
    # Save
    image_write(paste0(image_dir, base_images[i]))
}

