library(magick)
library(sf)
library(tidyverse)
library(InewsTheme)

gbelt <- st_read("Gbelt_fixed.shp")

## Greenbelt shapes
for(i in 1:nrow(gbelt)){
  gbel <- gbelt[i, ]
  write_sf(gbel, paste0("shapes//", gbel$lad19nm, ".shp"), delete_datasource = T)
  
}

## Satellite Images
for (i in 1:nrow(gbelt)){
  belt <- gbelt[i, ] %>% st_transform(4326)
  box <- st_bbox(belt)
  url <- paste0("https://tiles.maps.eox.at/wms?service=wms&request=getmap&version=1.1.1&layers=s2cloudless-2019&",
                "bbox=", box$xmin-.05, ",", box$ymin-.05, ",", box$xmax+.05, ",", box$ymax+.05,
                "&width=4096&height=4096&srs=epsg:4326")
  download.file(url, paste0("satellite//", belt$lad19nm, ".jpg"), mode = "wb")
}

#time to create the GDAL script
if (file.exists("script.txt")) { file.remove("script.txt")}

#first, georeference the images - convert them from flat jpgs to tifs that know where on the #globe the image is
for (i in 1:nrow(gbelt)) {
  shape <- gbelt[i, ]
  box <- st_bbox(shape)
  
  file_origin <- paste0(getwd(), '/satellite/', shape$lad19nm, '.jpg')
  file_temp <- paste0(tempdir(), "/", shape$lad19nm, '.jpg')
  file_final <- paste0(tempdir(), "/", shape$lad19nm, '_ref.tif')
  
  cmd1 <- paste0("gdal_translate -of GTiff",
                 " -gcp ", 0, " ", 0, " ", box$xmin, " ", box$ymax,
                 " -gcp ", 4096, " ", 0, " ", box$xmax, " ", box$ymax,
                 " -gcp ", 0, " ", 4096, " ", box$xmin, " ", box$ymin,
                 " -gcp ", 4096, " ", 4096, " ", box$xmax, " ", box$ymin,
                 ' "', file_origin, '"',
                 ' "', file_temp, '"')
  
  cmd2 <- paste0("gdalwarp -r near -tps -co COMPRESS=NONE  -t_srs EPSG:4326",
                 ' "', file_temp, '"',
                 ' "', file_final, '"')
  
  write(cmd1,file="script.txt",append=TRUE)
  write(cmd2,file="script.txt",append=TRUE)
}

#next, crop the georeferenced tifs to the outlines in the individual shapefiles
for (i in  1:nrow(gbelt)) {
  shape <- gbelt[i, ]
  file_shp <- paste0(getwd(), '/shapes/', shape$lad19nm, '.shp')
  file_orig <-  paste0(tempdir(), "/", shape$lad19nm, '_ref.tif')
  file_crop <-  paste0(tempdir(), "/", shape$lad19nm, '_crop.tif')
  
  cmd <- paste0("gdalwarp -cutline",
                ' "', file_shp, '"',
                " -crop_to_cutline -of GTiff  -dstnodata 255",
                ' "', file_orig, '"',
                ' "', file_crop, '"',
                " -co COMPRESS=LZW -co TILED=YES --config GDAL_CACHEMAX 2048 -multi")
  
  write(cmd,file="script.txt",append=TRUE)
}

#reproject the shapes to a reasonable projection, so they're not as warped and stretched
for (i in  1:nrow(gbelt)) {
  shape <- gbelt[i, ]
  
  center <- st_centroid(shape) %>% st_coordinates
  utm <- as.character(floor((center[1] + 180) / 6) + 1)
  if (nchar(utm) == 1) {utm <- paste0("0", utm)}
  if (center[2] >=0) {
    epsg <- paste0("326", utm)
  } else {
    epsg <- paste0("327", utm)
  }
  
  file_orig <- paste0(tempdir(), "/", shape$lad19nm, '_crop.tif')
  file_mod <- paste0(tempdir(), "/", shape$lad19nm, '_reproj.tif')
  
  cmd <- paste0("gdalwarp -t_srs EPSG:", epsg,
                ' "', file_orig, '"',
                ' "', file_mod, '"')
  
  write(cmd,file="script.txt",append=TRUE)
}

files <- list.files(tempdir(), pattern = "_reproj.tif")
for (i in 1:length(files)) {
  file.rename(from=paste0(tempdir(), "/", files[i]),to= paste0("./reproj/", files[i]))
}

#get the average color of each file
files <- list.files("./reproj/")
colors <- NULL

for (i in 1:length(files)) {
  #remove the white background of the image
  img <- image_read(paste0("./reproj/", files[i]))
  img <- image_transparent(img, "#ffffff", fuzz = 0)
  
  #convert to a 1x1 pixel as a trick to finding the avg color. ImageMagick will average the 
  #colors out to just 1.
  img <- image_scale(img,"1x1!")
  
  #get the color of the pixel
  avgcolor <- image_data(img, channels = "rgba")[1:3] %>% paste0(collapse="")
  
  #save it in a running list
  colors <- bind_rows(colors, data.frame(name = gsub("_reproj.tif", "", files[i]), color = paste0("#", avgcolor)))
}

#save the color file if you need it later
write.csv(colors, "colors.csv", row.names = F)

colours <- read.csv("colors.csv")

cgbelt <- merge(gbelt, colours, by.x="lad19nm", by.y="name") %>%
  st_transform(4326)

lads <- st_read("Local_Authority_Districts_(May_2021)_UK_BGC.shp") %>%
  st_transform(4326)

ggplot() +
  geom_sf(data = lads, size=0, colour=NA) +
  geom_sf(data=cgbelt, aes(fill = I(color)),color = alpha("#fffbf2", .1)) +
  theme_inews_map()

save_inews("mapproj.png", type = "map")