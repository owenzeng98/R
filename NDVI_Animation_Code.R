library(magick)
library(rgee)
library(sf)
library(rgeeExtra)
ee_Initialize()
#https://github.com/r-spatial/rgee






# Retrieve the MODIS Terra Vegetation Indices 16-Day Global 1km dataset
col <- ee$ImageCollection('MODIS/006/MOD13A2')$select('NDVI')

# Define the regional bounds of animation and a mask to clip the NDVI data by.

mask <- ee$FeatureCollection('USDOS/LSIB_SIMPLE/2017')


region <- ee$Geometry$Polygon(
  coords = list(
    c(19.5696, -33.660),
    c(18.146, -34.418),
    c(19.5696, -34.418),
    c(18.146, -33.660)
  )
)

# group images by composite date
col <- col$map(function(img) {
  doy <- ee$Date(img$get('system:time_start'))$getRelative('day', 'year')
  img$set('doy', doy)
})
distinctDOY <- col$filterDate('2011-01-01', '2016-01-01')


#Define a filter that identifies which images from the complete collection match the DOY from the distinct DOY collection.

filter <- ee$Filter$equals(leftField = 'doy', rightField = 'doy')

#Define a join; convert the resulting FeatureCollection to an ImageCollection.

join <- ee$Join$saveAll('doy_matches')
joinCol <- ee$ImageCollection(join$apply(distinctDOY, col, filter))


#Define RGB visualization parameters.

visParams = list(
  min = 0.0,
  max = 8000,
  palette = c(
    '5d3519', 'CE7E45', 'DF923D', 'F1B555', '68B06D', '5A9A60', '518C57',
    '4C8553', '335D39','2F5635', '274A2C', '1F3D24', '1C3720', '18311D', 
    '142C19', '112615'
    
    
  )
)

#color picker
#http://tristen.ca/hcl-picker/#/hlc/22/1/112615/68B06D



#Create RGB visualization images for use as animation frames.

rgbVis <- joinCol$map(function(img) {
  do.call(img$visualize, visParams) %>%
    ee$Image$clip(mask)
})


#Define GIF visualization parameters.

gifParams <- list(
  region = region,
  dimensions = 600,
  crs = 'EPSG:4326',
  framesPerSecond = 15
)



# Get month names

dates_modis_mabbr <- distinctDOY %>%
  ee_get_date_ic %>% # Get Image Collection dates
  '[['("time_start") %>% # Select time_start column
  lubridate::month() %>% # Get the month component of the datetime
  '['(month.abb, .) # subset around month abbreviations


# Get years
dates_modis_mabbr_2 <- distinctDOY %>%
  ee_get_date_ic %>% # Get Image Collection dates
  '[['("time_start") %>% # Select time_start column
  lubridate::year() # Get the month component of the datetime



# render the GIF animation and add texts.
animation <- ee_utils_gif_creator(rgbVis, gifParams)
animation %>%
  ee_utils_gif_annotate(
    text = "NDVI: MODIS/006/MOD13A2",
    size = 10, color = "white",
    location = "+10+305"
  ) %>%
  ee_utils_gif_annotate(
    text = dates_modis_mabbr,
    size = 15,
    location = "+5+0",
    color = "white",
    font = "arial",
    boxcolor = "#000000"
  ) %>%
  ee_utils_gif_annotate(
    text = dates_modis_mabbr_2,
    size = 15,
    location = "+40+0",
    color = "white",
    font = "arial",
    boxcolor = "#000000"
  )  #-> animation_wtxt


# ee_utils_gif_save(animation_wtxt,path='NDVI_small_11to15.gif')

# remove the above two number signs (#'s) when export the gif   


