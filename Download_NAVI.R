library(cptcity)
library(raster)
library(stars)
library(rgee)
library(sf)
library(rgdal)
library(tiff)
library(googledrive)
library(stringr)

ee_Initialize(drive = TRUE)
#ee_check()


#Define a region of interest

roi <- ee$Geometry$Polygon(
  proj = "EPSG:4326",
  geodesic = FALSE,
  coords = list(
    c(19.5696, -33.660),
    c(18.146, -34.418),
    c(19.5696, -34.418),
    c(18.146, -33.660)
  )
)

modis_ndvi <- ee$ImageCollection("MODIS/006/MOD13A2")


#MODIS makes it simple to filter out poor quality pixels thanks to a quality control bits band (DetailedQA). 
#The following function helps us to distinct between good data (bit == …00) and marginal data (bit != …00).

getQABits <- function(image, qa) {
  # Convert binary (character) to decimal (little endian)
  qa <- sum(2^(which(rev(unlist(strsplit(as.character(qa), "")) == 1))-1))
  # Return a mask band image, giving the qa value.
  image$bitwiseAnd(qa)$lt(1)
}




#Using getQABits we construct a single-argument function (mod13A2_clean) that is used to map over all the images of the collection (modis_ndvi).
mod13A2_clean <- function(img) {
  # Extract the NDVI band
  ndvi_values <- img$select("NDVI")
  
  # Extract the quality band
  ndvi_qa <- img$select("SummaryQA")
  
  # Select pixels to mask
  quality_mask <- getQABits(ndvi_qa, "11")
  
  # Mask pixels with value zero.
  ndvi_values$updateMask(quality_mask)
}

#Filter the collection (modis_ndvi) by a date range.

ndvi_composite <- modis_ndvi$
  filter(ee$Filter$date('2001-01-01', '2001-02-01'))$
  #filter(ee$Filter$lte("CLOUDY_PIXEL_PERCENTAGE", 20))$
  map(mod13A2_clean)



#OPTIONAL: Use Map to display the results in an interactive way.
#It only works for display one image, still needs to develop
#Do not run it yet
scale <- 0.0001
Map$setCenter(lon =18.83606, lat = -34.06176, zoom = 7)
Map$addLayer(
  eeObject = ndvi_composite, 
  visParams = list(
    min = 0.2 / scale, 
    max = 0.7 / scale,
    palette = cpt("grass_ndvi", 10)
  )
) + Map$addLayer(roi)



# checking the exsisting datasets in google drive
# path --- a single folder on Google Drive whose contents you want to list
# pattern --- items whose names match this regular expression are returned.
raster_find <- drive_ls(pattern = "20",path='rgee_backup')


# collecting the date of exisiting datasets
existing_date <- as.character(as.list(raster_find$name))
existing_date <- substr(existing_date,1,10)


# collecting the date of datasets you want to export 
desire_date <- as.list(ee_get_date_ic(ndvi_composite)[,1])
desire_date <- substr(desire_date,19,29)




# a loop to download data
`%!in%` <- Negate(`%in%`)
for (i in 1:length(desire_date)){
  if (desire_date[i] %!in% existing_date){
    missing_date<-desire_date[i] # data that you want to export is existing on the folder
    missing_date <- str_replace_all(missing_date,'_','-')
    print(missing_date)
    
    ndvi_composite <- modis_ndvi$
      filter(ee$Filter$date( missing_date,as.character(as.Date(missing_date)+1)))$ # Specifies the date
      map(mod13A2_clean)
    
    mod_ndvi <- ee_imagecollection_to_local(
      ic = ndvi_composite,
      scale = 100,
      region = roi,
      via = 'drive'
    )
    
  }
}



# access to your google drive
# path --- a single folder on Google Drive whose contents you want to list
# pattern --- items whose names match this regular expression are returned.
raster_find <- drive_ls(pattern = "2001_01_17",path='rgee_backup')
raster_path <- drive_get(raster_find$name)

# download data to your directory
# setwd()
# getwd()
raster_download<-drive_download(raster_path$name,type = "tif",overwrite = TRUE)

# produce raster objects
imported_raster <- raster(raster_download$local_path, as.is=TRUE)
imported_raster <- aggregate(imported_raster,fact=2)
plot(imported_raster)
