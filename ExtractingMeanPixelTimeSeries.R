
install.packages("stars")
library(stars) # for flexible handling of spatial objects
library(hrbrthemes)# more access to dynamic ggplot themes
library(lubridate) # for handling date objects

# define the directory where the .tif files are
setwd("/Users/rragankonywa/Downloads/NDVI_ESA_50")

# list all the .tif files in the  directory
files <- list.files(path = getwd(),pattern = ".tif")

# extract the date (time) information from the file names,
# store this information as a DATE object. This will be needeed later for plotting
Dates <- unlist(lapply(files, function(each){
  str_extract(string = each, pattern ="[0-9]{4}[-][0-9]{2}[-][0-9]{2}" )
}))

Dates <- as.Date(Dates)

# make each extent the extent of the first raster
# This step is important because the raster files are not of the same extent,
# this will cause problems when stacking

# this function reshapes all the files to the extent of the 1st file
ReshapeExtent <- function(files){
  for (i in 1:56){
  if(i==1){
    my_raster <- raster(files[i])
  } else {
    new_raster <- raster(files[i])
    extent(new_raster) <- extent(my_raster)
    my_raster <- stack(my_raster, new_raster)
  }
  }
  return(my_raster)
}

ndviStack <- ReshapeExtent(files = files)

dim(ndviStack)

# convert the rasterStack object to a star object
ndvi_stars <- st_as_stars(ndviStack)

#rename the attribute to ndvi

names(ndvi_stars) <- "ndvi"

# compute the mean ndvi value per pixel for each timestamp (aka band in this case) 
mean_ndvi_perPixel_perDate = st_apply(ndvi_stars, "band", mean, na.rm = TRUE)[[1]]

# join the ndvi values with the date object to form a df (only needed for plotting)
ndvi_df <- cbind(Dates, mean_ndvi_perPixel_perDate) %>% 
  as.data.frame()

# ensure the date column is still of class date and not a double/character
str(ndvi_df$Dates)
ndvi_df$mean_ndvi_perPixel_perDate <- as.numeric(ndvi_df$mean_ndvi_perPixel_perDate)
ndvi_df$Dates <- Dates

#subsetting a pixel of interest as follows

# first index is selecting the ndvi values
# second index the 1st dimension (x)
# third index is the second dimension (y)
# 4th index is the 3rd dimension bands in our case the timestamp
First_pixel_Xdim <-  st_apply(ndvi_stars[,1,,],"band", mean, na.rm = TRUE)[[1]]

#make a data frame
ndvi_df_Xdim <- cbind(Dates, First_pixel_Xdim) %>% 
  as.data.frame()
ndvi_df_Xdim$Dates <- Dates
str(ndvi_df_Xdim$Dates)

First_pixel_Ydim <- st_apply(ndvi_stars[,,1,], "band", mean, na.rm = TRUE)[[1]]
#make a data frame
ndvi_df_Ydim <- cbind(Dates, First_pixel_Ydim) %>% 
  as.data.frame()

ndvi_df_Ydim$Dates <- Dates
str(ndvi_df_Ydim$Dates)

combined <- cbind(ndvi_df_Xdim, ndvi_df_Ydim[2])


#plot the data
str(ndvi_df)
 ggplot(ndvi_df, aes(x=Dates)) +
  geom_line( aes(y=mean_ndvi_perPixel_perDate), size=0.5, color= "darkgreen") + # Divide by 10 to get the same range than the temperature
  
  scale_x_date(date_labels = "%m-%b",date_breaks = "1 month")+
  theme_ipsum() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5)
    
  ) +
  
  ggtitle("NDVI Time Series per Pixel") +
  xlab(label = "Time")+
   ylab(label = "Mean NDVI")
 
 #plot 2
 
 ggplot(combined, aes(x=Dates)) +
   geom_line( aes(y=First_pixel_Xdim), size=0.5, color= "darkgreen") + 
   geom_line( aes(y=First_pixel_Ydim), size=0.5, color= "red")+
   scale_x_date(date_labels = "%d-%b",date_breaks = "1 month")+
   scale_y_continuous(
     
     # Features of the first axis
     name = "NDVI First Pixel in x-Dimension",
     
     # Add a second axis and specify its features
     sec.axis = sec_axis(~.*1, name="NDVI First Pixel in y-Dimension")
   ) +
   theme_ipsum() +
   theme(
     axis.title.y = element_text(color = "darkgreen", size=13),
     axis.title.y.right = element_text(color = "red", size=13),
     plot.title = element_text(hjust = 0.5),
     axis.title.x = element_text(hjust = 0.5)
     
   ) +
   
   ggtitle("NDVI Time Series First Pixel in x and y Dimensions") +
   xlab(label = "Time")
   #ylab(label = "Mean NDVI")
 
  

















