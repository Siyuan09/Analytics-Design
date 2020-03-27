# # ===================================================
# Description: write a function that plots crimes 
#              incidence in Baltimore city
# Data: Baltimore crime data
# Source: https://data.baltimorecity.gov/
# ===================================================

 
# clear everything
rm(list = ls()) 

# libraries 
#   need to install.packages() these
#   let me know if installation does not work
library(maps)
library(maptools)

# download, unzip and read the shape file
url_zip <- 'https://dl.dropboxusercontent.com/s/chyvmlrkkk4jcgb/school_distr.zip'
if(!file.exists('school_distr.zip')) download.file(url_zip, 'school_distr.zip')     # download file as zip
unzip('school_distr.zip')   # unzip in the default folder
schdstr_shp <- readShapePoly('school.shp')  # read shape file
xlim <- schdstr_shp@bbox[1,]
ylim <- schdstr_shp@bbox[2,]

# example of how to use the shape file
#   if there are no error code reading the above, you can directly plot the map of Baltimore (lines within are school districts)
#   we'll be overlaying our plots of crime incidents on this map:
plot(schdstr_shp, axes = T)     # axes = T gives x and y axes


# ======= now let's follow instructions in the pdf file ======

# download and load the crime csv data
#   link is https://dl.dropboxusercontent.com/s/4hg5ffdds9n2nx3/baltimore_crime.csv
url <- 'https://dl.dropboxusercontent.com/s/4hg5ffdds9n2nx3/baltimore_crime.csv'
if (!file.exists('baltimore_crime.csv')) {     
    download.file(url, 'baltimore_crime.csv')
}
df.raw <- read.csv('baltimore_crime.csv', stringsAsFactors = F)



# transform dates and time variables depending on what you need



df.raw$Month <- format(as.Date(df.raw$CrimeDate, format = "%m/%d/%Y"), "%m")
df.raw$Day <- format(as.Date(df.raw$CrimeDate, format = "%m/%d/%Y"), "%d")
df.raw$time <- difftime(as.POSIXlt(df.raw$CrimeTime, format = "%H:%M:%S"), as.POSIXlt("00:00:00", format = "%H:%M:%S"), units="hours")



# split coordinates into longitude and latitude, both as numeric
# note: no for/while/repeat loop

df.raw$Latitude <- as.numeric(substr(df.raw$Location1, start = 2, stop = 14))
df.raw$Longtitude <- as.numeric(substr(df.raw$Location1, start = 16, stop = 29))



# generate geographic and time patterns for crimes with keyword "ASSAULT"
# note: no copy and paste of the same/similar command many times

Assult <- subset(df.raw, grepl("ASSAULT", as.character(df.raw$Description)))

par(mfrow = c(2, 2))
for (i in 0:3) {
    
    plot(schdstr_shp, axes = T, main = paste("Hour:", 6*i,"-", 6*(i+1)))
    par(new = T)
    points(Assult$Longtitude[Assult$time> 6*i & Assult$time <= 6*(i+1)], Assult$Latitude[ Assult$time> 6*i & Assult$time <= 6*(i+1)], col=rgb(1,0,0,0.05), cex = 0.1)
    par(new = F)
    
}

