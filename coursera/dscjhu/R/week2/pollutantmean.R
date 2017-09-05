#' @description 
#' Find out mean value of PM (particulate matter) specified pollutant from specified
#' air monitoring stations
#' 
#' @param directory Directory where all the data files are present
#' @param pollutant Name of the pollutant "sulfate" or "nitrate"
#' @param id        Numeric IDs of Air-Monitoring Stations to be considered
#' 
#' @return Mean value of the specified pollutant
#'  
#' @examples 
#' pollutantmean("specdata", "sulfate", 1:10)
#' pollutantmean("specdata", "nitrate", 70:72)

pollutantmean <- function(directory, pollutant, id = 1:332){
    
    # Filenames are of format "001.csv"
    filenames <- paste(sprintf("%03d",id), ".csv", sep = "")
    
    data <- data.frame()
    for (file in filenames){
        df <- read.csv(file.path(directory, file), header=TRUE)
        data <- rbind(data, df)
    }
    
    mean(data[[pollutant]], na.rm=TRUE)
}

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)