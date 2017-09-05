#' @description 
#' Function that reads a directory full of files and reports the number of 
#' completely observed cases in each data file.
#' 
#' @param directory Character vector of length 1 indicating the location of CSV files
#' @param id        Integer vector indicating the monitor ID numbers to be used
#' 
#' @return data frame, where-
#'         first column is name of the file
#'         second column is number of complete cases
#'         id nobs
#'         1  117
#'         2  1041
#'         .. ...
#'  
#' @examples 
#' complete("specdata", 30:25)
#' complete("specdata", c(2, 4, 8, 10, 12))

complete <- function(directory, id = 1:332){
    
    data <- data.frame()
    for (i in id){
        file <- paste(sprintf("%03d",i), ".csv", sep = "")
        
        df <- read.csv(file.path(directory, file))
        rows <- nrow(df[!is.na(df$sulfate) & !is.na(df$nitrate),])
        data <- rbind(data, c(i, rows))
    }
    colnames(data) <- c("id", "nobs")
    data
}

complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)