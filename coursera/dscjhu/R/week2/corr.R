printf <- function(...) invisible(print(sprintf(...)))

#' @description 
#' Function that takes a directory of data files and a threshold for complete 
#' cases and calculates the correlation between sulfate and nitrate for monitor 
#' locations where the number of completely observed cases (on all variables) 
#' is greater than the threshold.
#' 
#' @param directory Character vector of length 1 indicating the location of CSV files
#' @param threshold Numeric  vector of length 1, indicating the number of completely 
#'                  observed observations required to compute the correlation between
#'                  nitrate and sulfate. Default is 0
#' 
#' @return vector of correlations for the monitors that meet the threshold requirement. 
#'         If no monitors meet the threshold requirement, 
#'         then the function should return a numeric vector of length 0
#'  
#' @examples 
#' complete("specdata", 30:25)
#' complete("specdata", c(2, 4, 8, 10, 12))

corr <- function(directory, threshold = 0){
    
    files <- list.files(directory)
    correlations <- numeric()
    
    data <- data.frame()
    for (file in list.files(directory)){
        cor_sul_nit <- 0
        df <- read.csv(file.path(directory, file))

        complete_cases <- df[!is.na(df$sulfate) & !is.na(df$nitrate),]
        
        if (nrow(complete_cases) > threshold){
            cor_sul_nit <- cor(complete_cases$sulfate, complete_cases$nitrate)
            correlations <- c(correlations, cor_sul_nit)
        }

        # printf("File: %s. Complete Cases: %d. Correlation: %f", file, nrow(complete_cases), cor_sul_nit)
    }
    
    correlations
}

cr <- corr("specdata", 150)
head(cr)
summary(cr)
length (cr)

cr <- corr("specdata", 400)
head(cr)
summary(cr)

cr <- corr("specdata", 5000)
summary(cr)
length (cr)

cr <- corr("specdata")
summary(cr)
length (cr)

