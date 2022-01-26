#' Calculate Basal Area from Diameter
#'
#' This function calculates basal area from 
#' diameter-at-breast height.
#'
#' @param dbh DBH measurements
#' @return A matrix of the infile
#' @export
calc_ba <- function(dbh){
    
    (dbh/2) * pi
    
}