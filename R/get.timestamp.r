#' get timestamp function
#'
#' this function takes a POSIXct vector as returned by Sys.date() and returns a character vector with only numbers
#' @param date the POSIXct vector, no default
#' @export
## * content
## ** declare
get.timestamp <- function(
                          date
                          )
{
## ** perform replacement
    timestamp <- date
    for(p in c("-", ":", " ")) timestamp <- gsub(p, "", timestamp)
## ** return timestamp    
    return(timestamp)
## * end    
}    
