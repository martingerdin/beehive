#' get vector function
#'
#' this function takes as input a single string of values or value labels and returns a vector without escape characters or whitespace
#' @param x the string, no default
#' @export
## * content
## ** declare
get.vector <- function(
                       x
                       )
{
## ** split and remove whitespace    
    split <- unlist(strsplit(gsub("\\\"", "", x), split = ","))
    for (i in seq_along(split)) split[i] <- gsub("^ ", "", split[i])
## ** return split    
    return(split)
## * end    
}
