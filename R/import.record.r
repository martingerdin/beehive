#' import record function
#'
#' this function imports a record from a file
#' @param file_path path to file with record, no default
#' @export
## * content
## ** declare
import.record <- function(
                          file_path
                          )
{
## ** create record from JSON string
    record <- as.data.frame(fromJSON(file_path), stringsAsFactors = FALSE)
    return(record)
## * end    
}
