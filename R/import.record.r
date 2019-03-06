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
## ** get record
    text_record <- readtext::readtext(file_path)$text
## ** create record from JSON string
    json <- tryCatch(jsonlite::fromJSON(text_record),
                     warning = function(w) {
                         print(w)
                         print(file_path)
                         stop()
                     },
                     error = function(e) {
                         print(file_path)
                         print(e)
                         preprocessed <- preprocess.record(text_record)
                         json <- jsonlite::fromJSON(preprocessed)
                         return(json)
                     })
    record <- as.data.frame(json, stringsAsFactors = FALSE)
    return(record)
## * end    
}
