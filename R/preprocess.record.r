#' preprocess record function
#'
#' this function preprocesses a text record to make it possible to process
#' @param text_record the record as text representation, no default
#' @export
## * content
## ** declare
preprocess.record <- function(text_record)                   
{                                                            
## ** replace double quotes in double quotes with nothing
    preprocessed <- sub("\"\"", "", text_record)             
    return(preprocessed)
## * end    
}                                                            
