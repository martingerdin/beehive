#' initiate
#'
#' this function loads all required packages and sources all scripts
#' @param data_path path to data directory, no default
#' @param data_copy_path path to data copy directory, no default
#' @param codebook_path path to codebook, no default
#' @param codebook_name name of codebook, no default 
#' @export
## * content
## ** declare
initiate <- function(
                     data_path,
                     data_copy_path,
                     codebook_path,
                     codebook_name
                     )
{
## ** create vector to hold session variables
    today <- Sys.time()
    .session_variables <<- list(today = today,
                                timestamp = get.timestamp(today),
                                data_path = data_path,
                                data_copy_path = data_copy_path,
                                codebook_path = codebook_path, 
                                codebook_name = codebook_name)
## * end
}



