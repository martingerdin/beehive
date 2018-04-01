#' save dataset function
#'
#' this function saves a dataset (dataframe) to disk using write.csv
#' @param x the dataset to be save, no default
#' @param dir_path path where the dataset should be saved, no default
#' @export
## * content
## ** declare
save.dataset <- function(
                         x,
                         dir_path
                         )
{
## ** create directory if it does not exist
    if (!dir.exists(dir_path)) dir.create(dir_path,
                                          recursive = TRUE)
## ** write csv to that directory
    file <- paste0(dir_path, basename(dir_path), "-", .session_variables$timestamp, ".csv")
    write.csv(x, file = file, row.names = FALSE)
    message(file, " saved")
## * end    
}    
    
