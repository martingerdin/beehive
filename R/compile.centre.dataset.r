#' import records function
#'
#' this function imports records from a specific directory
#' @param dir_path path to directory to import records from, no default
#' @param save logical, if TRUE the datasets are saved to disk, no default
#' @export
## * content
## ** declare
compile.centre.dataset <- function(
                                   dir_path,
                                   save
                                   )
{
## ** create vector with record files to import
    files <- list.files(dir_path, full.names = TRUE, recursive = FALSE, pattern = ".txt$")
## ** import records
    if (length(files) > 0) {
        dataset <- plyr::rbind.fill(lapply(files, import.record))
    } else {
        dataset <- NULL
    }
## ** save dataset to disk
    if (save & !is.null(dataset)) {
        dataset <- dataset[, -grep(paste0(c("version", "X_id"),
                                          collapse = "|"),
                                   colnames(dataset))]
        ds_path <- paste0(.session_variables$data_path, "datasets/")
        dir_name <- gsub("records", "dataset", basename(dir_path))
        new_dir_path <- paste0(ds_path, dir_name, "/")
        save.dataset(dataset, new_dir_path)
    }
## ** return dataset    
    return(dataset)
## * end
}    
