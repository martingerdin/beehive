#' compile collated dataset function
#'
#' this function puts compiles the collated dataset from records
#' @param dataset_name_prefix a character string to prefix the datasets with, no default 
#' @param dir_names a vector of directory names to import records from. default is NULL, in which case records from all directories present in path are imported
#' @param skip_dirs a vector of directory names to skip importing from. default is NULL, in which case no directory is skipped
#' @param save logical, if TRUE the datasets are saved to disk, defaults to TRUE
#' @export
## * content
## ** declare
compile.collated.dataset <- function(
                                     dataset_name_prefix,
                                     dir_names = NULL,
                                     skip_dirs = NULL,
                                     save = TRUE
                                     )
{
## ** create vector of directories to find records in
    path <- .session_variables$data_copy_path
    if (is.null(dir_names)) {
        dirs <- list.dirs(path,
                          recursive = TRUE,
                          full.names = TRUE)
    } else {
        dirs <- unlist(lapply(dir_names, function(n) paste0(path, n)))
    }
    if (!is.null(skip_dirs)) dirs <- dirs[-grep(paste0(skip_dirs, collapse = "|"), dirs)]
## ** compile dataset
    dataset <- rbind.fill(lapply(dirs, compile.centre.dataset, save = save))
## ** save dataset to disk
    dataset_path <- paste0(.session_variables$data_path, "/datasets/", dataset_name_prefix, "-dataset-collated/")
    if (save) save.dataset(dataset, dataset_path)
## ** return dataset    
    return(dataset)
## * end    
}
