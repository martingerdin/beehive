#' compile collated dataset function
#'
#' this function puts compiles the collated dataset from records
#' @param dir_names a vector of directory names to import records from. default is NULL, in which case records from all directories present in path are imported
#' @param skip_dirs a vector of directory names to skip importing from. default is NULL, in which case no directory is skipped
#' @export
## * content
## ** declare
compile.collated.dataset <- function(
                                     dir_names = NULL,
                                     skip_dirs = NULL
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
    dataset <- rbind.fill(lapply(dirs, compile.centre.dataset))
## ** save dataset to disk
    save.dataset(dataset, "./data/datasets/taft-dataset-collated/")
## ** return dataset    
    return(dataset)
## * end    
}
