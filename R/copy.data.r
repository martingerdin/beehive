#' copy data function
#'
#' this function copies data from origin (e.g. a server) and store here for easy and fast access
#' @param path path to origin, no default
#' @param test for package development, defaults to FALSE
#' @export
## * content
## ** declare
copy.data <- function(
                      path,
                      test = FALSE
                      )
{
## ** check if data dir exists else create it
    if(!dir.exists(.session_variables$data_path))
        dir.create(.session_variables$data_copy_path, recursive = TRUE)
## ** sync files to data dir
    exclude_path <- "exclude_list.txt"
    if (test) exclude_path <- "../exclude_list.txt"
    cmd <- paste("rsync -rvcp --no-whole-file --exclude-from", exclude_path, path, .session_variables$data_copy_path)
    tryCatch(expr = system(cmd),
             warning = function(w) {print(w)},
             error = function(e) {print(e)},
             finally = {message("data copied")})
## * end    
}
