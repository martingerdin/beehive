#' copy data function
#'
#' this function copies data from origin and store here for easy and fast access
#' @param path path to origin, no default
#' @export
## * content
## ** declare
copy.data <- function(
                      path
                      )
{
## ** check if data dir exists else create it
    if(!dir.exists(.session_variables$data_path))
        dir.create(.session_variables$data_copy_path, recursive = TRUE)
## ** sync files to data dir
    cmd <- paste("rsync -rvu", path, "./data/copy")
    tryCatch(expr = system(cmd),
             warning = function(w) {print(w)},
             error = function(e) {print(e)},
             finally = {message("data copied")})
## * end    
}
