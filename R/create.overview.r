#' create overview
#'
#' creates an overview, including a tableplot
#' @param variables character vector holding the names of the variables to summarise, no default
#' @param data the dataset, no default
#' @export
## * content
## ** declare
create.overview <- function(
                            variables,
                            data
                            )
{
## ** create entry header    
    header <- paste0("\\section{Overview} \n")
## ** create tableplot
    tableplot(data[variables[41:46]])
}
