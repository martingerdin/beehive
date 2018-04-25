#' apply exclusion criteria function
#'
#' this function applies exclusion criteria
#' @param dataset the dataset on which to base the report, no default
#' @param exclusion_criteria a list of functions defining what observations to exclude from the report, no default
#' @export
## * content
## ** declare
apply.exclusion.criteria <- function(
                                     dataset,
                                     exclusion_criteria
                                     )
{
## ** apply exclusion criteria
    for (i in seq_along(exclusion_criteria)) {
        dataset <- exclusion_criteria[[i]](dataset)
    }
    return(dataset)
## * end    
}
