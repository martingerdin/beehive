#' summarise variable function
#'
#' this is the master summarising function, taking as input the name of a variable, a dataset and a codebook
#' @param var_name the name of the variable, no default
#' @param dataset the dataset, no default
#' @param codebook the codebook, no default
#' @param strata name of stratifying variable, defaults to NULL
#' @export
## * content
## ** declare
summarise.variable <- function(
                               var_name,
                               dataset,
                               codebook,
                               strata = NULL
                               )
{
## ** get details and create header
    cb <- codebook[codebook$name == var_name, ]
    if (length(cb$label) > 0) {
        if (cb$type == "qualitative" | cb$type == "quantitative") {
            header <- paste0("\\section{", cb$label, " (", var_name, ")} \n",
                             cb$description, "\\par \n")
            data <- dataset[, var_name]
            if (!is.null(strata)) {
                strata_list <- list(name = strata, data = dataset[[strata]])
            } else strata_list <- list(name = NULL)
            summary <- match.fun(paste0("summarise.", cb$type))(
                cb,
                data,
                strata = strata_list)
            end <- paste0("\\noindent\\hyperlink{toc}{\\textit{Click here to go back to the table of contents}}",
                          "\\pagebreak \n")
            entry <- paste0(header, summary, end)
        }
    }
    if (!exists("entry")) entry <- ""
    return(entry)
## * end
}
