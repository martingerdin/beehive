#' create report function
#'
#' this function creates the weekly data report
#' @param report_title title of the data review report, no default
#' @param dataset the dataset on which to base the report
#' @param report_name the name of the report file, no default
#' @param test logical, if TRUE the generated report is named test, no default
#' @param exclusion_criteria a list of functions defining what observations to exclude from the report, no default
#' @param codebook_path the path to the data codebook that governs how to interpret the data, defaults to .session_variables$codebook_path
#' @param codebook_name the name of the codebook, defaults to .session_variables$codebook_name
#' @export
## * content
## ** declare
create.report <- function(
                          report_title,
                          dataset,
                          report_name,
                          test,
                          exclusion_criteria,
                          codebook_path = .session_variables$codebook_path,
                          codebook_name = .session_variables$codebook_name
                          )
{
## ** apply exclusion criteria
    dataset <- apply.exclusion.criteria(dataset, exclusion criteria)
## ** get codebook    
    codebook <- read.csv(paste0(codebook_path, codebook_name))
    codebook[] <- lapply(codebook, as.character)
    codebook[codebook == ""] <- NA
## ** create document header
    header <- create.header(report_title)
## ** generate content
    plots <- list()
    overview_data <- list()
    variables <- colnames(dataset)
    content <- paste0(unlist(lapply(variables, summarise.variable, dataset,
                                    codebook, "centre")),
                      collapse = " \n")
## ** create overview
    # overview <- create.overview(variables)    
## ** end document and knit     
    end <- "\\end{document}"
    file_name <- paste0(report_name, "-", .session_variables$timestamp, ".rtex")
    if (test) file_name <- "test.rtex"
    write(paste0(header, content, end),
          file_name)
    knit2pdf(file_name)
    if (test) system("open test.pdf")
## * end                   
}



