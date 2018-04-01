#' create report function
#'
#' this function creates the weekly data report
#' @param dataset the dataset on which to base the report
#' @param report_name the name of the report file, no default
#' @param test logical, if TRUE the generated report is named test, no default
#' @param codebook_path the path to the data codebook that governs how to interpret the data, defaults to .session_variables$codebook_path
#' @param codebook_name the name of the codebook, defaults to .session_variables$codebook_name
#' @param title report title, defaults to "TAFT data review report"
#' @export
## * content
## ** declare
create.report <- function(
                          dataset,
                          report_name,
                          test,
                          codebook_path = .session_variables$codebook_path,
                          codebook_name = .session_variables$codebook_name,
                          title = "TAFT data review report"
                          )
{
## ** use only live and non-test records
    doar <- dataset$doar
    doar[doar == 999] <- NA
    doar <- as.POSIXct(doar, format = "%Y-%m-%d")
    dataset <- dataset[doar >= "2017-10-16" & !is.na(doar), ]
    dataset <- dataset[dataset$centre != 9999, ]
    dataset <- dataset[dataset$testvar != 1, ]
## ** get codebook    
    codebook <- read.csv(paste0(codebook_path, codebook_name))
    codebook[] <- lapply(codebook, as.character)
    codebook[codebook == ""] <- NA
## ** create document header
    header <- create.header(title)
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



