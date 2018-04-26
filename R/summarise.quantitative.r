#' summarise quantitative function
#'
#' this function generates a summary of a quantitative variable
#' @param variable_codebook variable specific codebook, no default
#' @param data variable specific data, no default
#' @param strata list with two entries, name and data. the name entry is the name of the variable that will be used to stratify output and data is the data of that stratifying variable. both default to NULL
#' @param verbose logical, if TRUE progress is printed to console, defaults to FALSE
#' @export
## * content
## ** declare
summarise.quantitative <- function(
                                   variable_codebook,
                                   data,
                                   strata = list(name = NULL,
                                                 data = NULL),
                                   verbose = FALSE
                                   )
{
## ** prepare data
    if (verbose) print(vc$name)
    frame <- parent.frame(n = 3)    
    vc <- variable_codebook
    data[data == vc$unknown] <- NA
    data <- as.numeric(data)
    frame$overview_data[[vc$name]] <- data
## ** create table
    summary_all <- get.summary(data = data, name = "All")$m
    if (!is.null(strata$name) & strata$name != vc$name) {
        ls <- levels(as.factor(strata$data))
        summary_strata <- lapply(setNames(nm = ls),
                                 get.summary,
                                 data = data,
                                 strata_data = strata$data,
                                 verbose = FALSE)
    } else summary_strata <- NULL
    summary_strata <- lapply(summary_strata, function(x) x$m)
    table_list <- c(list(summary_all), summary_strata)
    rownames <- unlist(lapply(table_list, rownames))
    summary <- rbind.fill(table_list)
    rownames(summary) <- rownames
    table <- print.xtable(xtable(summary,
                                 digits = 1),
                          floating = FALSE,
                          print.results = FALSE,
                          )
    table <- paste0("\\begin{center} \n",
                    table, " \n",
                    "\\end{center} \n")
    if (verbose) print(table)
## ** create graphics
    if (sum(!is.na(data)) > 0) {
        x <- "~ y"
        hist_data <- data.frame(y = data)
        if (!is.null(strata$data) & strata$name != vc$name) {
            z <- as.factor(strata$data)
            hist_data <- data.frame(y = data, z = z)
            x <- paste0(x, " | z")
        }
        hist_data <- na.omit(hist_data)
        frame$plots[[vc$name]] <- histogram(eval(parse(text = x)),
                                            data = hist_data, 
                                            xlab = vc$name,
                                            breaks = NULL,
                                            nint = length(levels(as.factor(data))))
        graphics <- paste0("%% begin.rcode ", vc$name, ", echo=FALSE, results='asis', fig.width=4, fig.height=4 \n",
                           "% print(plots$", vc$name, ") \n", 
                           "%% end.rcode \n")
    } else graphics <- ""
## ** create entry and return
    entry <- paste0(table, graphics)
    return (entry)
## * end    
}
