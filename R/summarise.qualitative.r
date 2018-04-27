#' summarise qualitative function
#'
#' this function generates a summary of a qualitative variable
#' @param variable_codebook variable specific codebook, no default
#' @param data variable specific data, no default
#' @param verbose logical, if TRUE progress is printed to console, defaults to FALSE
#' @export
## * content
## ** declare
summarise.qualitative <- function(
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
    values <- vc$valid_values
    labels <- vc$value_labels
    if (values != "" & !is.na(labels)){
        values <- get.vector(values)
        labels <- paste(values, get.vector(vc$value_labels), sep = " = ")
        for (i in seq_along(labels))
            if (nchar(labels[i]) > 15) labels[i] <- substring(labels[i], 1, 15)
        for (i in seq_along(values)) data[data == values[i]] <- labels[i]        
    }
    data <- as.factor(data)
    frame$overview_data[[vc$name]] <- data
## ** create table
    t <- FALSE
    maxsum <- 10
    include_n <- include_p <- TRUE
    summary_all <- get.summary(data = data,
                               t = t,
                               maxsum = maxsum,
                               include_n = include_n,
                               include_p = include_p, 
                               name = "All")
    data <- as.character(data)
    data[!(data %in% rownames(summary_all$m)) & !is.na(data)] <- "(Other)"
    data <- as.factor(data)
    if (!is.null(strata$name) & strata$name != vc$name) {
        ls <- levels(as.factor(strata$data))
        summary_strata <- lapply(setNames(nm = ls),
                                 get.summary,
                                 data = data,
                                 strata_data = strata$data,
                                 t = t,
                                 maxsum = maxsum,
                                 include_n = include_n,
                                 include_p = include_p)
    } else summary_strata <- NULL
    names(summary_strata) <- NULL
    summaries <- lapply(setNames(nm = c("m", "p")), function(n) {
        do.call(cbind, c(list(summary_all[[n]]),
                         lapply(summary_strata, function(s) s[[n]])))
    })
    align <- rep("X", ncol(summaries$m) + 1)
    alifn <- "X"
    x <- "summaries$p"
    if (is.null(summaries$p)) x <- "summaries$m"
    table <- print.xtable(xtable(eval(parse(text = x)),
                                 digits = 0,
                                 align = align),
                          floating = FALSE,
                          print.results = FALSE,
                          tabular.environment = "tabularx",
                          width = "\\textwidth")
    table <- paste0("\\begin{centering} \n",
                    table, " \n",
                    "\\end{centering} \n")
## ** create graphics
    if (sum(!is.na(data)) > 0) {
        x <- "Freq. ~ data"
        ## prepare data in table format for barchart
        summary <- as.data.frame(summaries$m)
        bc_data <- lapply(setNames(nm = colnames(summary)), function(n) {
            c <- as.data.frame(summary[, n])
            colnames(c) <- "Freq."
            c$data <- rownames(summary)
            if (!is.null(strata$data) & strata$name != vc$name) c$strata <- n
            return (c)
        })
        if (!is.null(strata$data) & strata$name != vc$name) {
            x <- paste0(x, " | strata")
            bc_data[[grep("All", names(bc_data))]] <- NULL
        }
        bc_data <- rbind.fill(bc_data)
        nls <- nrow(summary)
        cex <- ifelse(nls <= 5, 1,
               ifelse(nls > 5 & nls <= 8, 0.6,
               ifelse(nls > 8, 0.4, NA))) 
        bc <- barchart(eval(parse(text = x)),
                       ylim = c(0, NA),
                       data = bc_data,
                       xlab = vc$name,
                       horizontal = FALSE,
                       type = "frequency",
                       scales = list(x = list(rot = 45, cex = cex)))
        frame$plots[[vc$name]] <- bc
        graphics <- paste0("\\begin{centering} \n",
                           "%% begin.rcode ", vc$name, ", echo=FALSE, results='asis', fig.width=4, fig.height=4 \n",
                           "% print(plots$", vc$name, ") \n", 
                           "%% end.rcode \n",
                           "\\end{centering} \n")
    } else graphics <- ""
## ** return entry    
    entry <- paste0(table, graphics)
    return (entry)
## * end    
}
