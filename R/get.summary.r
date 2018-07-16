#' get summary function
#'
#' this is a wrapper for the summary function, returns a data frame
#' @param level the strata level to use when subsetting data based on strata_data, defaults to NULL
#' @param strata_data the grouping data, defaults to NULL
#' @param t logical, if TRUE the matrix is transposed before made into a data frame. defaults to TRUE
#' @param maxsum determines the maximum number of levels shown for qualitative variables, defaults to NULL
#' @param force_na logical, if TRUE the number of NAs is reported even if there are none, defaults to TRUE
#' @param include_n logical, if TRUE the number of observations is included, defaults to TRUE
#' @param include_p logical, if TRUE the percentage is also included, defaults to FALSE
#' @param p_digits integer, if include_p = TRUE this will be the number of digits in the percentage, defaults to 0
#' @param name (col)name of data, defaults to level
#' @param data the data to be summarised, no default
#' @param verbose logical, if TRUE more is printed to console, defaults to FALSE
#' @export
## * content
## ** declare
get.summary <- function(
                        level = NULL,
                        strata_data = NULL,
                        t = TRUE,
                        maxsum = NULL,
                        force_na = TRUE,
                        include_n = TRUE,
                        include_p = FALSE,
                        p_digits = 0,
                        name = level,
                        data,
                        verbose = FALSE
                        )
{
## ** stratify if necessary
    if (!is.null(level)) data <- data[strata_data == level]
    if (verbose) for (l in list(level, data, strata_data)) print(l)
## ** get summary as data frame
    m <- as.matrix(summary(data, maxsum = maxsum))
    colnames(m) <- name
    if (force_na & !any(is.na(data))) m <- rbind(m, matrix(0, dimnames = list("NA's")))
    if (include_n & !is.factor(data)) m <- rbind(m, matrix(length(data), dimnames = list("N.")))
    if (include_n & is.factor(data)) colnames(m) <- paste0(colnames(m), ", n=", length(data))
    if (t) m <- t(m)
    if (verbose) print(m)
    df <- as.data.frame(m)
    df$i <- rownames(df)
    df <- df[order(df$i, decreasing = TRUE), ]
    na_pos <- grep("NA's", df$i)
    if (length(na_pos) > 0) {
        na <- df[na_pos, ]
        df <- rbind(df[-na_pos, ], na)
    }
    df$i <- NULL
    if (verbose) print(df)
    sum_list <- list(m = df)
    if (include_p & is.factor(data)) {
        p <- df
        p[, 1] <- paste0(m[, 1], " (", round(m/length(data) * 100, digits = p_digits), ")")
        colnames(p) <- paste0(colnames(p), " (%)")
        sum_list$p <- p
    }
    if (verbose) print(sum_list)    
## ** return data frame
    return (sum_list)
## * end
} 
