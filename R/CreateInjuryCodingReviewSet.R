#' Create injury coding review set
#'
#' This function takes the dataset as input and creates a set of injuries and
#' codes for review.
#' @param dataset A data frame. Has the collated data from all centres. No
#'     default.
#' @param sample.size A positive integer. Defines the size of the random sample
#'     to draw from the complete set. If NULL the complete set is kept. Defaults
#'     to 100.
#' @param number.of.reviewers NOT YET IMPLEMENTED. A positive integer. Defines
#'     the number of reviewers. The set is randomly split in the same number of
#'     parts as the number of reviewers. Defaults to 1.
#' @param log A logical vector of length 1. If TRUE a logfile is used to remove
#'     observations that have already been reviewed and appended with the IDs of
#'     observations that are included in the current set. Defaults to TRUE.
#' @param save A logical vector of length 1. If TRUE the review set is saved to
#'     disk. Defaults to TRUE.
#' @param return.set A logical vector of length 1. If TRUE the review set is
#'     returned to the parent environment. Defaults to FALSE.
#' @export
CreateInjuryCodingReviewSet <- function(dataset, sample.size = 100,
                                        number.of.reviewers = 1, log = TRUE,
                                        save = TRUE, return.set = FALSE) {
    ## Error handling
    if (!is.data.frame(dataset))
        stop("dataset has to be a data frame")
    if ((!is.null(sample.size) & sample.size < 0) | length(sample.size) > 1)
        stop("sample.size has to be a positive integer")
    if ((!is.null(number.of.reviewers) & number.of.reviewers < 0) | length(number.of.reviewers) > 1)
        stop("number.of.reviewers has to be a positive integer")
    if (!is.logical(log) | length(log) > 1)
        stop("log has to be a logical vector of length 1")
    if (!is.logical(save) | length(save) > 1)
        stop("save has to be a logical vector of length 1")
    if (!is.logical(return.set) | length(return.set) > 1)
        stop("return.set has to be a logical vector of length 1")
    ## Import log and remove already reviewed observations
    if (log) {
        logfile <- data.frame(id = NULL)
        if (file.exists("injury_coding_review_logged_ids.rds")) 
            logfile <- readRDS("injury_coding_review_logged_ids.rds")
        combined.ids <- paste0(dataset$centre, dataset$pid)
        dataset <- dataset[!(combined.ids %in% logfile$id), ]
    }
    ## Create objects to use to stack the set
    dataset.colnames <- colnames(dataset)
    injury.pattern <- "^[a-z]*inj[1-9]*"
    injury.colnames <- grep(paste0(injury.pattern, "$"), dataset.colnames, value = TRUE)
    icd.colnames <- grep(paste0(injury.pattern, "icd$"), dataset.colnames, value = TRUE)
    ## Stack the set
    stack.parts <- lapply(seq_along(injury.colnames), function(i) {
        injury.colname <- injury.colnames[i]
        icd.colname <- icd.colnames[i]
        stack.part <- dataset[, c("centre", "pid", injury.colname, icd.colname)]
        colnames(stack.part)[3:4] <- c("injury_description", "icd10_code")
        return(stack.part)
    })
    set <- do.call(rbind, stack.parts)
    ## Remove observations without reported injury
    set <- set[set$injury_description != "999", ]
    ## Add new IDs to log and save
    if (log) {
        new.ids <- data.frame(id = unique(paste0(set$centre, set$pid)))
        logfile <- rbind(logfile, new.ids)
        saveRDS(logfile, "injury_coding_review_logged_ids.rds")
    }
    ## Add review variables
    if (nrow(set) > 0) 
        set$correct <- set$correct_code <- ""
    ## Draw a random sample from the complete set
    if (!is.null(sample.size) & nrow(set) > sample.size)
        set <- set[sample(1:nrow(set), sample.size), ]
    ## Split into the same number of sets as the number of reviewers
    ## Add code here
    ## Save set to disk and return
    if (save) write.csv(x = set,
                        file = paste0("injury_coding_review_set_", .session_variables$timestamp, ".csv"),
                        row.names = FALSE)
    if (return.set) return(set)
}
