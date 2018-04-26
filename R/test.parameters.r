#' test parameters function
#'
#' not really a function, but includes individual parameters to set for testing
test.parameters <- function()
{
    depends <- c("readtext", "jsonlite", "knitr", "lattice", "latticeExtra", "plyr", "tabplot", "xtable")
    for(p in depends) require(p, character.only = TRUE)
    fs <- list.files()
    fs <- fs[!(fs %in% c(grep("test-report|test.parameters", fs, value = TRUE), "test.make.data.review.report.r"))]
    for (f in fs) source(f)
    path <- "../test_data/server/"
    report_name <- "test-weekly-data-review-report"
    report_title <- "Test data review report"
    data_path <- "../test_data/data/"
    data_copy_path <- "../test_data/data/copy/"
    dataset_name_prefix <- "test"
    codebook_path <- "../test_data/codebook/"
    codebook_name <- "test-codebook.csv"
    develop <- FALSE
    test <- TRUE
    dataset <- compile.collated.dataset(dataset_name_prefix, save = FALSE)
    codebook <- read.csv(paste0(codebook_path, codebook_name))
    codebook[] <- lapply(codebook, as.character)
    codebook[codebook == ""] <- NA
}
