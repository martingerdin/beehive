#' make weekly data review report test suite main function
#'
#' this function compiles the weekly data review report when testing beehive
#' @param path path to directory with records
#' @param report_name name of the report file
#' @param report_title title of the data review report
#' @param data_path path to data directory
#' @param data_copy_path path to data copy directory
#' @param dataset_name_prefix a character string to prefix the datasets with
#' @param codebook_path path to codebook
#' @param codebook_name name of codebook
#' @param develop logical, if TRUE the develop version of beehive is used, defaults to FALSE
#' @param test logical, if TRUE the generated report is named test, defaults to FALSE
#' @export
## * content
## ** declare
test.make.weekly.report <- function(
                                    path = "../test_data/server/",
                                    report_name = "test-weekly-data-review-report",
                                    report_title = "Test data review report",
                                    data_path = "../test_data/data/",
                                    data_copy_path = "../test_data/data/copy/",
                                    dataset_name_prefix = "test",
                                    codebook_path = "../test_data/codebook/",
                                    codebook_name = "test-codebook.csv",
                                    develop = FALSE,
                                    test = TRUE
                                    )
{
## ** define exclusion criteria
    exclude_test_centre <- function(x) x[x$centre != 9999, ]
    exclude_test_observations <- function(x) x[x$testvar != 1, ]
    exclusion_criteria <- list(exclude_test_centre,
                               exclude_test_observations)    
## ** create report    
    make.data.review.report(path = path,
                            report_name = report_name,
                            report_title = report_title,
                            data_path = data_path,
                            data_copy_path = data_copy_path,
                            dataset_name_prefix = dataset_name_prefix, 
                            codebook_path = codebook_path,
                            codebook_name = codebook_name,
                            exclusion_criteria = exclusion_criteria,
                            test = test)
## * end    
}
## test.make.weekly.report()
