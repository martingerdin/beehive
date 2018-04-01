#' make data review report
#'
#' this function compiles a data review report
#' @param path path to directory with records, no default
#' @param report_name name of the report file, no default
#' @param data_path path to data directory, no default
#' @param data_copy_path path to data copy directory, no default
#' @param codebook_path path to codebook, no default
#' @param codebook_name name of codebook, no default
#' @param test logical, if TRUE the generated report is named test, defaults to FALSE
#' @export
## * content
## ** declare
make.data.review.report <- function(
                                    path,
                                    report_name,
                                    data_path,
                                    data_copy_path,
                                    codebook_path,
                                    codebook_name,
                                    test = FALSE
                                    )
{
## ** source initate code
    source("./initiate.r")
## ** run initiate function
### loads required packages, sources all function from scripts and sets
### variables to be used by multiple functions throughout the session
    initiate(data_path,
             data_copy_path,
             codebook_path,
             codebook_name)
## ** copy data
### copies all records from path to a local directory so that subsequent
### functions do not have to access path
    copy.data(path)
## ** compile dataset     
### creates centre specific and collated datasets and saves those to disk
    dataset <- compile.collated.dataset()
## ** create report    
    create.report(dataset, report_name, test)
## * end    
}
