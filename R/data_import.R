


#' Import Luminex text files
#'
#' @param dir_data Path to the directory containing Luminex .txt files. dir_data can contain subfolders.
#' @importFrom magrittr %>%
#' @return
#' @export
#'
#' @examples
data_import <- function(dir_data) {

  files <- list.files(dir_data, recursive = TRUE,
                      full.names = TRUE)

  luminex_list <- list()

  files_invalid <- tibble::tibble(
    files = files,
    txt = NA,
    line_first = NA,
    header_repeat = NA
  )

  names_check <-
    c("Analyte",
      "Type",
      "Well",
      "Outlier",
      "Description",
      "FI",
      "FI...Bkgd")

  for (i in 1:length(files)) {
    #  for(i in 1:2){
    plate <-
      read.delim(files[i],
                 stringsAsFactors = FALSE,
                 strip.white = TRUE)

    line_first <- unlist(names(plate))  # what happens when it doesn't have names?
    files_invalid$line_first[i] <-
      sum(names_check %in% line_first) == length(names_check)

    head_repeat <- grep("Analyte", plate[, 1])
    files_invalid$header_repeat[i] <- is.numeric(head_repeat)
    files_invalid$txt[i] <- grepl(".txt", files[i])

    x <- sum(files_invalid$line_first[i],
             files_invalid$header_repeat[i],
             files_invalid$txt[i])

    if (x == 3) {
      plate <- plate[(head_repeat + 1):nrow(plate), ]

      plate$filename <-
        sub("\\..*$", "", sub("^.*[/\\]", "", files[i]))
      # remove path and extension from filename

      luminex_list[[i]] <- plate
    } else{
      luminex_list[[i]] <- "invalid"
    }
  }

  files <- dplyr::filter(files_invalid,
                         txt == TRUE &
                           line_first == TRUE &
                           header_repeat == TRUE) %>%
    dplyr::pull(files)
  readr::write_rds(files, "rds/files.rds")
  files_invalid <- dplyr::filter(files_invalid,
                                 txt == FALSE |
                                   line_first == FALSE |
                                   header_repeat == FALSE)
  readr::write_rds(files_invalid, "rds/files_invalid.rds")

  dta <- dplyr::bind_rows(luminex_list)
  #readr::write_rds(dta, "rds/dta.rds")
}



