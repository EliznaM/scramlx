
# functions that act on text files
# aim is to scramble analytes and replace Description in .txt files
# ------------------------------------------------------------------------------




#' Find which analytes are combined on plates
#'
#' @param data Data frame prepared by data_import().
#' @import dplyr
#' @import tidyr
#'
#' @return
#' @export
#'
#' @examples
analyte_comb_find <- function(data = dta) {
  tmp <- data %>%
    select(filename, Analyte) %>%
    distinct() %>%
    mutate(n = 1) %>%
    pivot_wider(id_cols = filename,
                names_from = Analyte,
                values_from = n) %>%
    select(-filename) %>%
    distinct()

  analyte_comb <- vector("list", nrow(tmp))

  for (i in 1:length(analyte_comb)) {
    x <- tmp[i, ]
    analyte_comb[[i]] <- names(tmp[i, ])[!is.na(x)]

  }

  analyte_comb <- purrr::map2(analyte_comb,
                       1:length(analyte_comb),
                       ~ tibble::tibble(set = .y,
                                analyte = .x)) %>%
    bind_rows()

}



# -----------------------------------------------------------------------------

# check for analytes that occur in more than one collection


#' Make analyte reference table for joining to the .txt files
#'
#' @param analyte_comb Data frame created by analyte_comb_find()
#' @param seed Numeric seed for random number generator
#' @import tidyr
#' @import dplyr
#' @return
#' @export
#'
#' @examples
analyte_ref_make <- function(analyte_comb = analyte_comb,
                             seed = 123) {
  dupl <- analyte_comb %>%
    group_by(analyte) %>%
    count() %>%
    filter(n != 1) %>%
    pull(analyte)

  # pair up analytes to swop around within plates
  # if 1 analyte occurs in more than one kit assure they are always replaced by the same other analyte

  # duplicated
  set.seed(seed)
  set_dupl <- tibble::tibble(analyte = dupl,
                     analyte_scrmbl = sample(dupl, replace = FALSE))

  # non-duplicated

  sets <- analyte_comb %>%
    filter(!analyte %in% dupl) %>%
    distinct(set) %>%
    pull(set)

  # create reference table for scrambling analytes

  set.seed(seed)

  analyte_ref <- purrr::map(
    sets,
    ~ analyte_comb %>%
      filter(!analyte %in% dupl) %>%
      filter(set %in% .x) %>%
      mutate(analyte_scrmbl = sample(analyte, replace = FALSE))
  ) %>%
    bind_rows() %>%
    bind_rows(set_dupl) %>%
    rename(Analyte = analyte)

}


# ------------------------------------------------------------------------------
# create id_ref for replacing Description

# id component
## arguments size, letters t/f, nrs t/f, seed



#' Create character vector to source from for id component of new Description
#'
#' @param ltrs Logical indicating whether lower-case letters should be included.
#' @param LTRS Logical indicating whether upper-case letters should be included.
#' @param nrs Logical indicating whether numbers should be included.
#' @param seed Numeric seed for random number generation.
#' @param sz Numeric indicating number of characters to sample from letters, LETTERS or numbers.
#' @param data Data frame created by data_import().
#'
#' @return
#' @export
#'
#' @examples
id_source_from <- function(ltrs = TRUE,
                              LTRS = FALSE,
                              nrs = TRUE,
                              seed = 123,
                              sz = 4,
                              data = dta) {

  n <- length(unique(data$Description))

  if (ltrs == TRUE) {
    a <- letters[]
  }

  if (LTRS == TRUE) {
    a <- LETTERS[]
  }

  ifelse(nrs == TRUE,
         assign("b", 0:9, envir = .GlobalEnv),
         assign("b", "", envir = .GlobalEnv))

  set.seed(seed)
  xlist <- vector("list", length = n)
  for(i in 1:length(xlist)){
    x <- sample(a, replace = TRUE, size = sz)
    x <- paste(x, collapse = "")
    x1 <- sample(b, replace = TRUE, size = sz)
    x1 <- paste(x1, collapse = "")
    xlist[[i]] <- paste(x, x1, sep = "")
  }
  return(unlist(xlist))
}


#' Create id component for new Description
#'
#' @param new_id_src Character vector created by id_source_from()
#' @param tp Character vector of time points.  Must be the same time points that occur in the raw data.  Currently supports only two time points.
#' @param stim
#' @param data
#'
#' @return
#' @export
#'
#' @examples
id_component_make <- function(new_id_src = new_id_src,
                              tp = NULL,
                              stim = NULL,
                              data = dta){

  tmp <- data$Description

  if(!is.null(tp)) {
    for (i in 1:length(tp)) {
      tmp[which(grepl(tp[i], tmp) == TRUE)] <- gsub(tp[i], "",
                                                    tmp[which(grepl(tp[i], tmp) == TRUE)])
    }
  }

  if(!is.null(stim)) {
    for (i in 1:length(stim)) {
      tmp[which(grepl(stim[i], tmp) == TRUE)] <- gsub(stim[i], "",
                                                      tmp[which(grepl(stim[i], tmp) == TRUE)])
    }
  }


  id_comp <- tibble::tibble(descr_id = unique(tmp),
                            new_id = new_id_src[1:length(unique(tmp))]) %>%
    full_join(tibble::tibble(descr_id = tmp,
                             Description = data$Description)) %>%
    distinct()

}


#' #' Create id component with no uniform pattern for new Description column
#' #'
#' #' @param data Data frame created by data_import().
#' #' @param sz Numeric vector to sample size of character strings from.
#' #' @param specials Character vector of special characters to sample from.
#' #' @param seed Numeric seed for random number generation.
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' id_component_wild <- function(
#'   data = dta,
#'   sz = 1:3,
#'   specials = c(".", "-", "\\", "/", "_", "&", "+"),
#'   seed = 123
#' ) {
#'   # specials <- c(".", "-", "\\", "/", "_", "&", "+")
#'   # sz <- 1:3
#'   n <- length(unique(data$Description))
#'   xlist <- vector("list", length = n)
#'   set.seed(seed)
#'   for (i in 1:length(xlist)) {
#'     a <- sample(specials, size = sample(sz, 1), replace = TRUE)
#'     b <- sample(letters[], size = sample(sz, 1), replace = TRUE)
#'     c <- sample(LETTERS[], size = sample(sz, 1), replace = TRUE)
#'     d <- sample(0:9, size = sample(sz, 1), replace = TRUE)
#'
#'     x <- sample(c("a", "b", "c", "d"), size = 3)
#'
#'     xlist[[i]] <-
#'       paste(get(x[1]),
#'             get(x[2]),
#'             get(x[3]),
#'             sep = "",
#'             collapse = "")
#'   }
#'
#'   x <- tibble::tibble(Description = unique(data$Description),
#'               id_comp = unlist(xlist))
#'
#' }




#' Make time point component for new Description column
#'
#' @param data Data frame created by data_import().
#' @param tp Character vector of time points.  Must be the same time points that occur in the raw data.
#'
#' @return
#' @export
#'
#' @examples
timepoint_component_make <- function(id_comp = id_comp,
                                     tp = c("D0", "D7")){

  # tp <- c("D0", "25W")
  n <- length(unique(id_comp$Description))

  x <- tibble::tibble(Description = unique(id_comp$Description),
              tp = NA)

  for(i in 1:length(tp)){
  x$tp[which(grepl(tp[i], x$Description) == TRUE)] <- tp[i]
  # x$tp[which(grepl(tp[2], x$Description) == TRUE)] <- tp[2]
  }
  return(x)
}



#' Make stimulation condition component for new Description column
#'
#' @param data Data frame created by data_import().
#' @param stim Character vector of stimulation conditions.  Must be the same conditions that occur in the raw data.
#'
#' @return
#' @export
#'
#' @examples
stim_cond_component_make <- function(id_comp = id_comp,
                                     stim = c("nil", "ag")){

  # stim <- c("nil", "ag")
  n <- length(unique(id_comp$Description))

  x <- tibble::tibble(Description = unique(id_comp$Description),
              stim = NA)

  for(i in 1:length(stim)){
  x$stim[which(grepl(stim[i], x$Description) == TRUE)] <- stim[i]
  # x$stim[which(grepl(stim[2], x$Description) == TRUE)] <- stim[2]
  }
  return(x)
}



#' Make reference table for new Description for joining to .txt files
#'
#' @param components List of data frames created by *_component_make functions.
#' @param separ Character string to separate the components when uniting them.
#'
#' @return
#' @export
#'
#' @examples
new_id_ref_make <- function(components = list(id_comp, stim_comp, timepoint_comp),
                            separ = "_"){

  # argument
  # components <- list(id_comp, timepoint_comp)

  id_ref <- purrr::map(components,
                ~.x) %>%
    purrr::reduce(inner_join) %>%
    select(-descr_id)

  x <- names(id_ref)[which(names(id_ref) != "Description")]

  id_ref <- id_ref %>%
    tidyr::unite(new_id, x, sep = separ) %>%
    distinct()

}


# ---------------------------------------------------------------------------

# scramble analytes and add new id in files


#' Create new .txt files after scrambling analytes and replacing Description with new Description.
#'
#' @param file_list List of original file paths to .txt files to be scrambled.
#' @param id_ref Data frame created by new_id_ref_make().
#' @param dir_new_files Path to directory in which to save new files.
#' @import tidyr
#' @import dplyr
#' @return
#' @export
#'
#' @examples
txt_scramble <- function(file_list = "rds/files.rds",
                         analyte_ref = analyte_ref,
                         id_ref = id_ref,
                         dir_new_files = "newfiles",
                         d = "\t") {

  files <- readr::read_rds(file_list)
  compare_nrow <- tibble(files = files,
                         plate_orig_nrow = NA,
                         plate_new_nrow = NA,
                         Analyte_rep = NA)

  for (i in 1:length(files)) {
    plate <-
      read.delim(files[i],
                 stringsAsFactors = FALSE,
                 strip.white = TRUE,
                 sep = d)

    compare_nrow$plate_orig_nrow[i] <- nrow(plate)
    i
    x <- names(plate)
    x1 <- grep("Analyte", plate$Analyte)

    tmp <- plate %>%
      full_join(analyte_ref) %>%
      select(analyte_scrmbl, x)


    tmp$analyte_scrmbl[x1] <- "Analyte"

    plate <- tmp %>%
      select(-Analyte) %>%
      rename(Analyte = analyte_scrmbl)



    x <- names(plate)

    tmp <- plate %>%
      left_join(id_ref)

    tmp$new_id[which(!stringr::str_detect(tmp$Type, "^X"))] <-
      tmp$Description[which(!stringr::str_detect(tmp$Type, "^X"))]

    plate <- tmp %>%
      select(-Description) %>%
      rename(Description = new_id) %>%
      select(x) %>%
      distinct()

    compare_nrow$plate_new_nrow[i] <- nrow(plate)
    compare_nrow$Analyte_rep[i] <- sum(grepl("Analyte", plate$Analyte))

    x <- unlist(stringr::str_split(files[i], "\\/"))
    x <- paste(dir_new_files, x[length(x)], sep = "/")

    readr::write_delim(plate, file = x, delim = d)

  }
  return(compare_nrow)
}

# --------------------------------------------------------------

# check Description conversion

#' Check the scrambled data against the original data
#'
#' @param dir_data Path to folder where original .txt files are
#' @param dir_data_new Path to where new files have been saved
#' @param d Delimiter in .txt files.  Try tab separated first.
#' @import tidyr
#' @import dplyr
#' @return
#' @export
#'
#' @examples
scramble_check <- function(dir_data = dir_data, dir_data_new = dir_data_new,
                           d = "\t"){

  dta <- data_import(dir_data, d = d)
  x <- readr::read_rds("rds/files.rds")

  dta_new <- data_import(dir_data = dir_data_new, d = d)
  x1 <- readr::read_rds("rds/files.rds")

  file_nrs <- tibble::tibble(data_folders = c("orig", "newfiles"),
                             file_nrs = c(length(x), length(x1)))

  dims <- tibble::tibble(dataset = c("orig", "new"),
                 nrow = c(nrow(dta), nrow(dta_new)),
                 Description_len = c(length(unique(dta$Description)),
                                     length(unique(dta_new$Description))))

  hed <- head(dta_new)


  tmp <- dta %>%
    select(Description, Type) %>%
    filter(!stringr::str_detect(Type, "X")) %>%
    distinct() %>%
    arrange(Description)


  tmp1 <- dta_new %>%
    filter(!stringr::str_detect(Type, "X")) %>%
    select(Description, Type) %>%
    distinct() %>%
    arrange(Description)


  x <- max(nrow(tmp), nrow(tmp1))
  descr <- tibble::tibble(dta_Descr = c(tmp$Description, rep(NA, (x - length(tmp$Description)))),
                          dta_Type = c(tmp$Type, rep(NA, (x - length(tmp$Type)))),
                          dta_new_Descr = c(tmp1$Description, rep(NA, (x - length(tmp1$Description)))),
                          dta_new_Type = c(tmp1$Type, rep(NA, (x - length(tmp1$Type)))))

  conversion_list <- list(file_nrs = file_nrs,
                          dims = dims,
                          newdta_head = hed,
                          description_check = descr)

}








