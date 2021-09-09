
# functions that act on text files
# aim is to scramble analytes in .txt files


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

  analyte_comb <- map2(analyte_comb,
                       1:length(analyte_comb),
                       ~ tibble(set = .y,
                                analyte = .x)) %>%
    bind_rows()

}



# ---------------------------

# check for analytes that occur in more than one collection

analyte_duplicates <- function(analyte_comb = analyte_comb,
                               seed = 123)

  dupl <- analyte_comb %>%
    group_by(analyte) %>%
    count() %>%
    filter(n != 1) %>%
    pull(analyte)

# pair up analytes to swop around within plates
# if 1 analyte occurs in more than one kit assure they are always replaced by the same other analyte

# duplicated
set.seed(seed)
set_dupl <- tibble(analyte = dupl,
                   analyte_scrmbl = sample(dupl, replace = FALSE))

# non-duplicated

sets <- analyte_comb %>%
  filter(!analyte %in% dupl) %>%
  distinct(set) %>%
  pull(set)

# create reference table for scrambling analytes

set.seed(seed)

replacements <- map(
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


# ------------------------------------
# create id_ref for replacing Description

# id component
## arguments size, letters t/f, nrs t/f, seed

id_component_make <- function(ltrs = TRUE,
                              LTRS = FALSE,
                              nrs = TRUE,
                              seed = 123,
                              sz = 4,
                              data = dta) {
  # ltrs <- TRUE
  # # LTRS <- TRUE
  # nrs <- TRUE
  # sz <- 4
  # seed <- 123
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
  x <- tibble(Description = unique(data$Description),
              id_comp = unlist(xlist))

}



timepoint_component_make <- function(data = dta,
                                     tp = c("D0", "D7")){

  # tp <- c("D0", "D7")
  n <- length(unique(data$Description))

  x <- tibble(Description = unique(data$Description),
              tp = NA)

  x$tp[which(grepl(tp[1], data$Description) == TRUE)] <- tp[1]
  x$tp[which(grepl(tp[2], data$Description) == TRUE)] <- tp[2]
  x
}




stim_cond_component_make <- function(data = dta,
                                     tp = c("nil", "ag")){

  # stim <- c("nil", "ag")
  n <- length(unique(data$Description))

  x <- tibble(Description = unique(data$Description),
              stim = NA)

  x$stim[which(grepl(stim[1], data$Description) == TRUE)] <- stim[1]
  x$stim[which(grepl(stim[2], data$Description) == TRUE)] <- stim[2]
  x
}



new_id_ref_make <- function(components = list(id_comp, stim_comp, timepoint_comp),
                            separ = "_"){

  # argument

  id_ref <- map(components,
                ~.x) %>%
    reduce(inner_join)

  x <- names(id_ref)[-1]

  id_ref <- id_ref %>%
    unite(new_id, x, sep = separ)

}

# ------------------------------------------------

# scramble analytes in files


txt_scramble <- function(file_list = "rds/files.rds",
                         id_ref = id_ref) {
  files <- read_rds(file_list)

  for (i in 1:length(files)) {
    plate <-
      read.delim(files[i],
                 stringsAsFactors = FALSE,
                 strip.white = TRUE)
    x <- names(plate)
    x1 <- grep("Analyte", plate$Analyte)

    tmp <- plate %>%
      full_join(replacements) %>%
      select(analyte_scrmbl, x)

    #grep("Analyte", tmp$Analyte) == x1

    tmp$analyte_scrmbl[tmp$Analyte == "Analyte"] <- "Analyte"

    plate <- tmp %>%
      select(-Analyte) %>%
      rename(Analyte = analyte_scrmbl)

    x <- names(plate)

    tmp <- plate %>%
      full_join(id_ref)

    tmp$new_id[which(!str_detect(tmp$Type, "^X"))] <-
      tmp$Description[which(!str_detect(tmp$Type, "^X"))]

    plate <- tmp %>%
      select(-Description) %>%
      rename(Description = new_id) %>%
      select(x)

    x <- unlist(str_split(files[i], "\\/"))
    x <- paste("newfiles", x[length(x)], sep = "/")

    write_delim(plate, file = x, delim = "\t")

  }
}



