

#' @export

percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

price <- function(x) {
  format(x, big.mark = " ", scientific = FALSE, nsmall = 0)
}

is_roman <- function(input) {
  grepl("I|X|V", input, fixed = FALSE)
}


converty_to_numeric_if_roman <- function(number) {
  if (is.roman(number)) {
    number <- as.roman(number)
  }
  as.numeric(number)
}

converty_to_numeric_if_roman <- Vectorize(converty_to_numeric_if_roman)


formatPrice <- function(number) {
  return(format(number, big.mark = " ", scientific = FALSE, nsmall = 0))
}



fill_with_mapping <- function(dataframe, value_mapping, column_mapping) {
  assert_that(assertthat::not_empty(dataframe), msg = "dataframe should have rows")
  assert_that(assertthat::not_empty(value_mapping))
  assert_that(assertthat::has_name(value_mapping, c("oldvalue", "newvalue", "oldColname")), msg = "value_mapping needs to have oldvalue, newvalue and oldColname columns")

  assert_that(assertthat::not_empty(column_mapping))
  assert_that(assertthat::not_empty(names(column_mapping)), msg = "column_mapping should have names")
  assert_that(assertthat::noNA(column_mapping))

  maching_colnames <- colnames(dataframe)[colnames(dataframe) %in% names(column_mapping)]
  assert_that(assertthat::not_empty(maching_colnames))
  assert_that(assertthat::noNA(maching_colnames))

  for (colname in maching_colnames) {
    mapped <- unlist(sapply(dataframe[, colname], function(val) {
      value_mapping[value_mapping$oldvalue == val & value_mapping$oldColname == colname, ]$newvalue
    }))
    dataframe[, colname] <- mapped
  }
  names(dataframe)[colnames(dataframe) %in% maching_colnames] <- column_mapping[maching_colnames]
  return(dataframe)
}



#' @export
aoperat_options <- function() {
  options(stringsAsFactors = FALSE)
  options(max.print = 100)
  options(scipen = 999)
  options(Encoding = "UTF-8")
  options(show.error.locations = TRUE)
  options(show.error.messages = TRUE)
  options(verbose = TRUE)
  options(warnPartialMatchArgs = TRUE)
  options(warnPartialMatchAttr = TRUE)
  options(warnPartialMatchDollar = TRUE)

}


#' @export
copy_template_to_generator <- function(template_dir, output_dir) {
  all_files <- list.files(template_dir, recursive = T, no.. = T, all.files = T, full.names = T)

  # sub-catalogs
  if (length(all_files) > 0) {
    copy_to <- file.path(output_dir, substring(dirname(all_files), nchar(template_dir) + 1), basename(all_files))
    copy_to_dirs <- unique(dirname(copy_to))
    for (copt_to_dir in copy_to_dirs) {
      if (!dir.exists(copt_to_dir)) {
        dir.create(copt_to_dir, recursive = T)
      }
    }

    file.copy(all_files, copy_to, overwrite = T, recursive = T)
  }
}

#' @export
prepare_generation <- function(id) {
  output_dir <- file.path(dir_generation_path, id)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = T)
  }
  output_file_start <- file.path(output_dir, "start")
  if (!file.exists(output_file_start)) {
    file.create(output_file_start)
  }
  return(output_dir)
}
