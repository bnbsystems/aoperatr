

#' @export
percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

#' @export
price <- function(x){
  format(x,big.mark=" ", scientific=FALSE, nsmall = 0)
}

#' @export
aoperat_options <- function(){
  options(stringsAsFactors=FALSE)
  options(max.print=100)
  options(scipen=10)
  options(Encoding="UTF-8")
  options(show.error.locations=TRUE)
  options(show.error.messages=TRUE)
  options(verbose=TRUE)
  options(warnPartialMatchArgs=TRUE)
  options(warnPartialMatchAttr=TRUE)
  options(warnPartialMatchDollar=TRUE)
  options(tinytex.verbose = TRUE)
  knitr::opts_chunk$set(echo = F,message=F,warning=F,cache = F)
}


#' @export
copy_template_to_generator <- function(template_dir, output_dir){
  all_files <- list.files(template_dir, recursive = T, no.. = T, all.files = T, full.names = T)

  #sub-catalogs
  if(length(all_files)>0){
    copy_to <- file.path(output_dir,substring(dirname(all_files),nchar(template_dir)+1 ), basename(all_files))
    copy_to_dirs <- unique(dirname(copy_to))
    for (copt_to_dir in copy_to_dirs) {
      if(! dir.exists(copt_to_dir)){
        dir.create(copt_to_dir, recursive = T)
      }
    }

    file.copy(all_files,copy_to, overwrite = T, recursive = T)
  }
}

#' @export
prepare_generation <- function(id){
  output_dir <- file.path(dir_generation_path, id)
  if (!dir.exists(output_dir)){
    dir.create(output_dir, recursive = T)
  }
  output_file_start <- file.path(output_dir, "start")
  if(!file.exists(output_file_start)) {
        file.create(output_file_start)
  }
  return(output_dir)
}