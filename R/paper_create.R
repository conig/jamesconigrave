#' new_paper
#'
#' Create a paper subdirectory
#' @param path path to new folder
#' @export

new_paper <- function(path){
  name <- basename(path)
  dir <-dirname(path)

  existing_files <- list.files(dir)
  starts_number <- grepl("^\\d\\_", existing_files)
  existing_files <- existing_files[starts_number]
  numbers <- gsub("\\_.*", "", existing_files)
  if(length(existing_files)>0){
    new <- max(as.numeric(numbers)) + 1
  } else{
    new <- 1
  }

  new_path <- glue::glue("{dir}/{new}_{name}/")

  coathor_path <- paste0(new_path,"/","0_coauthor feedback/")
  processed_path <- paste0(coathor_path,"/","0_processed/")
  inbox_path <- paste0(coathor_path,"/","1_inbox/")

  data_path <- paste0(new_path,"/","1_data/")

  reference_path <- paste0(new_path,"/","2_references/")

  dir.create(new_path)
  dir.create(coathor_path)
  dir.create(processed_path)
  dir.create(inbox_path)
  dir.create(data_path)
  dir.create(reference_path)

  }

