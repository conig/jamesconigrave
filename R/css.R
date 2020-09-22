#' css
#'
#' modify css and save to file
#' @param path to file
#' @param line_height gap between lines
#' @param block_margin margin between blocks

css = function(path = NULL, line_height = 1.2, block_margin = .075){
string <- r"(.blocks {
  break-inside: avoid;
}

.pagedjs_page:not(:first-of-type) {
  --sidebar-width: 7.90rem;
  --sidebar-background-color: #ffffff;
  --main-width: calc(var(--content-width) - var(--sidebar-width));
  --decorator-horizontal-margin: 0.2in;
}

body {
    width: var(--content-width);
    font-family: serif !important;
    line-height: ##line_height##;
}

h3 {
    font-weight: normal;
}

.date{
    padding-top: calc(var(--row-blocks-padding-top) + 0.04rem)!important;
    font-size: .8rem;
}

.details{
    padding-top: calc(var(--row-blocks-padding-top) - 0.1rem)!important;v
}

.main-block {
    margin-top: ##block_margin##in;
}

/* table of contents, experiment */

   <link type="text/css" rel="stylesheet" href="jquery.tocify.css" />
)"

out <- hash_replace(string)
write(out, path)

}


hash_replace = function(string, sample = NULL, envir = parent.frame()){
  while(grepl(".*##(.+)##.*",string)){
    hash = gsub(".*##(.+)##.*", "\\1", string)
    replace = as.character(eval(parse(text = hash), envir = envir))

    if(!is.null(sample)) {
      replace = sample(replace, sample)
    }

    to_replace = paste0("##",hash,"##")
    to_replace = gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", to_replace) # escape regex

    string = gsub(to_replace,replace,string)
  }
  return(string)
}
