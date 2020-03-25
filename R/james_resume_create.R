#xaringan::inf_mr("james resume.rmd")

#' resume
#'
#' open resume
#' @param path If provided, the resume is copied to that destination. In this instance, the file is not opened
#' @export resume

resume = function(path = NULL){

  location = system.file("to_github/jamesconigrave_resume.pdf", package = "jamesconigrave")

  if(!is.null(path)){
    file.copy(from = location, to = path, overwrite = T)
  }else{
  shell.exec(location)
  }
}

#' website
#'
#' open resume
#' @export website

website = function(){
    shell.exec("https://jconigrave.github.io/")
}

#' update_resume
#'
#' update resume on github
#' @export update_resume

update_resume = function(){
  md = system.file("to_github/jamesconigrave_resume.rmd", package = "jamesconigrave")
  gh = system.file("to_github", package = "jamesconigrave")



  rmarkdown::render(md,
                    output_file = system.file("to_github/docs/index.html", package = "jamesconigrave"))
  pagedown::chrome_print(
    input = system.file("to_github/docs/index.html", package = "jamesconigrave"),
    output =
      system.file("to_github/jamesconigrave_resume.pdf", package = "jamesconigrave")
  )

  if(!dir.exists(paste0(gh,"/.git"))){
    message("git dir doesn't exist, initialising... ",glue::glue("{gh}"))

    pass = shell("git config --global user.password", intern = TRUE)

    shell(paste(glue::glue("cd {gh}"),
          "git init",
          glue::glue("git config user.password {pass}"),
          "git remote add origin https://github.com/JConigrave/resume.git",
          sep = "&"))
  }



  shell(paste(glue::glue("cd {gh}"),
              "git add .",
              'git commit -m "automatic resume update"',
              'git push --force origin master',sep = "&"))
}





fix_case = function(string,
                    words = c(
                      "aboriginal",
                      "torres",
                      "strait",
                      "islander",
                      "australian",
                      "australia",
                      "australian's"
                    )) {
  words2 = stringr::str_to_title(words)

  for (i in seq_along(words)) {
    string = gsub(words[i], words2[i], string)
  }

  return(string)

}
