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
#' @param push if TRUE, changes are pushed to github
#' @export update_resume

update_resume = function(push = TRUE){
  md = system.file("resume_files/jamesconigrave_resume.rmd", package = "jamesconigrave")
  gh = system.file("to_github", package = "jamesconigrave")
  root = system.file("", package = "jamesconigrave")

  resume_html = paste0(root, "/", "resume_files/docs/index.html")
  resume_pdf = paste0(root, "/", "resume_files/jamesconigrave_resume.pdf")

  rmarkdown::render(md,
                    output_file = resume_html)


  pagedown::chrome_print(
    input = resume_html,
    output = resume_pdf
  )

      pass = shell("git config --global user.password", intern = TRUE)
      username = shell("git config --global user.name", intern = TRUE)

  if(!dir.exists(paste0(gh,"/.git"))){
    message("git dir doesn't exist, initialising... ",glue::glue("{gh}"))

    shell(paste(glue::glue("cd {gh}"),
          "git clone https://github.com/JConigrave/resume.git",
          "cd resume",
          glue::glue("git config user.password {username}"),
          glue::glue("git config user.password {pass}"),
          sep = "&"))
  }

      file.copy(from = resume_html,
                to = system.file("to_github/resume/jamesconigrave_resume.html", package = "jamesconigrave"),
                overwrite = TRUE)
      file.copy(from = resume_pdf,
                to = system.file("to_github/resume/jamesconigrave_resume.pdf", package = "jamesconigrave"),
                overwrite = TRUE)

      if(push) {
        shell(
          paste(
            glue::glue("cd {gh}/resume"),
            "git add .",
            glue::glue("git config user.password {username}"),
            glue::glue("git config user.password {pass}"),
            glue::glue("git config user.password"),
            'git commit -m "automatic resume update"',
            glue::glue(
              'git push https://{username}:{pass}@github.com/conig/resume.git'
            ),
            sep = "&"
          )
        )
      }
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
