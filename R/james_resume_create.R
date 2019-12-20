#xaringan::inf_mr("james resume.rmd")

resume = function(){
shell.exec(system.file("to_github/jamesconigrave_resume.pdf", package = "jamesconigrave"))
}

update_resume = function(){
  md = system.file("to_github/jamesconigrave_resume.rmd", package = "jamesconigrave")
  gh = system.file("to_github", package = "jamesconigrave")


  rmarkdown::render(md, output_file = system.file("to_github/docs/index.html", package = "jamesconigrave"))
  pagedown::chrome_print(md)

  if(dir.exists(paste0(gh,"/.git"))){

    shell(paste(glue::glue("cd {gh}"),
          "git init",
          "git remote add origin https://github.com/JConigrave/resume.git",
          sep = "&"))
  }

  shell(paste(glue::glue("cd {gh}"),
              "git add .",
              'git commit -m "automatic resume update"',
              'git push -f origin master',sep = "&"))
}
