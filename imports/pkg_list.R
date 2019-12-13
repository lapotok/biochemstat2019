# Shortcut: source('https://raw.githubusercontent.com/lapotok/biochemstat2019/master/imports/pkg_list.R')

options(repos = c(CRAN = "http://cran.rstudio.com"))

build_list = function(from_scratch = F){
  # список пакетов базового репозитория
  base.packages <<- c("Amelia","boot","broom","car","caret","corrplot","cowplot","curl","devtools","esquisse","furrr","gdtools","GGally","ggforce","ggplot2","ggpubr","ggrepel","ggridges","ggstance","ggthemes","glue","gridExtra","gtools","hexbin","httr","investr","jsonlite","kableExtra","knitr","lme4","lmtest","lubridate","magrittr","mice","missForest","modelr","mvtnorm","naniar","officer","openxlsx","plotly","prettydoc","profvis","purrr","randomForest","readr","readxl","repr","reprex","reshape","reshape2","rio","rmarkdown","rvest","rvg","sandwich","scales","selectr","shiny","sinaplot","sjPlot","tidyverse","multcomp","drc","RColorBrewer","nlme","caret","ipred","effsize","dunn.test","magrittr","remedy","ggalt","ggExtra","gsheet","jtools","export", "ggmosaic", "vcd")
  dependencies_ <<- unlist(tools::package_dependencies(base.packages))
  base.packages.minimal <<- base.packages[!(base.packages %in% dependencies_)]
  
  # список пакетов из github
  git.packages <<- c("patchwork"="thomasp85/patchwork", "crayon"="r-lib/crayon", "rstatix"="kassambara/rstatix")
  
  # составляем списки того, что надо поставить
  if (from_scratch) 
    installed <<- ""
  else
    installed <<- as.character(installed.packages()[,"Package"])
  
  uninstalled_cran <<- setdiff(base.packages, installed)
  uninstalled_git <<- setdiff(names(git.packages), installed)
  uninstalled <<- c(uninstalled_cran, uninstalled_git)
}
print_pkg_list = function(){
  if (length(uninstalled_cran))  cat( paste0("install.packages( c(\"", paste0(uninstalled_cran, collapse='\", \"'), "\"), dependencies = TRUE, type = \"binary\" )\n\n"))
  if (length(git.packages[uninstalled_git])) cat( paste0("devtools::install_github( c(\"", paste0(git.packages[uninstalled_git], collapse='\", \"'), "\"), upgrade = \"never\", dependencies = TRUE )\n\n"))
}

print_conda_list = function(){
  cat(paste0("conda install r-", paste0(tolower(base.packages.minimal), collapse=' r-'), "\n"))
}

# ставим
install_pkgs = function(){
  install.packages(uninstalled_cran, dependencies = TRUE, type ="binary")
  devtools::install_github(git.packages[uninstalled_git], upgrade = "never", dependencies = TRUE)
}

installation_report = function(){
  # составляем списки того, что осталось непоставленным
  installed_upd <<- as.character(installed.packages()[,"Package"])
  uninstalled_upd <<- setdiff(c(base.packages, names(git.packages)), installed_upd)
  # пишем отчет
  cat(crayon::bold$underline('\nPackage installation report\n'))
  if (length(uninstalled) == 0) {
    cat(paste0(' ', crayon::green(clisymbols::symbol$tick), " No packages need to be installed.\n")) 
  } else {
    cat(paste0('Following packages (', length(uninstalled), ') needed to be installed:\n'))
    for (p in uninstalled) cat(' ', paste0(crayon::yellow(clisymbols::symbol$star), ' ' , crayon::style(p, 'gray'), '\n'))
  }
  if(length(setdiff(uninstalled, uninstalled_upd))>0){
    cat(paste0('Following packages (', length(setdiff(uninstalled, uninstalled_upd)), ') were succesfully installed:\n'))
    for (p in setdiff(uninstalled, uninstalled_upd)) cat(paste(' ', crayon::green(clisymbols::symbol$tick), crayon::style(p, 'gray'), '\n'))
  }
  if(length(uninstalled_upd)>0) {
    cat(paste0(crayon::red('Following packages (', length(uninstalled_upd), ') are still missing:\n')))
    for (p in uninstalled_upd) cat(' ', paste(crayon::red(clisymbols::symbol$cross), crayon::style(p, 'gray'), '\n'))
  }
}