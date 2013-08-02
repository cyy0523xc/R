# 自动加载自己实现的函数库
# source("/home/windows/Dropbox/code/r/R/cai_init.r")
cai_init <- function() {
  lib_path <- "/home/windows/Dropbox/code/r/R/lib/"
  setwd(lib_path)
  lapply(list.files(pattern='\\.r$'), source)
}

cai_init()