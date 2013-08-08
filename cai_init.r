# 自动加载自己实现的函数库
# source("/home/windows/Dropbox/code/r/R/cai_init.r")
cai_init <- function() {
  lib_path <- "/home/windows/Dropbox/code/r/R/lib/"   # 能否获取当前文件所在的路径？
  setwd(lib_path)
  lapply(list.files(pattern='\\.r$'), source)
}

cai_init()

print("init OK")