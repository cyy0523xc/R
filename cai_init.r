CaiInit <- function(p.path="") {
  # 自动加载自己实现的函数库
  # source("/home/windows/Dropbox/code/r/R/cai_init.r")
  #
  # Args:
  #   p.path: 函数库的目录路径, 例如：D:/Dropbox/code/r/R/lib
  
  # 能否获取当前文件所在的路径？
  lib.path <- ifelse(p.path == "", "/home/windows/Dropbox/code/r/R/lib/", p.path)
  #setwd(lib.path)
  
  CaiSource <- function(x, p.path) {
    source(paste(p.path, x, sep=""))
  }
  lapply(list.files(path=lib.path, pattern='\\.[rR]$'), CaiSource, lib.path)
}

CaiInit()

# 保存工作空间的内容
# save.image("/home/windows/Dropbox/code/r/R/data/workspace.RData")

print("init OK")