CaiInit <- function() {
  # 自动加载自己实现的函数库
  # source("/home/windows/Dropbox/code/r/R/cai_init.r")
  
  lib.path <- "/home/windows/Dropbox/code/r/R/lib/"   # 能否获取当前文件所在的路径？
  setwd(lib.path)
  lapply(list.files(pattern='\\.r$'), source)
}

CaiInit()

# 保存工作空间的内容
save.image("/home/windows/Dropbox/code/r/R/data/workspace.RData")

print("init OK")