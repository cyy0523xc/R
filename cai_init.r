CaiInit <- function() {
  # 自动加载自己实现的函数库
  # source("/home/windows/Dropbox/code/r/R/cai_init.r")
  # source("D:/Dropbox/code/r/R/cai_init.r")
  #
  # Args:
  #   p.path: 函数库的目录路径, 例如：D:/Dropbox/code/r/R/lib
  
  # 能否获取当前文件所在的路径？
  lib.path <- ifelse(file.exists('/home/windows/Dropbox/code/r/R/cai_init.r'),
                     '/home/windows/Dropbox/code/r/R/lib/',
                     'D:/Dropbox/code/r/R/lib/')
  #setwd(lib.path)
  
  lib.path
  CaiSource <- function(x, p.path) {
    print(x)
    source(paste(p.path, x, sep=""))
    print(' ---> ok')
  }
  lapply(list.files(path=lib.path, pattern='\\.[rR]$'), CaiSource, lib.path)
}

CaiInit()

# 载入需用的packages
# FNN: Fast Nearest Neighbour
# e1071提供朴素贝叶斯分类器
#CaiInstallPackages(c("XML", "FNN", "RCurl", "e1071", "RODBC", "rjson"))


# 保存工作空间的内容
# save.image("/home/windows/Dropbox/code/r/R/data/workspace.RData")

print("init OK")