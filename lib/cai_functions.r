
CaiLoadRData <- function() {
  # 加载默认的RData
  
  load("/home/windows/Dropbox/code/r/R/data/workspace.RData")
}

CaiLogFileLoad <- function(filename) {
  # LOG文件的数据格式化
  #   Log文件分行保存，每行的格式例如：aid=29071&ei=866335010754292&mac=2c26c58ac3f2
  #   说明：&是分隔符，=号左边是字段名，右边是对应的值
  #
  # Args:
  #   filename: log文件地址
  # Return:
  #   list
  
  # 处理字段名
  x <- scan(filename, what="", sep="&", nlines=1)
  fields.name <- sapply(x, FUN=function(x){x <- strsplit(x, split="=")[[1]][1]})
  fields.name <- as.list(tapply(fields.name, fields.name, FUN=function(x){x}))
  
  # 读入数据
  scan(filename, what=fields.name, sep="&")
}