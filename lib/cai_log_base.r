CaiLogBase <- function(filename) {
  # LOG文件处理的基础库
  #   Log文件分行保存，每行的格式例如：aid=29071&ei=866335010754292&mac=2c26c58ac3f2
  #   说明：&是分隔符，=号左边是字段名，右边是对应的值
  #
  # Args:
  #   filename: log文件地址
  # Return:
  #   CaiLogBaseCls类，S3类
  
  # 读入数据
  x <- CaiLogFileLoad(filename)
  class(x) <- "CaiLogBaseCls"
}