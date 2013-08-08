CaiAnalyseEiMac <- function(x) {
  # 分析应用中，一个mac对应多个imei地址的情况
  #
  # Args:
  #   x: list类型，待分析数据
  #     x$aid: 应用ID，格式例如：aid=23。（下面的格式也类同）
  #     x$ei:  imei列表
  #     x$mac: mac列表
  # Return:
  #   list，对应多个imei的mac的占比
  
  # 格式化应用数据
  x$aid <- substr(x$aid, 5, 100)
  n <- length(x$aid)
  aid.lst <- unique(x$aid)
  
  # 计算总体一个mac对应多个imei的情况
  tmp.lst <- list()
  mac.unique <- unique(x$mac)
  for (mac in mac.unique) {
    tmp.lst[[mac]] <- c()
  }
  
  # 把imei都加入mac列表
  for (i in 1:n) {
    tmp.lst[[x$mac[i]]] <- c(tmp.lst[[x$mac[i]]], x$ei[i])
  }
  
  # 汇总唯一值的个数
  tmp.lst <- lapply(tmp.lst, FUN=function(x){return(length(unique(x)))})
  
  tmp.mac.n <- length(tmp.lst)
  print(c(tmp.mac.n, n/tmp.mac.n, tmp.lst[[which.max(tmp.lst)]], length(tmp.lst[tmp.lst > 1]) / tmp.mac.n))
  
  # 计算每个应用的一个mac对应多个imei的情况
  
}