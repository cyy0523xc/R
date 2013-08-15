CaiAnalyseEiMacV2 <- function(x) {
  # 分析应用中，一个mac对应多个imei地址的情况
  #
  # Version: 20130814
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
  
  # 避免输出时使用科学计数法
  options(scipen=3)
  
  # 计算去重后的频数
  # 避免在循环里面不断的生成匿名函数
  x.rate.cal.fn <- function(x) length(unique(x))
  
  # 计算总体一个mac对应多个imei的情况
  tmp.lst <- tapply(x$ei, x$mac, x.rate.cal.fn)
  tmp.mac.n <- length(tmp.lst)
  print(c(tmp.mac.n, n/tmp.mac.n, tmp.lst[[which.max(tmp.lst)]], length(tmp.lst[tmp.lst > 1]) / tmp.mac.n))
  
  # 计算每个应用的一个mac对应多个imei的情况
  app.rate.lst <- list()  # 保存每个app上一个mac地址对应超过两个imei的比例数据
  for (aid in aid.lst) {
    tmp.aid.lst <- x$aid == aid
    tmp.lst <- tapply(x$ei[tmp.aid.lst], x$mac[tmp.aid.lst], x.rate.cal.fn)
    tmp.n <- length(tmp.lst)
    tmp.more1.n <- length(tmp.lst[tmp.lst > 1])
    
    # 可能异常的才需要记录
    if (tmp.more1.n > 1) {
      tmp.more2.n <- length(tmp.lst[tmp.lst > 2])
      tmp.more5.n <- length(tmp.lst[tmp.lst > 5])
      
      app.rate.lst[[aid]] <- c(tmp.n, tmp.more1.n, tmp.more2.n, tmp.more5.n, max(tmp.lst))
      
      # test
      if (tmp.more5.n > 0) {
        print(aid)
        print(tmp.lst[tmp.lst > 5])
      }
    }
  }
  
  # 按第一列排序
  app.rate.lst[order(sapply(app.rate.lst, FUN = function(x)x[1]))]
}