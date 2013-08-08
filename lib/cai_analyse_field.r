CaiAnalyseField <- function(x, p.field = "ip", p.rate = 0.1) {
  # 根据某个字段的分布数据判断应用是否作弊
  # 
  # Args:
  #  x: 列表数据
  #     x$aid: 向量，格式如：aid=2222
  #     x$ip:  向量，格式如：ip=127.3.3.3
  #     x$ei:  向量，格式如：ei=a00000362e82c8
  #  p.field: 待分析的字段，值为：ip, ei 
  #  p.rate:  重合度的阀值，超过这个阀值的才会输出
  # 
  # Returns:
  #   每个应用的IP分布数据
  #   并输出每两个应用之间重叠用户的比例
  
  # 格式化应用数据
  x$aid <- substr(x$aid, 5, 100)
  n <- length(x$aid)
  aid.lst <- unique(x$aid)

  # 栏目的分布概况
  x[[p.field]] <- substr(x[[p.field]], 4, 100)
  field.table <- as.vector(table(x[[p.field]]))
  print(c("分布概况:", n, length(field.table), mean(field.table), sd(field.table), max(field.table)))
  
  # 每个应用的栏目分布情况
  field.lst <- list()
  for (aid in aid.lst) {
    tmp.field.lst <- x[[p.field]][which(x$aid == aid)]
    n <- length(tmp.field.lst)
    
    # 大于50个数据的才计算（统计特性比较明显）
    if (n > 50) {
      tmp.field.tb <- as.vector(table(tmp.field.lst))
      field.lst[[aid]] <- c(n, length(tmp.field.tb), mean(tmp.field.tb), sd(tmp.field.tb), max(tmp.field.tb))
    }
  }
  
  # 按mean值排序
  field.lst <- field.lst[order(sapply(field.lst, function(x){return(x[3])}))]
  print("order by mean OK")
  
  # 排序
  aid.sort <- as.integer(aid.lst)
  aid.sort <- aid.sort[order(aid.sort)]
  
  # 计算任意两个应用之间的ip重合度（交集/并集）
  tmp.field.lst <- list()
  for (i in aid.lst) {
    tmp.field.lst[[i]] <- unique(x[[p.field]][which(x$aid == i)])
  }
  print("coincidence degree BEGIN")
  
  options(scipen=3)                     # 避免输出时使用科学计数法
  coincidence.degree.lst <- list()      # 记录重合的应用
  index <- 0
  for (i in aid.sort) {
    si <- tmp.field.lst[[as.character(i)]]
    for (j in aid.sort) {
      if (j > i) {
        sj <- tmp.field.lst[[as.character(j)]]
        
        li <- length(intersect(si,sj))
        ui <- length(union(si, sj))
        tmp.rate <- li / ui
        #print(c(si,sj,li,ui,tmp.rate))
        #if (tmp.rate > p.rate) {
        if (tmp.rate < p.rate) {
          # 保存重合度比较高的应用及相关参数
          #coincidence.degree.lst <- rbind(coincidence.degree.lst, c(i, j, li, ui, tmp.rate))
          index <- index + 1
          coincidence.degree.lst[[index]] <- c(i, j, li, ui, tmp.rate)
        }
      }
    }
  }
  
  # 输出重合度
  #print(order(sapply(coincidence.degree.lst, function(x){return(x[5])})))
  coincidence.degree.lst <- coincidence.degree.lst[order(sapply(coincidence.degree.lst, function(x){return(x[5])}))]
  print(coincidence.degree.lst)
  print(class(coincidence.degree.lst))
  
  # 字段数据的分布情况
  return(field.lst)
}