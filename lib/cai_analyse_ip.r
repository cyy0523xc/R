CaiAnalyseIp <- function(x) {
  # 根据IP的分布数据判断应用是否作弊
  # 
  # Args:
  #  x: 列表数据
  #     x$aid: 向量，格式如：aid=2222
  #     x$ip:  向量，格式如：ip=127.3.3.3
  # Returns:
  #   每个应用的IP分布数据
  
  
  # 格式化数据
  x$aid <- substr(x$aid, 5, 100)
  n <- length(x$aid)
  aid <- unique(x$aid)
  
  # ip分布概况
  x$ip <- substr(x$ip, 4, 100)
  ip_table <- as.vector(table(x$ip))
  print(c("ip分布概况:", n, length(ip_table), mean(ip_table), sd(ip_table), max(ip_table)))
  
  # 每个应用的ip分布情况
  ip_lst <- list()
  for (a in aid) {
    tmp_ip <- x$ip[which(x$aid == a)]
    n <- length(tmp_ip)
    
    if (n > 50) {
      tmp_ip_table <- as.vector(table(tmp_ip))
      ip_lst[[a]] <- c(n, length(tmp_ip_table), mean(tmp_ip_table), sd(tmp_ip_table), max(tmp_ip_table))
    }
  }
  
  # 按mean值排序
  ip_lst <- (ip_lst[order(sapply(ip_lst, function(x){return(x[3])}))])
  print("order by mean OK")
  
  # 计算任意两个应用之间的ip重合度（交集/并集）
  aid_sort <- as.integer(aid)
  aid_sort <- aid_sort[order(aid_sort)]
  
  lst <- list()
  for (i in aid) {
    lst[[i]] = unique(x$ip[which(x$aid == i)])
  }
  
  tmp_lst <- list()      # 记录重合的应用
  for (i in aid_sort) {
    si <- lst[[as.character(i)]]
    #print(c("APP: ", i))
    for (j in aid_sort) {
      if (j > i) {
        sj <- lst[[as.character(j)]]
        
        li <- length(intersect(si,sj))
        ui <- length(union(si, sj))
        tmp <- li / ui
        #print(c("异常", i, j, li, ui, tmp, length(si), length(sj)))
        if (tmp > 0.1) {
          tmp_lst <- rbind(tmp_lst, c(i, j, li, ui, tmp))
          #print(c("IP重合率", i, j, li, ui, tmp))
          #print(list(si,sj))
        }
      }
    }
  }
  
  print(tmp_lst)
  
  # return 
  return(ip_lst)
}