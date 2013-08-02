# 小时分布计算
# source('/home/windows/Dropbox/code/r/cai_analyse.r')
cai_analyse <- function(x) {
  # 格式化数据
  x$aid <- substr(x$aid, 5, 100)
  n <- length(x$aid)
  aid <- unique(x$aid)
  
  # drt总体分布情况
  x$drt <- as.integer(substr(x$drt, 16, 17))  #转化成小时
  drt_summary <- table(x$drt) / n
  print(drt_summary)
  drt_names <- names(drt_summary)
  
  #browser()
  aid_drt_var <- list()                  # 记录每个应用的小时分布与总体分布的方差
  for (i in aid) {
    #print(i)
    tmp_drt <- x$drt[which(x$aid==i)]
    if (length(tmp_drt) > 100) {
      tmp_drt_summary <- table(tmp_drt)/length(tmp_drt)
      tmp_drt_summary_names <- names(tmp_drt_summary)
      #print(tmp_drt_summary)
      
      # 计算方差
      drt_var <- 0
      for (j in drt_names) {
        drt_var <- drt_var + ifelse(is.na(tmp_drt_summary[j]), 0, ((tmp_drt_summary[[j]] - drt_summary[[j]])/drt_summary[[j]])^2)
      }
      
      aid_drt_var[i] <- drt_var
      
      if(drt_var>15) {
        print("应用")
        print(c(i, length(tmp_drt), drt_var))
        print(tmp_drt_summary)
      }
    }
  }
  #browser()
  # 输出差异情况
  #print(aid_drt_var)
  
  err_lst <- aid_drt_var[aid_drt_var>0.95]
  
  return(aid_drt_var)
}
