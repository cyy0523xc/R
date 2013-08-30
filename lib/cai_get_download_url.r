CaiGetDownloadUrl <- function(url) {
  # 从网易公开课读取视频的标题和下载地址
  # 
  # url <- "http://v.163.com/special/opencourse/machinelearning.html"
  # CaiGetDownloadUrl(url)
  #
  
  library(XML)
  html <- htmlParse(url, encoding="gb2312")   #读取html数据
  
  # title
  title <- getNodeSet(html, "//table[@id='list2']//tr//td[@class='u-ctitle']//a")
  title <- lapply(title, FUN=xmlValue)
  
  # url
  url <- getNodeSet(html, "//table[@id='list2']//tr//td[@class='u-cdown']//a")
  url <- lapply(url, FUN=function(x, name)xmlGetAttr(x, name), "href")
  
  # return
  n <- length(title)
  for (i in 1:n) {
    title[[i]] <- c(title[[i]], url[[i]])
  }
  
  title
}

