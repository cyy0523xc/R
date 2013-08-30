# Copy from http://www.dataguru.cn/article-3206-1.html
# lab2使用R语言爬取淘宝网站的笔记本商品价格和名称
library(XML);
url1 <-"http://3c.taobao.com/detail.htm?spm=872.217037.254698.6.deIiSJ&spuid=205341228&cat=1101"
url2 <-"http://3c.taobao.com/detail.htm?spm=872.217037.254698.11.deIiSJ&spuid=203228104&cat=1101"
read_taobao <- function(url) {
  name_text <- ""
  price_text <- ""
  i <- 1
  for (i_url in url) {
    i_url2 <- htmlParse(i_url, encoding="UTF-8")  #读取html数据
    name <- getNodeSet(i_url2, "//div[@id='idetail']//div[@class='info-area']//div[@class='tlt clearfix']//h1")#通过xpath找到网页中的name
    #xpath://任意位置的  @是属性
    name_text_tmp <- xmlValue(name[[1]])  #提取name的内容
    price <- getNodeSet(i_url2, "//div[@id='idetail']//div[@class='info-area']//div[@class='key-info']//span[@class='price']")#通过xpath找到网页中的price
    price_text_tmp <- xmlValue(price[[1]])  #提取price的内容
    name_text[i] <- name_text_tmp
    price_text[i] <- price_text_tmp
    i <- i+1
  }
  data.frame(name=name_text, price=price_text)
}
url <- c(url1,url2)
#read_taobao(url)