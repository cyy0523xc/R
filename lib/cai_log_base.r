CaiLogBase.get.value.name <- function (x) {
  # 对aid=29071这样的向量，得到值29071
  strsplit(x, split="=")[[1]][2]
}

CaiLogBase.get.key.name <- function (x) {
  # 对aid=29071这样的向量，得到key aid
  strsplit(x, split="=")[[1]][1]
}

CaiLogBase.array.barplot <- function (arr, p.plot.threshold = 0) {
  # 对数组arr画柱状图
  # 
  # Args:
  #   arr: array
  #   p.plot.threshold: 阀值，小于该值的不输出
  
  if (p.plot.threshold >= 0) {
    tmp.plot <- arr[arr > p.plot.threshold]
  } else {
    tmp.plot <- arr
  }
  tmp.plot.n <- sapply(names(tmp.plot), CaiLogBase.get.value.name)
  barplot(beside=T, tmp.plot, tmp.plot.n, width=0.8, space=0.3)
}

CaiLogBase.load.old <- function(filename) {
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
  fields.name <- sapply(x, FUN=function(x){strsplit(x, split="=")[[1]][1]})
  names(fields.name) <- NULL
  names(fields.name) <- fields.name
  fields.name <- as.list(fields.name)
  
  # 读入数据
  scan(filename, what=fields.name, sep="&")
}

CaiLogBase.load <- function(filename) {
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
  fields.name <- sapply(x, FUN=CaiLogBase.get.key.name)
  tmp.fields.name <- vector("list", length(fields.name))
  names(tmp.fields.name) <- fields.name
  
  # 读入数据
  fields.name = lapply(tmp.fields.name, function(x){""})
  scan(filename, what=fields.name, sep="&")
}

CaiLogBase.one.field.stat <- function(x, p.is.total = F, p.plot.threshold = -1) {
  # 对一个向量进行频数统计
  #
  # Args:
  #   x: 向量
  #   p.is.total: 如为T，则统计频数，如为F，则统计频率
  #   p.plot.threshold: 做图的阀值，<0 ：no plot；>=0：plot
  # Returns:
  #   array
  
  tmp.x <- table(x)
  tmp.x <- tmp.x[order(tmp.x)]
  if (F == p.is.total) {
    tmp.x <- tmp.x / length(x)
  }
  
  # 作图
  if (p.plot.threshold >= 0) {
    CaiLogBase.array.barplot(tmp.x, p.plot.threshold)
  }
  tmp.x
}

CaiLogBase.two.fields.relate.stat <- function(y, x, p.is.total = F, p.plot.threshold = -1) {
  # 变量y关联到分组变量x上的频数统计
  # 例如：统计每个应用（x）的城市（y）分布
  #
  # Args:
  #   y: 待汇总的变量
  #   x: 分组变量
  #   p.is.total: 如为T，则统计频数，如为F，则统计频率
  # Returns:
  #   array
  
  tmp.y <- tapply(y, x, FUN=function(x)length(unique(x)))
  
  if (p.is.total == T) {
    tmp.y <- tmp.y[order(tmp.y)]
  } else {
    tmp.y <- tmp.y[order(tmp.y)] / length(y)
  }
  
  # 作图
  if (p.plot.threshold >= 0) {
    CaiLogBase.array.barplot(tmp.y, p.plot.threshold)
  }
  
  tmp.y
}

CaiLogBase.two.fields.rep.stat <- function(x, y) {
  # 两个向量间重复值的统计
  # 例如：某个应用今天的数据和昨天的数据的用户重复率计算，或者两个应用间的用户重复率计算
  # 
  # Args:
  #   x: 向量
  #   y: 向量
  # Returns:
  #   list
  
  tmp.x <- unique(x)
  tmp.y <- unique(y)
  
  length(intersect(tmp.x, tmp.y)) / length(union(tmp.x, tmp.y))
}

CaiLogBase.fields.rep.relate.stat <- function(y1, y2, x) {
  # 计算向量y1和y2在分组变量x下的重复率
  
  tmp.x <- unique(x)
  rt.lst <- vector("list", length=length(tmp.x))
  for (i in tmp.x) {
    tmp.i <- x == i
    rt.lst[[i]] <- CaiLogBase.two.fields.rep.stat(y1[tmp.i], y2[tmp.i])
  }
  
  rt.lst
}

