CaiPos <- function (p.data) 
{
    # pos数据分析

    # 配置
    c.week <- c('星期一','星期二', '星期三', '星期四', '星期五', '星期六', '星期日')

    # 数据清洗与转换
    # 日期格式化
    p.data['销售日期'] = as.Date(p.data[['销售日期']])

    # 增加工作时间段字段：上午，下午
    p.data['工作时间段'] <- sapply(strsplit(as.vector(p.data[['时间']]), ' '), function(x){x[2]})

    # return data
    retdata <- list()

    # 基本数据统计
    t.stat <- list()
    t.stat['成交次数'] <- length(unique(p.data[['单号']]))
    t.stat['天数'] <- length(unique(p.data[['销售日期']]))
    #t.stat['日均成交次数'] <- t.stat[[]]
    #t.stat[''] <- length(unique[['']])
    #t.stat[''] <- length(unique[['']])
    #t.stat[''] <- length(unique[['']])
    #t.stat[''] <- length(unique[['']])
    #t.stat[''] <- length(unique[['']])
    #t.stat[''] <- length(unique[['']])
    #t.stat[''] <- length(unique[['']])

    # 每日趋势
    retdata$day <- CaiPosField(p.data, '销售日期', p.sortby.field = '销售日期', p.decreasing = FALSE)

    # 星期分析
    retdata$weekday <- CaiPosField(p.data, '星期', p.sort.fields = c.week)

    # 上下午分析
    retdata$worktime <- CaiPosField(p.data, '工作时间段', p.sort.fields = c('上午', '下午'))

    # 时段分析
    retdata$time <- CaiPosField(p.data, '时间段', p.sortby.field = '时间段', p.decreasing = FALSE)

    # 品类分析
    retdata$cat <- CaiPosField(p.data, '品类')

    # 品牌分析
    retdata$brd <- CaiPosField(p.data, '品牌')

    # 功效分析
    retdata$attr <- CaiPosField(p.data, '护肤功效')

    # 商品分析
    retdata$product <- CaiPosField(p.data, '品名')

    # 供应商分析
    retdata$source <- CaiPosField(p.data, '供应商编码')

    # 价格带分析
    retdata$price <- CaiCalPosSection(p.data, '销售价')

    # 折扣率分析
    retdata$off <- CaiCalPosSection(p.data, '折扣率')

    # 成交件数分析
    retdata$pnum <- CaiPosField(p.data, '数量')

    # 订单分析 
    retdata$order_detail = list()
    # 订单对应的商品数量
    retdata$order_detail  <- aggregate(p.data['数量'], p.data['单号'], sum)
    names(retdata$order_detail) <- c('单号', '商品件数')
    # 订单对应的成交额
    t.income<- aggregate(p.data['销售价'] * p.data['数量'], p.data['单号'], sum)
    retdata$order_detail['成交额']<- t.income['销售价']
    # 订单对应的折扣率
    t.off   <- aggregate(p.data['原价金额'], p.data['单号'], sum)
    retdata$order_detail['折扣率'] <- t.income['销售价'] / t.off['原价金额'] 

    # 成交商品件数分析
    retdata$order <- list()
    retdata$order <- aggregate(retdata$order_detail['成交额'], retdata$order_detail['商品件数'], sum)
    t.pnum   <- table(retdata$order_detail['商品件数'])
    t.off    <- aggregate(retdata$order_detail['折扣率'], retdata$order_detail['商品件数'], mean)
    retdata$order['成交次数'] <- t.pnum
    retdata$order['折扣率'] <- t.off['折扣率']
    
    
    # return
    retdata
}

CaiCalPosSection <- function (p.data, p.index.field, p.steps = 10) {
    # 连续数据的区间的数据透视，例如价格带分析
    # p.index.field: 待分析的字段
    # p.steps: 区间的个数
    t.max <- max(p.data[p.index.field])
    t.min <- min(p.data[p.index.field])
    t.sec <- (t.max - t.min) / p.steps
    t.sec.field <- paste('section-', p.index.field, sep='')

    # 计算区间字段
    p.data[t.sec.field] <- t.min + t.sec * trunc((p.data[p.index.field] - t.min) / t.sec)
    
    # 字段透视
    CaiPosField(p.data, t.sec.field)
}

CaiPosField <- function (p.data, p.index.field, p.sortby.field = '', p.sort.fields = c(), p.decreasing = TRUE) {
    # 对一个字段进行透视

    retdata <- list()    # 返回值

    # 函数参数：数值
    fun.data <- list()
    fun.data['成交次数'] <- p.data['单号']
    fun.data['单品数'] <- p.data['品名']
    fun.data['商品件数'] <- p.data['数量']
    fun.data['成交额'] <- p.data['销售价'] * p.data['数量']
    fun.data['折扣率'] <- p.data['折扣率']
    #fun.data[''] <- p.data['']

    # 函数参数：字段对应的处理函数
    fun.fun <- list()
    fun.fun[['成交次数']]  <- CaiCountUnique
    fun.fun[['单品数']]    <- CaiCountUnique
    fun.fun[['商品件数']]  <- sum
    fun.fun[['成交额']]    <- sum
    fun.fun[['折扣率']]    <- mean

    # 计算
    retdata <- CaiAggregate(fun.data, p.data[p.index.field], fun.fun)

    # 格式化返回数据
    if (length(p.sort.fields) > 1) {
        # 根据指定的字段顺序进行排序
        rownames(retdata) <- retdata[,1]
        retdata           <- retdata[p.sort.fields,]
        tmp <- dim(retdata)
        rownames(retdata) <- 1:tmp[1]
    } else if ('' != p.sortby.field) {
        # 数据排序
        retdata <- CaiDataframeOrder(retdata, p.sortby.field, p.decreasing)
        tmp <- dim(retdata)
        rownames(retdata) <- 1:tmp[1]
    }

    print(retdata)
    return(retdata)


    # 计算累计占比
    names(retdata$income) <- c(p.index.field, '成交额')
    # 折扣率
    retdata$off     = aggregate(p.data['折扣率'], p.data[p.index.field], mean)
    names(retdata$off) <- c(p.index.field, '折扣率')
    # 连带率
    retdata$stat[p.index.field] <- retdata$count[p.index.field]
    retdata$stat['连带率'] <- retdata$pnum['商品件数'] / retdata$count['成交次数']
    retdata$stat['件单价'] <- retdata$income['成交额'] / retdata$pnum['商品件数']
    retdata$stat['成交价'] <- retdata$income['成交额'] / retdata$count['成交次数']
    retdata$stat <- as.data.frame(retdata$stat)


    retdata
}

CaiCountUnique <- function (x) {
    # 计算向量x的唯一值的个数（去重）
    length(unique(x))
}

CaiDataframeOrder <- function (p.data.frame, p.sortby, p.decreasing = FALSE) {
    # 将data.frame按照某个字段排序
    t <- names(p.data.frame)
    index <- 1
    for (i in t) {
        if (p.sortby == i) {
            break
        }
        index <- index + 1
    }

    print(index)

    # return
    p.data.frame[order(p.data.frame[, index], decreasing = p.decreasing),]
}


CaiAggregate <- function (
                           p.data.x,               # 待计算的变量
                           p.data.by,              # 分组变量
                           p.data.fun              # 和p.data.y对应，不同的列值使用不同的函数来处理
) {
    # 对变量p.data.x进行透视计算

    retdata <- list()
    x.names <- names(p.data.x)

    # 循环处理x的每一个子list
    for (x in x.names) {
        if (0 == length(retdata)) {
            retdata <- aggregate(p.data.x[x], p.data.by, p.data.fun[[x]])
        } else {
            tmp <- aggregate(p.data.x[x], p.data.by, p.data.fun[[x]])
            retdata[x] <- tmp[x]
        }
    }

    retdata
}
