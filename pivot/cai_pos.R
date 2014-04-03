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

    # 每日趋势
    retdata$day <- CaiPosField(p.data, '销售日期', TRUE)

    # 星期分析
    retdata$weekday <- CaiPosField(p.data, '星期', TRUE, c.week)

    # 上下午分析
    retdata$worktime <- CaiPosField(p.data, '工作时间段', TRUE, c('上午', '下午'))

    # 时段分析
    retdata$time <- CaiPosField(p.data, '时间段', TRUE)

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
    retdata$order_detail$pnum  <- aggregate(p.data['数量'], p.data['单号'], sum)
    names(retdata$order_detail$pnum) <- c('单号', '商品件数')
    # 订单对应的成交额
    retdata$order_detail$income<- aggregate(p.data['销售价'] * p.data['数量'], p.data['单号'], sum)
    names(retdata$order_detail$income) <- c('单号', '成交额')
    # 订单对应的折扣率
    retdata$order_detail$off   <- aggregate(p.data['原价金额'], p.data['单号'], sum)
    retdata$order_detail$off   <- retdata$order_detail$off / retdata$order_detail$price
    names(retdata$order_detail$off) <- c('单号', '折扣率')

    # 订单分析
    retdata$order <- list()
    retdata$order$pnum   <- table(retdata$order_detail$pnum['商品件数'])
    retdata$order$income <- aggregate(retdata$order_detail$income['成交额'], retdata$order_detail$pnum['商品件数'], sum)
    #retdata$order$off    <- aggregate(retdata$order_detail$off['销售价'], retdata$order_detail$pnum['数量'], sum)
    
    
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
    CaiPosField(p.data, t.sec.field, p.sortby.field=TRUE)
}

CaiPosField <- function (p.data, p.index.field, p.sortby.field = FALSE, p.sort.fields = c()) {
    # 对一个字段进行透视

    retdata = list()
    # 订单数量
    retdata$count   <- aggregate(p.data['单号'], p.data[p.index.field], function(x){length(unique(x))})
    names(retdata$count) <- c(p.index.field, '成交次数')
    # 单品数
    retdata$product = aggregate(p.data['品名'], p.data[p.index.field], function(x){length(unique(x))})
    names(retdata$product) <- c(p.index.field, '单品数')
    # 商品件数
    retdata$pnum    = aggregate(p.data['数量'], p.data[p.index.field], sum)
    names(retdata$pnum) <- c(p.index.field, '商品件数')
    # 成交额
    retdata$income  = aggregate(p.data['销售价'] * p.data['数量'], p.data[p.index.field], sum)
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


    
    # 格式化返回数据
    if (length(p.sort.fields) > 1) {
        # 根据指定的字段顺序进行排序
        #retdata$count <- retdata$count[p.sort.fields]
        rownames(retdata$count) <- retdata$count[,1]
        retdata$count           <- retdata$count[p.sort.fields,]
        
        rownames(retdata$product) <- retdata$product[,1]
        retdata$product           <- retdata$product[p.sort.fields,]
        
        rownames(retdata$pnum) <- retdata$pnum[,1]
        retdata$pnum           <- retdata$pnum[p.sort.fields,]
        
        rownames(retdata$income) <- retdata$income[,1]
        retdata$income           <- retdata$income[p.sort.fields,]
        
        rownames(retdata$off) <- retdata$off[,1]
        retdata$off           <- retdata$off[p.sort.fields,]
    } else {
        tmp.sort.key = 1
        tmp.sort.dec = TRUE
        if (FALSE == p.sortby.field) {
            # 如果不是对字段进行排序的话，则对值进行排序
            tmp.sort.key = 2;
            tmp.sort.dec = TRUE
        } else {
            # 按字段排序,默认为升序
            tmp.sort.key = 1;
            tmp.sort.dec = FALSE
        }

        retdata$count  <- retdata$count[order(retdata$count[, tmp.sort.key], decreasing=tmp.sort.dec),]
        retdata$product<- retdata$product[order(retdata$product[, tmp.sort.key], decreasing=tmp.sort.dec),]
        retdata$pnum   <- retdata$pnum[order(retdata$pnum[, tmp.sort.key], decreasing=tmp.sort.dec),]
        retdata$income <- retdata$income[order(retdata$income[, tmp.sort.key], decreasing=tmp.sort.dec),]
        retdata$off    <- retdata$off[order(retdata$off[, tmp.sort.key], decreasing=tmp.sort.dec),]
    }

    retdata
}
