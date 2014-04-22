CaiSimpleRFM <- function (p.data, p.r = 0, p.f = 0, p.m = 0) {
    # RFM简单分析模型：按照R，F，M三个维度，将客户细分为8个子类
    # 参数：
    #      p.data: data.frame格式，包含4个字段：
    #                ID: 客户ID（唯一值）
    #                R： 近度
    #                F： 频度
    #                M： 额度
    #      p.r, p.f, p.m：对应的阀值，默认则由中位数代替
    # 返回：
    
    # 阀值初始化
    p.r <- ifelse(p.r == 0, median(p.data[["R"]]), p.r)
    p.f <- ifelse(p.f == 0, median(p.data[["F"]]), p.f)
    p.m <- ifelse(p.m == 0, median(p.data[["M"]]), p.m)

    # 二值化：得到新的R，F，M字段
    p.data$NewR <- ifelse(p.data$R <= p.r, 0, 1)
    p.data$NewF <- ifelse(p.data$F <= p.f, 0, 1)
    p.data$NewM <- ifelse(p.data$M <= p.m, 0, 1)

    # 把值合并到一个字段里，方便分析
    p.data$RFM <- paste(p.data$NewR, p.data$NewF, p.data$NewM, sep='')

    # return
    p.data
}

CaiRFM <- function (p.data, p.r = 2, p.f = 2, p.m = 2, p.method = 'simple') {
    # 更加通用RFM分析：可以定义每个维度上切分的客户群数量，可以指定客户群切分的方法
    # 参数：
    #    p.data: data.frame格式，包含4个字段：
    #            ID：客户ID（唯一值）
    #            R： 近度
    #            F： 频度
    #            M： 额度
    #    p.r, p.f, p.m: 每个维度上切分的客户群数量，默认为2
    #    p.method: 客户群切分的方法：
    #            simple: 简单的切分方法，按照数值进行等值切分
    #            kmeans: kmeans聚类切分
    # 返回：
    #    list()

    t.len <- length(p.data$ID)
    if (p.r >= 2) {
        p.data$NewR <- CaiDivideCumtomer(p.data$R, p.r, p.method)
    } else {
        p.data$NewR <- rep(0, t.len)
    }
    if (p.f >= 2) {
        p.data$NewF <- CaiDivideCumtomer(p.data$F, p.f, p.method)
    } else {
        p.data$NewF <- rep(0, t.len)
    }
    if (p.m >= 2) {
        p.data$NewM <- CaiDivideCumtomer(p.data$M, p.m, p.method)
    } else {
        p.data$NewM <- rep(0, t.len)
    }

    # 把值合并到一个字段里，方便分析
    p.data$RFM <- paste(p.data$NewR, p.data$NewF, p.data$NewM, sep='')

    # return
    p.data    
}

CaiDivideCumtomer <- function (p.data, p.num = 2, p.method = 'simple') {
    # 切分客户

    retdata <- c()
    if ('simple' == p.method) {
        t.max <- max(p.data)
        t.min <- min(p.data)
        t.sec <- (t.max - t.min) / p.num
        print(c(t.min, t.max, t.sec))   # test
        
        retdata <- trunc((p.data - t.min) / t.sec)
        retdata[retdata == p.num] <- p.num - 1
        print(retdata)
    } else if ('kmeans' == p.method) {
        t.data <- kmeans(p.data, p.num)
        retdata <- t.data$cluster - 1
    }

    # return
    retdata
}

CaiOrderToRFM <- function (p.order, p.name.orderid, p.name.id, p.name.r, p.name.m) {
    # 将订单数据处理成RFM的输入数据
    # 参数：
    #     p.order:    订单数据
    #     p.name.orderid:  订单ID
    #     p.name.id:  ID字段的名字
    #     p.name.r:   日期字段的名字（近度）
    #     p.name.m:   金额字段的名字（额度）
    # 返回
    #
    
    # 返回数据
    data <- list()

    data <- aggregate(p.order[p.name.r], p.order[p.name.id], max)
    tmp <- aggregate(p.order[p.name.orderid], p.order[p.name.id], length)
    data$F <- tmp[[2]]
    tmp <- aggregate(p.order[p.name.m], p.order[p.name.id], sum)
    data$M <- tmp[[2]]

    # 重命名字段名
    names(data) <- c('ID', 'R', 'F', 'M')

    # 处理近度：计算到最近的日期
    data$R <- as.integer(Sys.Date() - data$R)
    
    # return 
    data
}


