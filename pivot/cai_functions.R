CaiDateFormat <- function (p.list) {
    lapply(p.list, as.Date)
}

CaiToOne <- function (p.vector, p.min = F, p.max = F) {
    # 将向量归一化
    # 参数：
    #     p.vector: 待处理向量
    #     p.min:    最小值
    #     p.max:    最大值
    
    if ( identical(F, p.min) ) {
        p.min <- min(p.vector)
    }
    if ( identical(F, p.max) ) {
        p.max <- max(p.vector)
    }

    (p.vector - p.min) / (p.max - p.min)
}
