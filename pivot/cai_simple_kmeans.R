CaiSimpleKmeans <- function (p.arr, p.num) {
    # 一维数据的聚类算法
    # p.arr : 一维向量
    # p.num : 聚类后的个数

    if (p.num < 2) {
        return(FALSE)
    }
    
    # return
    retdata <- list()

    # 初始化centers
    t.len <- length(p.arr)
    t.min <- min(p.arr)
    t.max <- max(p.arr)
    t.sec <- (t.max - t.min) / p.num
    retdata$centers <- c(t.min + t.sec / 2)   # 第一个
    if (p.num > 2) {
        # 加入中心点
        for (i in 1:(p.num - 2)) {
            retdata$centers <- c(retdata$centers, t.min + t.sec / 2 + i * t.sec)
        }
    }
    retdata$centers <- c(retdata$centers, t.max - t.sec / 2)

    # 迭代聚类
    new.centers <- CaiCalNewCenters(p.arr, retdata$centers)
    while (new.centers != retdata$centers) {
        retdata$centers <- new.centers
        new.centers <- CaiCalNewCenters(p.arr, retdata$centers)
    }
}

CaiCalNewCenters <- function(p.arr, p.centers) {
    # 计算新的中心点
}
