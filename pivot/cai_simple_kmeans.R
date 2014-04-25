CaiSimpleKmeans <- function (p.arr, p.num, p.centers = c(), p.algorithm = mean) {
    # 一维数据的聚类算法
    # p.arr : 一维向量，且是按从小到大排序的
    # p.num : 聚类后的个数
    # p.centers : 可以由外部指定中心点
    # p.algorithm : 中心点选取算法，默认为平均值。可以设置为中位数（median），或者自定义的算法，例如kmedoids算法的中心点选取
    

    if (p.num < 2) {
        return(FALSE)
    }
    

    # 初始化中心点
    if (length(p.centers) != p.num) {
        # 按照一定的间隔选取中心点
        t.unique <- unique(p.arr)
        t.len <- trunc(length(t.unique) / p.num)
        t.index <- trunc(t.len / 2)    # 通常情况下可以避免初始中心点取集合中的最大和最小值，使得程序不容易陷入最大和最小值上的局部最优解
        if (t.index < 1) {
            t.index <- 1
        }
        
        # 计算中心点
        p.centers <- vector(length=p.num)
        for (i in 1:p.num) {
            p.centers[i] <- t.unique[t.index]
            t.index <- t.index + t.len
        }

        #for (i in p.arr) {
        #    if (!any(p.centers == i)) {
        #        # 如果这个元素不在中心点里，则加入
        #        p.centers <- c(p.centers, i)
        #        if (length(p.centers) == p.num) {
        #            break;
        #        }
        #    }
        #}
    }

    # 异常处理
    if (length(p.centers) != p.num) {
        # 当数组中的唯一值的个数比最终需要的类别数少的话
        return()
    }

    # 对中心点排序
    #t.centers <- t.centers[order(t.centers)]

    # 迭代处理数据
    print(p.centers)
    print(p.algorithm)
    retdata <- CaiCalNewCenters(p.arr, p.centers, p.algorithm)
    while (!all(p.centers == retdata$centers)) {
        p.centers <- retdata$centers
        print(retdata)
        retdata <- CaiCalNewCenters(p.arr, p.centers, p.algorithm)
    }

    retdata
}


CaiCalNewCenters <- function(p.arr, p.centers, p.algorithm) {
    # 计算新的中心点
   
    # 按照中心点将数据归类
    t.centers.index <- 1
    t.n <- length(p.centers)
    new.centers <- vector('list', length = t.n)
    for (a in p.arr) {
        if ( (t.centers.index < t.n)
             && ( abs(a - p.centers[t.centers.index]) > abs(a - p.centers[t.centers.index + 1]) ) 
             ) {
            # 到当前中心点的距离大于到下一个中心点的距离，且当前中心点又不是最后一个
            t.centers.index <- t.centers.index + 1
        }
        new.centers[[t.centers.index]] <- c(new.centers[[t.centers.index]], a)
    }

    # 计算新的中心点
    retdata <- list()
    retdata$centers.list <- new.centers
    if (!is.function(p.algorithm) && 'kmedoids' == p.algorithm) {
        # kmedoids的中心点选取算法
        retdata$centers <- vector(length=t.n)
        for (i in 1:t.n) {
            retdata$centers[i] <- CaiCalKmedoidsCenter(new.centers[[i]], p.centers[i]);
        }
    } else {
        # 其他中心点的选取算法，例如mean，median等
        retdata$centers <- sapply(new.centers, p.algorithm)
    }

    retdata
}


CaiCalKmedoidsCenter <- function(p.arr, p.center) {
    # 在p.arr 中找到新的center
    # kmedoids聚类的中心点计算

    t.len <- length(p.arr)

    # 当前的中心点的聚集度
    t.curr.sum <- sum(abs(p.arr - p.center))

    # 初始化中心点
    ret.center <- p.center

    # 计算其余每个点作为中心点的聚集度
    for (i in p.arr) {
        if (i != p.center) {
            t.sum <- sum(abs(p.arr - i))
            if (t.sum < t.curr.sum) {
                # 如果聚集度更高的化，则设置为当前的中心点
                ret.center <- i
                t.curr.sum <- t.sum
            }
        }
    }

    ret.center
}

