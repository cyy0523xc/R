CaiPivotOneField <- function (p.data, p.field, p.stat.type = 'string') {
    # 对某个字段进行统计
    # 统计的类型分为两类，默认为number。
    # p.stat.type = 'number' : 数值型统计，计算平均值，方差，中位数，计算区间分布
    # p.stat.type = 'string' : 字符串型统计。频数统计，按频数倒序输出
    # p.stat.type = 'sequence' : 序号型统计。频数统计，按序号顺序输出

    # 参数判断
    tmp <- names(p.data)
    if (length(tmp[tmp == p.field]) == 0) {
        return(FALSE)
    }

    if ('number' == p.stat.type) {
        data <- CaiPivotOneFieldNumber(p.data, p.field)
    } else if ('sequence' == p.stat.type) {
        data <- CaiPivotOneFieldSequence(p.data, p.field)
    } else if ('string' == p.stat.type){
        data <- CaiPivotOneFieldString(p.data, p.field)
    } else {
        print('p.stat.type is error')
        return(FALSE)
    }

    # return data
    data
}

CaiPivotOneFieldNumber <- function(p.data, p.field) {
    #
}

CaiPivotOneFieldSequence <- function(p.data, p.field) {
    #
}

CaiPivotOneFieldString <- function(p.data, p.field) {
    #
    data <- as.factor(table(p.data[p.field]))
    sort(data, decreasing=TRUE)
}
