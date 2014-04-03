CaiLoadData <- function(p.filename, p.filetype='csv') {
    # 根据文件名加载数据，默认为csv格式
    # csv格式：首行会作为字段名

    if (p.filetype == 'csv') {
        data <- read.csv(p.filename)
    }
    
    tmp <- dim(data)
    print(paste('数据加载成功。字段数：', tmp[2], '记录数：', tmp[1], '字段名：'))
    print(names(data))

    # return
    data
}
