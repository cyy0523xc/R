

## tapply

```
tapply(X, INDEX, FUN = NULL, ..., simplify = TRUE)
```
把变量X（通常为向量）通过函数FUN作用在INDEX这个因子变量上，返回值可以根据simplify参数设置。simplify = T（默认）返回array，simplify = F则返回list。

### 实例

计算各个省份的人均收入
```
> ins <- list(year=c(2011, 2012, 2012, 2013, 2013), province=c("GZ", "GZ", "BG", "BG", "GZ"), income=c(10, 12, 13, 12, 15))  # 定义数据
> ins
$year
[1] 2011 2012 2012 2013 2013

$province
[1] "GZ" "GZ" "BG" "BG" "GZ"

$income
[1] 10 12 13 12 15

> tapply(ins$income, ins$province, mean)
      BG       GZ 
12.50000 12.33333 
```

计算各个省份在各个年份的平均收入
```
> tapply(ins$income, list(ins$province, ins$year), mean)
   2011 2012 2013
BG   NA   13   12
GZ   10   12   15
```

### 源码
```
function (X, INDEX, FUN = NULL, ..., simplify = TRUE) 
{
    FUN <- if (!is.null(FUN)) 
        match.fun(FUN)
    if (!is.list(INDEX))           # 如果分组因子不是list，则自动转为list
        INDEX <- list(INDEX)
    nI <- length(INDEX)
    if (!nI) 
        stop("'INDEX' is of length zero")
    namelist <- vector("list", nI)
    names(namelist) <- names(INDEX)
    extent <- integer(nI) 
    nx <- length(X)
    one <- 1L
    group <- rep.int(one, nx)     # 构造一个重复向量
    ngroup <- one
    for (i in seq_along(INDEX)) {   # 对分组因子列表的下标进行循环处理
        index <- as.factor(INDEX[[i]])
        if (length(index) != nx) 
            stop("arguments must have same length")
        namelist[[i]] <- levels(index)
        extent[i] <- nlevels(index)
        
        # 注意这里计算分组的算法：计算组合后的分组情况
        group <- group + ngroup * (as.integer(index) - one)
        ngroup <- ngroup * nlevels(index)
    }
    if (is.null(FUN)) 
        return(group)
        
    # 分组数据，完成映射（FUN）
    ans <- lapply(X = split(X, group), FUN = FUN, ...)   # split按照分组因子切割变量X
    index <- as.integer(names(ans))
    if (simplify && all(unlist(lapply(ans, length)) == 1L)) {
        ansmat <- array(dim = extent, dimnames = namelist)
        ans <- unlist(ans, recursive = FALSE)
    }
    else {
        ansmat <- array(vector("list", prod(extent)), dim = extent, 
            dimnames = namelist)
    }
    if (length(index)) {
        names(ans) <- NULL
        ansmat[index] <- ans
    }
    ansmat
}
```

这个源码有可以优化的地方，例如循环体里面的：
```
        namelist[[i]] <- levels(index)
        extent[i] <- nlevels(index)
        ngroup <- ngroup * nlevels(index)
```
实际上nlevels(index)只是对levels(index)取length，所以这三行代码实质上调用了levels函数三次（在一次循环体里面），调用nlevels和length两次。可以修改成这样：
```
        namelist[[i]] <- levels(index)
        extent[i] <- length(namelist[[i]])
        ngroup <- ngroup * extent[i]
```

> 有时间的话，可以考虑把tapply改成C语言实现