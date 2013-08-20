
> 包括函数：lapply, sapply, vapply

## lapply
```
lapply(X, FUN, ...)
```
该函数会返回一个长度和X参数的长度相同的列表，其中每个元素都是X参数在FUN函数作用下的结果。

### 实现源码
```
function (X, FUN, ...) 
{
    FUN <- match.fun(FUN)
    if (!is.vector(X) || is.object(X))    # 如果不是向量（列表等也是向量），则会先转成list
        X <- as.list(X)
    .Internal(lapply(X, FUN))   # 直接调用C核心的函数
}
```

### 实例
```
> a
     [,1] [,2]
[1,]    1    3
[2,]    2    4

> lapply(a, function(x)x^2)  # length(a) == 4
[[1]]
[1] 1

[[2]]
[1] 4

[[3]]
[1] 9

[[4]]
[1] 16

> d
$a
[1] 1 2

$b
[1] 3 4 5

> lapply(d, sum)  # length(d) == 2
$a
[1] 3

$b
[1] 12

```

一个实际的例子：一次性加载某个目录下的R文件
```
  CaiSource <- function(x, p.path) {
    source(paste(p.path, x, sep=""))
  }
  
  # lib.path是指定的目录
  lapply(list.files(path=lib.path, pattern='\\.[rR]$'), CaiSource, lib.path)
```


## sapply
```
sapply(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE)
```
sapply是对lapply的封装，实现代码：
```
function (X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE) 
{
    FUN <- match.fun(FUN)
    answer <- lapply(X = X, FUN = FUN, ...)     # 直接调用lapply
    if (USE.NAMES && is.character(X) && is.null(names(answer))) 
        names(answer) <- X   # USE。NAMES参数
    if (!identical(simplify, FALSE) && length(answer)) 
        simplify2array(answer, higher = (simplify == "array"))  # simplify参数，默认会转换成array
    else answer
}
```

## vapply
```
vapply(X, FUN, FUN.VALUE, ..., USE.NAMES = TRUE)
```
该函数和sapply类似
```
function (X, FUN, FUN.VALUE, ..., USE.NAMES = TRUE) 
{
    FUN <- match.fun(FUN)
    if (!is.vector(X) || is.object(X))   # 对数据进行预处理
        X <- as.list(X)
    .Internal(vapply(X, FUN, FUN.VALUE, USE.NAMES))
}
```
FUN.VALUE的值定义是？

