R
=

我的R代码

## 编码规范

google：http://google-styleguide.googlecode.com/svn/trunk/Rguide.xml

中文版：http://www.road2stat.com/rstyle/rstyle.html

https://docs.google.com/document/d/1esDVxyWvH8AsX-VJa-8oqWaHLs4stGlIbk8kLc5VlII/edit

### 补充规范

#### 变量的特殊前缀

* tmp：   临时变量
* p:      函数参数

#### 变量的后缀

后缀通常用来表示变量的类型，如：lst，tb，vec, ft等

## 技巧

1. 在shell直接运行R脚本

```
#!/usr/bin/Rscript --slave
argv <- commandArgs(TRUE)
x <- as.numeric(argv[1])
```

然后：sudo chmod +x file.r




