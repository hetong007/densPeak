densPeak
========

An R implementation of [Clustering by fast search and find of density peak, by Alex Rodriguez, Alessandro Laio](http://www.sciencemag.org/content/344/6191/1492.abstract).

Here's a demo on `iris`.


```r
source('R/densPeak.R')
data(iris)

result = densPeak(iris[,1:4], centers=3, dc=1,dc.range=c(0.01,0.02))
```

```
## dc is corrected to be 0.25 and the percentage is 0.01724 .
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-11.png) ![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-12.png) 

```r
table(result$cluster,iris[,5])
```

```
##    
##     setosa versicolor virginica
##   1     50         37        36
##   2      0         13         1
##   3      0          0        13
```
