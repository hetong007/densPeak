Demos for densPeak
==================


```r
source('../R/densPeak.R')
```

Download data from [http://cs.joensuu.fi/sipu/datasets/](http://cs.joensuu.fi/sipu/datasets/)

## Aggregation


```r
aggregation = read.table('data/Aggregation.txt')
result = densPeak(aggregation[,1:2], centers=7, dc=1,dc.range=c(0.01,0.02))
```

```
## dc is corrected to be 1.591 and the percentage is 0.01601 .
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-21.png) ![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-22.png) ![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-23.png) 

## Flame


```r
flame = read.table('data/flame.txt')
result = densPeak(flame[,1:2], centers=2, dc=1,dc.range=c(0.01,0.02))
```

```
## dc is corrected to be 0.75 and the percentage is 0.01587 .
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-31.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-32.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-33.png) 

## Spiral


```r
spiral = read.table('data/spiral.txt')
result = densPeak(spiral[,1:2], centers=3, dc=1,dc.range=c(0.01,0.02))
```

```
## dc is corrected to be 1 and the percentage is 0.01393 .
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-41.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-42.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-43.png) 
