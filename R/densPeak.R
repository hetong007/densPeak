densPeak = function(X=NULL, distMat=NULL, centers, dc, method = "euclidean", dc.range = NULL, plot = TRUE)
{
    if (is.null(distMat) && is.null(X))
        stop("Invalid Input")
    if (dc<=0 || centers<=1)
        stop("Invalid Input")
    if (is.null(distMat))
    {
        distMat = as.matrix(dist(X, method = method))
    }
    
    if (!is.null(dc.range))
    {
        if (dc.range[2]<dc.range[1])
            dc.range = dc.range[2:1]
        inRange = FALSE
        l = 0
        r = 1
        while(!inRange)
        {
            meanRho = mean(rowSums(distMat<dc))
            if (meanRho>dc.range[2])
            {
                r = dc
                dc = (l+dc)/2
            }
            else if (meanRho<dc.range[1])
            {
                l = dc
                dc = (dc+r)/2
            }
            else
            {
                inRange = TRUE
            }
        }
        cat('dc is corrected to be',dc,'and the percentage is',meanRho,'.\n')
    }
    
    rho = rowSums(distMat<dc)
    
    n = ncol(dist)
    index = order(rho)
    delta = rep(0,n)
    for (i in 1:(n-1))
    {
        ind = index[i]
        delta[ind] = min(dist[ind,index[(i+1):n]])
    }
    delta[index[n]] = max(dist[index[n],])
    
    if (plot)
    {
        plot(delta,rho,xlab='Delta',ylab='Rho')
    }
    
    
}
