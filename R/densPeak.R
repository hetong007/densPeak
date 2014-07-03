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
    
    n = ncol(distMat)
    
    if (!is.null(dc.range))
    {
        if (dc.range[2]<dc.range[1])
            dc.range = dc.range[2:1]
        inRange = FALSE
        counter = 1
        l = 0
        r = max(distMat)
        while(!inRange)
        {
            meanRho = mean(rowSums(distMat<dc))/n
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
            if (counter>100)
                inRange = TRUE
        }
        cat('dc is corrected to be',dc,'and the percentage is',meanRho,'.\n')
    }
    
    rho = rowSums(distMat<dc)
    
    index = order(rho)
    delta = rep(0,n)
    for (i in 1:(n-1))
    {
        ind = index[i]
        delta[ind] = min(distMat[ind,index[(i+1):n]])
    }
    delta[index[n]] = max(distMat[index[n],])
    
    rank = (delta+rho)*pmin(delta,rho)
    centers = order(rank,decreasing=TRUE)[1:centers]
    
    cluster = rep(0,n)
    cluster[centers] = 1:length(centers)
    cluster_list = centers
    index = order(rho,decreasing=TRUE)
    
    #browser()
    
    for (i in index)
    {
        if (cluster[i]==0)
        {
            isStop = FALSE
            unique_list = i
            current = i
            while (!isStop)
            {
                tmpMat = distMat[unique_list,-unique_list,drop=F]
                neighbour = which(tmpMat == min(tmpMat), arr.ind = TRUE)
                neighbour = as.numeric(colnames(tmpMat)[neighbour[2]])
                if (cluster[neighbour]==0)
                {
                    current = neighbour
                    unique_list = c(unique_list,current)
                }
                else
                {
                    cluster[unique_list] = cluster[neighbour]
                    isStop = TRUE
                }
            }
        }
    }

    if (plot)
    {
        color = rep(1,n)
        color[centers] = 1+1:length(centers)
        sizes = rep(1,n)
        sizes[centers] = 3
        plot(delta,rho,xlab='Delta',ylab='Rho',pch=20,col=color,cex=sizes)
        plot(X[,1],X[,2],pch=20,col=cluster+1)
    }
    
    result = list(centers = centers,
                  cluster = cluster,
                  dc = dc,
                  rho = rho,
                  delta = delta)
    result
}
