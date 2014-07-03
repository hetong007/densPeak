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
        l = 0
        r = 1
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
    
    subDistMat = distMat[,centers]
    cluster = apply(subDistMat,1,which.min)
    distance = apply(subDistMat,1,min)
    
    border = vector(5,mode = "list")
    halo = rep(0,n)
    k = length(centers)
    for (i in 1:k)
    {
        index = which(cluster==i)
        diff_index = setdiff(1:n,index)
        tmpMat = distMat[index,diff_index]
        border[[i]] = which(rowSums(tmpMat<dc)>0)
        if (length(border[[i]])>0)
        {
            rho_h = max(rho[border[[i]]])
            halo_ind = which(rho<rho_h)[index]
            if (length(halo_ind)>0)
            {
                halo[halo_ind] = i
                cluster[halo_ind] = 0
            }
        }
    }

    if (plot)
    {
        color = rep(1,n)
        color[centers] = 1+1:length(centers)
        plot(delta,rho,xlab='Delta',ylab='Rho',pch=20,col=color)
        plot(X[,1],X[,2],pch=20,col=cluster+1)
    }
    
    result = list(centers = centers,
                  cluster = cluster,
                  halo = halo,
                  dc = dc,
                  rho = rho,
                  delta = delta)
    result
}
