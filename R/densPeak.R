densPeak = function(X=NULL, distMat=NULL, centers, dc, method = "euclidean", dc.range = NULL, plot = TRUE)
{
    if (is.null(distMat) && is.null(X))
        stop("Invalid Input")

    if (is.null(distMat))
    {
        distMat = as.matrix(dist(X, method = method))
    }
    n = ncol(distMat)
    if (dc<=0 | min(centers)<=1 | length(centers)>n | max(centers)>n | any(duplicated(centers)))
        stop("Invalid Input")
    
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
    near_neighbour = rep(0,n)
    for (i in 1:(n-1))
    {
        ind = index[i]
        delta[ind] = min(distMat[ind,index[(i+1):n]])
        if (i+1==n)
            near_neighbour[ind] = index[n]
        else
            near_neighbour[ind] = as.numeric(names(which.min(distMat[ind,index[(i+1):n]])))
    }
    delta[index[n]] = max(distMat[index[n],])
    near_neighbour[index[n]] = index[n]
    
    rank = (delta+rho)*pmin(delta,rho)
    if (length(centers)==1)
    {
        if (centers>n)
            stop("More centers than data points")
        centers = order(rank,decreasing=TRUE)[1:centers]
    }

    cluster = rep(0,n)
    cluster[centers] = 1:length(centers)
    cluster_list = centers
    index = order(rho,decreasing=TRUE)
    
    tree = cbind(near_neighbour,1:n)
    
    allHaveLabel = FALSE
    i = 1
    
    while(!allHaveLabel)
    {
        father = cluster_list[i]
        index = which(tree[,1]==father)
        sons = tree[index,2]
        sons = sons[which(cluster[sons]==0)]
        cluster[sons] = cluster[father]
        cluster_list = c(cluster_list,sons)
        i = i+1
        if (length(setdiff(1:n,cluster_list))==0)
            allHaveLabel = TRUE
    }
    
    if (plot)
    {
        color = rep(1,n)
        color[centers] = 1+1:length(centers)
        sizes = rep(1,n)
        sizes[centers] = 3
        plot(delta,rho,xlab='Delta',ylab='Rho',pch=20,col=color,cex=sizes)
        if (ncol(X)>2)
            pairs(X,pch=20,col=cluster+1,cex=sizes)
        else
        {
            plot(X[,1],X[,2],pch=20,col=cluster+1,cex=sizes)
            plot(X[,1],X[,2],pch=20,col=cluster+1,cex=sizes,main='The path of labels')
            arrows(X[tree[,1],1],X[tree[,1],2],X[tree[,2],1],X[tree[,2],2],length=0.05)
            #for (i in 1:nrow(tree))
            #    lines(X[tree[i,],1],X[tree[i,],2])
        }
    }
    
    result = list(centers = centers,
                  cluster = cluster,
                  dc = dc,
                  rho = rho,
                  delta = delta)
    result
}
