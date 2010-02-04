################################################################################
##
##  Hierarchical Vector Quantization - Version 1.05
##
##  Perform hierarchical vector quantization on a data matrix. For all members 
##  within the cluster, an average absolute percent error distance is calculated 
##  between the cluster centroid and the members.
##
##  Usage:
##                 hvq(x, nclust = 3, depth = 3, quant.err = 10 , lite = T,
##                 algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen")) 
##                 
##
##
##  Arguments:
## 
##    x             matrix of multivariate data. Each row corresponds to an 
##                  observation, and each column corresponds to a variable. 
##                  Missing values are not accepted.
##
##    nclust        number of nodes per hierarchy
##
##    depth         a.Hierarchy Depth – the depth of the tree (1=no hierarchy, 
##                  2 = 2 levels, etc..)
##
##    quant.err     quantization error
##
##    lite          logical indicating the lite version. Default is True.
##
##    algorithm     Character string. The algorithm of Hartigan and Wong is used 
##                  by default and other options are "Lloyd", "Forgy", "MacQueen".
##    
##
##
##  Output:
##
##    clusters      list showing each ID assigned to a cluster
##
##    nodes.clust   list corresponding to nodes' details
##
##    idnodes       ID and Segments. Similar to nodes.clust with additional columns 
##                  for nodes ID.
##
##    error.quant   list of quantization error for all levels and nodes
##
##
##    plt.clust     list of logical values indicating if the quantization error
##                  was met
##
##    ztab          output table with summary
##
##
#################################################################################

hvq <- function (x, nclust = 3, depth = 3, quant.err = 10 , lite = T,
algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen")) 
{

#
#  The lite version is triggered by default. The user can run hvqlite directly
#  To compare hvq and hvqlite, set the seed!
#
#   

    if(lite){ hvqlite( x=x,nc=nclust, de=depth, quan=quant.err, algo=algorithm)}
else{
             
    rescl <- list()
    resid <- list()
    resm <- list()
    resplt <- list()
    ztab3up <- list()
    ztab1 <- ztab2 <- ztabn <- NULL
    ztab11 <- ztab12 <- ztab13 <- NULL
    zdepth <- depth
    quantinit <- rep(F, nclust)
    outkinit <- IHGkout(x, kout = kmeans(x, nclust, iter.max=100, algo=algorithm), nclust)
    rescl[[1]] <- outkinit$val
    tet <- lapply(outkinit$val, row.names)
    for (k in 1:length(tet)) tet[[k]] <- data.frame(tet[[k]], 
        1, 1, k)
    resid[[1]] <- tet
    resm[[1]] <- outkinit$cent
    resplt[[1]] <- unlist(outkinit$cent) > quant.err
    initclust <- outkinit$values
    if (depth > 1) {
        i <- 1
        while (i < depth) {
            ijclust <- NULL
            ijrescl <- list()
            ijresid <- list()
            ijresm <- list()
            ijresplt <- list()
            ijresnsize <- list()
            ijztab3up <- list()
            quantok <- unlist(resplt[[i]])
            j <- 1
            while (j < (nclust^i) + 1) {
                if (quantok[j] & NROW(initclust[[j]]) > nclust) {
                  outk <- IHGkout(initclust[[j]], kout = kmeans(initclust[[j]], 
                    nclust,iter.max=100, algo=algorithm), nclust)
                  ijrescl[[j]] <- outk$val
                  tet <- lapply(outk$val, row.names)
                  for (k in 1:length(tet)) tet[[k]] <- data.frame(tet[[k]], 
                    i + 1, j, k)
                  ijresid[[j]] <- tet
                  ijresm[[j]] <- outk$cent
                  ijresnsize[[j]] <- outk$nsize
                  ijztab3up[[j]] <- sapply(outk$val, mean)
                  ijresplt[[j]] <- unlist(outk$cent) > quant.err
                  ijclust <- c(ijclust, outk$values)
                }
                else {
                  ijrescl[[j]] <- rep(NA, nclust)
                  ijresid[[j]] <- NULL
                  ijresm[[j]] <- rep(NA, nclust)
                  ijresnsize[[j]] <- rep(0, nclust)
                  ijztab3up[[j]] <- matrix(NA, ncol(x), nclust)
                  ijresplt[[j]] <- rep(F, nclust)
                  ijclust <- c(ijclust, rep(NA, nclust))
                }
                ztab1 <- c(ztab1, paste(i + 1, j, 1:nclust, sep = ","))
                ztab11 <- c(ztab11, rep(i + 1, nclust))
                ztab12 <- c(ztab12, rep(j, nclust))
                ztab13 <- c(ztab13, 1:nclust)
                j <- j + 1
            }
            rescl[[i + 1]] <- ijrescl
            resid[[i + 1]] <- ijresid
            resm[[i + 1]] <- ijresm
            resplt[[i + 1]] <- ijresplt
            ztab2 <- c(ztab2, unlist(ijresnsize))
            ztab3up[[i + 1]] <- data.frame(ijztab3up)
            ztabn <- c(ztabn, unlist(ijresm))
            initclust <- ijclust
            i <- i + 1
            if (!is.element(T, unlist(ijresplt))) {
                zdepth <- i
                i <- depth
            }
        }
        ztab <- data.frame(matrix(0, nrow = sum(nclust^(1:zdepth)), 
            ncol = (ncol(x) + 5)))
        ztab[1:nclust, 1] <- rep(1, nclust)
        ztab[1:nclust, 2] <- rep(1, nclust)
        ztab[1:nclust, 3] <- 1:nclust
        ztab[1:nclust, 4] <- unlist(outkinit$nsize)
        ztab[1:nclust, 5] <- unlist(outkinit$cent)
        ztab3upc <- sapply(outkinit$val, mean, na.rm = T)
        for (l in 1:length(ztab3up)) ztab3upc <- cbind(ztab3upc, 
            ztab3up[[l]])
        ztab[(nclust + 1):sum(nclust^(1:zdepth)), 1] <- ztab11
        ztab[(nclust + 1):sum(nclust^(1:zdepth)), 2] <- ztab12
        ztab[(nclust + 1):sum(nclust^(1:zdepth)), 3] <- ztab13
        ztab[(nclust + 1):sum(nclust^(1:zdepth)), 4] <- ztab2
        ztab[, 6:ncol(ztab)] <- t(ztab3upc)
        ztab[(nclust + 1):sum(nclust^(1:zdepth)), 5] <- ztabn
        names(ztab) <- c("Segment Level", "Segment Parent", "Segment Child", 
            "n", "Quant Error", names(x))
    }
    else {
        ztab <- data.frame(matrix(0, nrow = nclust, ncol = (ncol(x) + 
            5)))
        ztab[, 1] <- rep(1, nclust)
        ztab[, 2] <- rep(1, nclust)
        ztab[, 3] <- 1:nclust
        ztab[, 4] <- unlist(outkinit$nsize)
        ztab[, 6:ncol(ztab)] <- t(sapply(outkinit$val, mean, 
            na.rm = T))
        ztab[, 5] <- unlist(outkinit$cent)
        names(ztab) <- c("Segment Level", "Segment Parent", "Segment Child", 
            "n", "Quant Error", names(x))
    }
    return(list(clusters = initclust, nodes.clust = rescl, idnodes = resid, 
        error.quant = resm, plt.clust = resplt, ztab = ztab))
}

}

IHGkout <- function (x,kout, nclust){

	outl <- list()
	nout <- list()
	centl <- list()
	for(i in 1:nclust) {
		outl[[i]] <- x[kout$cluster == i,  ]
		nout[[i]] <- kout$size[i]	

#
#  Generating lists of outputs
#
		if(nrow(outl[[i]]) > 1) {
			icent <- apply(outl[[i]], 2, mean)
			centl[[i]] <- mean(apply(outl[[i]], 1, function(x, y)
			mean(abs(x - y), na.rm = T), icent), na.rm = T)
		}
		else {
			centl[[i]] <- 0
			icent <- outl[[i]]
		}
	}

	return(list(centers = centl, values = outl, nsize = nout))
}



hvqlite <- function (x, nclust = 3, depth = 3,quant.err = 10 ,algorithm = 
                 c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen")) 
{
    #rescl <- list()
    #resid <- list()
    resm <- list()
    resplt <- list()
    ztab3up <- list()
    ztab1 <- ztab2 <- ztabn <- NULL
    ztab11 <- ztab12 <- ztab13 <- NULL
    zdepth <- depth
    quantinit <- rep(F, nclust)
    outkinit <- IHGkout(x, kout = kmeans(x, nclust,iter.max=100, algo=algorithm), nclust)
    #rescl[[1]] <- outkinit$val
    tet <- lapply(outkinit$val, row.names)
    #for (k in 1:length(tet)) tet[[k]] <- data.frame(tet[[k]], 
    #    1, 1, k)
    idseg <- NULL
    for (k in 1:length(tet)) idseg <- rbind(idseg, data.frame(ID=tet[[k]], 
        Level=1, Parent=1, Child=k))
    #resid[[1]] <- tet
     
    resm[[1]] <- outkinit$cent
    resplt[[1]] <- unlist(outkinit$cent) > quant.err
    initclust <- outkinit$values
    if (depth > 1) {
        i <- 1
        while (i < depth) {
            ijclust <- NULL
            #ijrescl <- list()
            #ijresid <- list()
            ijresm <- list()
            ijresplt <- list()
            ijresnsize <- list()
            ijztab3up <- list()
            quantok <- unlist(resplt[[i]])
            j <- 1
            while (j < (nclust^i) + 1) {
                if (quantok[j] & NROW(initclust[[j]]) > nclust) {
                  outk <- IHGkout(initclust[[j]], kout = kmeans(initclust[[j]], 
                    nclust,iter.max=100, algo=algorithm), nclust)
                  #ijrescl[[j]] <- outk$val
                  tet <- lapply(outk$val, row.names)

                  for (k in 1:length(tet)) idseg <- rbind(idseg, data.frame(ID=tet[[k]], 
        Level=i + 1, Parent=j, Child=k))


#for (k in 1:length(tet)) tet[[k]] <- data.frame(tet[[k]], 
  #                  i + 1, j, k)


                  #ijresid[[j]] <- tet
                  ijresm[[j]] <- outk$cent
                  ijresnsize[[j]] <- outk$nsize
                  ijztab3up[[j]] <- sapply(outk$val, mean)
                  ijresplt[[j]] <- unlist(outk$cent) > quant.err
                  ijclust <- c(ijclust, outk$values)
                }
                else {
                  #ijrescl[[j]] <- rep(NA, nclust)
                  #ijresid[[j]] <- NULL
                  ijresm[[j]] <- rep(NA, nclust)
                  ijresnsize[[j]] <- rep(0, nclust)
                  ijztab3up[[j]] <- matrix(NA, ncol(x), nclust)
                  ijresplt[[j]] <- rep(F, nclust)
                  ijclust <- c(ijclust, rep(NA, nclust))
                }
                ztab1 <- c(ztab1, paste(i + 1, j, 1:nclust, sep = ","))
                ztab11 <- c(ztab11, rep(i + 1, nclust))
                ztab12 <- c(ztab12, rep(j, nclust))
                ztab13 <- c(ztab13, 1:nclust)
                j <- j + 1
            }
            #rescl[[i + 1]] <- ijrescl
            #resid[[i + 1]] <- ijresid
            resm[[i + 1]] <- ijresm
            resplt[[i + 1]] <- ijresplt
            ztab2 <- c(ztab2, unlist(ijresnsize))
            ztab3up[[i + 1]] <- data.frame(ijztab3up)
            ztabn <- c(ztabn, unlist(ijresm))
            initclust <- ijclust
            i <- i + 1
            if (!is.element(T, unlist(ijresplt))) {
                zdepth <- i
                i <- depth
            }
        }
        ztab <- data.frame(matrix(0, nrow = sum(nclust^(1:zdepth)), 
            ncol = (ncol(x) + 5)))
        ztab[1:nclust, 1] <- rep(1, nclust)
        ztab[1:nclust, 2] <- rep(1, nclust)
        ztab[1:nclust, 3] <- 1:nclust
        ztab[1:nclust, 4] <- unlist(outkinit$nsize)
        ztab[1:nclust, 5] <- unlist(outkinit$cent)
        ztab3upc <- sapply(outkinit$val, mean, na.rm = T)
        for (l in 1:length(ztab3up)) ztab3upc <- cbind(ztab3upc, 
            ztab3up[[l]])
        ztab[(nclust + 1):sum(nclust^(1:zdepth)), 1] <- ztab11
        ztab[(nclust + 1):sum(nclust^(1:zdepth)), 2] <- ztab12
        ztab[(nclust + 1):sum(nclust^(1:zdepth)), 3] <- ztab13
        ztab[(nclust + 1):sum(nclust^(1:zdepth)), 4] <- ztab2
        ztab[, 6:ncol(ztab)] <- t(ztab3upc)
        ztab[(nclust + 1):sum(nclust^(1:zdepth)), 5] <- ztabn
        names(ztab) <- c("Segment Level", "Segment Parent", "Segment Child", 
            "n", "Quant Error", names(x))
    }
    else {
        ztab <- data.frame(matrix(0, nrow = nclust, ncol = (ncol(x) + 
            5)))
        ztab[, 1] <- rep(1, nclust)
        ztab[, 2] <- rep(1, nclust)
        ztab[, 3] <- 1:nclust
        ztab[, 4] <- unlist(outkinit$nsize)
        ztab[, 6:ncol(ztab)] <- t(sapply(outkinit$val, mean, 
            na.rm = T))
        ztab[, 5] <- unlist(outkinit$cent)
        names(ztab) <- c("Segment Level", "Segment Parent", "Segment Child", 
            "n", "Quant Error", names(x))
    }
    return(list(clusters = initclust,   
        error.quant = resm, idnodes= idseg, plt.clust = resplt, ztab = ztab))

# return(list(clusters = initclust, nodes.clust = rescl, idnodes = resid, 
#       error.quant = resm, plt.clust = resplt, ztab = ztab))





}
