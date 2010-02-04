###################################################################################
##
##   HVQGRAPH Graphs ZTAB from the output of HVQ. In this function, only the first 
##   hierarchy is plotted and assume the argument depth = 1 when running hvq.
##
##   
##   ztab            hvq ztab output 
##
##   zcols           ztab columns to be used: eg   c(4,5,6,9), 7:10 
##
##   heatdat         Additional data frame for heat maps.
##
##   palette.color   Numeric from 1 through 6 giving the color palette. 1 for rainbow
##                   2 for heat.colors, 3 for terrain.colors, 4 for topo.colors, 5 for 
##                   cm.colors, 6 for seas color palettes. The default is rainbow.
##
##   labtess         character vector for labelling tesselation. 
##
##   labsize         Numerical value giving the amount by which tesselation labels
##                   should be scaled relative to the default. Default is .5.
##
##   bwtess          logical indicating color of the plot. Default generate black & White.
##
##   numrec          logical indicating if the number of records in the cluster should 
##                   be assigned to the centerpoint.
##
##   magnif          Numerical vector to magnify ploting symbol indicating cluster size.  
##
##   axes            logical if you want axes around the plots
##
##   asp             Numeric defining aspect ratio type. Default is 1. 
##                   For flexible aspect ratio set asp=NA
##
##   ask             logical. If TRUE (and the R session is interactive) the user 
##                   is asked for input, before a new figure is drawn.           
##
##   lim.del.x       Outlier tresholds vector c(1,2) for voronoi plot, default is NULL
##   lim.del.y  
##
##   deldir          required packages
##   gtools 
##   seas
##   MASS  
##
###################################################################################


hvqgraph33 <- function (hvqztab, zcols = 7:10, labtess = NULL, heatdat = NULL, 
    bwtess = T, palette.color = 1, numrec = F, magnif=NULL, lim.del.x = NULL, 
    lim.del.y = NULL, axes = T, asp = 1, ask=T, labsize=.5) 
{
    require(MASS)
    require(deldir)
    require(gtools)
    require(seas)
    gdata <- hvqztab[, eval(zcols), drop = F]
    gtitles <- names(gdata)
    pal.col <- c("rainbow(n, start=.7, end=.1)", "heat.colors(n)", 
        "terrain.colors(n)", "topo.colors(n)", "cm.colors(n)", 
        "colorRampPalette(c(crp1,crp2))(500)")
    gsam <- sammon(dist(unique(gdata)), tr = F)
    rawdeldat <- cbind(gsam$points[, 1], gsam$points[, 2])
    if (!is.null(lim.del.x)) {
        rawdeldat <- rawdeldat[(rawdeldat[, 1] > lim.del.x[1]) & 
            (rawdeldat[, 1] < lim.del.x[2]), ]
    }
    if (!is.null(lim.del.y)) {
        rawdeldat <- rawdeldat[(rawdeldat[, 2] > lim.del.y[1]) & 
            (rawdeldat[, 2] < lim.del.y[2]), ]
    }
    lim1 <- min(rawdeldat[, 1], rawdeldat[, 2]) * 0.9
    lim2 <- max(rawdeldat[, 1], rawdeldat[, 2]) * 1.1
    deldat <- deldir(rawdeldat[, 1], rawdeldat[, 2])
    gleg <- 1:nrow(hvqztab)
    crp1 <- "white"
    crp2 <- "blue"
    if (bwtess) {
        plot(deldat, wlines = "tess")
        title("Default Tesselation")
    }
    else {
        for (i in 1:length(gtitles)) {
            close.screen(all = T)
            pdat <- tile.list(deldat)
            n <- 500
            colvec <- eval(parse(text = pal.col[palette.color]))
            mymat <- rbind(c(0, 1, 0.05, 1), c(0.3, .8, 0, 0.1))
            split.screen(mymat)
            screen(1)


xrange <- range (gdata[,i])
mfac <- 499/(xrange[2] - xrange[1])
xcolors <- 1 + (gdata[,i] - xrange[1]) * mfac
xcolind <- findInterval(xcolors, 1:500)


colvec500 <- colvec[1:500]
par(ask=ask)
plot.tile.hvq(pdat, ptext = labtess, polycol = colvec500[xcolind], 
                close = T, showpoints = numrec, axes = axes, 
                frame.plot = F, xlab = "", ylab = "", vectrec = magnif 
                  , asp = asp, labsize=labsize)
            title(gtitles[i])
            colind <- format(seq(min(gdata[, i]), max(gdata[, 
                i]), length = 10), digit = 1)
            zusr <- par("usr") * 0.9
            zseq <- seq(zusr[1], zusr[2], length = length(colvec) + 
                1)
            screen(2)
            zseq <- seq(0, 1, length = length(colvec) + 1)
            rect(zseq[-length(zseq)], 0.5, zseq[-1], 1, col = colvec, 
                border = NA)
            tseq1 <- seq(0, 1, length = 10)
            text(tseq1, 0.45, colind, cex = 0.5, adj = 0.5)
        }
    }
    if (!is.null(heatdat)) {
        htitles <- names(heatdat)
        for (i in 1:length(htitles)) {
close.screen(all = T)
            pdat <- tile.list(deldat)
            n <- 500
            colvec <- eval(parse(text = pal.col[palette.color]))
           mymat <- rbind(c(0, 1, 0.05, 1), c(0.3, .8, 0, 0.1))
            split.screen(mymat)
            screen(1)


xrange <- range (heatdat[,i])
mfac <- 499/(xrange[2] - xrange[1])
xcolors <- 1 + (heatdat[,i] - xrange[1]) * mfac
xcolind <- findInterval(xcolors, 1:500)


colvec500 <- colvec[1:500]
par(ask=ask)
plot.tile.hvq(pdat, ptext = labtess, polycol = colvec500[xcolind], 
                close = T, showpoints = numrec, axes = axes, 
                frame.plot = F, xlab = "", ylab = "", vectrec = magnif 
                  , asp = asp, labsize=labsize)
            title(htitles[i])
            colind <- format(seq(min(heatdat[, i]), max(heatdat[, 
                i]), length = 10), digit = 1)
            zusr <- par("usr") * 0.9
            zseq <- seq(zusr[1], zusr[2], length = length(colvec) + 
                1)
            screen(2)
            zseq <- seq(0, 1, length = length(colvec) + 1)
            rect(zseq[-length(zseq)], 0.5, zseq[-1], 1, col = colvec, 
                border = NA)
            tseq1 <- seq(0, 1, length = 10)
            text(tseq1, 0.45, colind, cex = 0.5, adj = 0.5)

           



        }
    }
}




plot.tile.hvq <- function (x, ptext=NULL, verbose = FALSE, close = FALSE, pch = 1, polycol = NA, 
    showpoints = TRUE, asp = 1, vectrec=1, labsize=.5, ...) 
{
    object <- x
    if (!inherits(object, "tile.list")) 
        stop("Argument \"object\" is not of class tile.list.\n")
    n <- length(object)
    x.all <- unlist(lapply(object, function(w) {
        c(w$pt[1], w$x)
    }))
    y.all <- unlist(lapply(object, function(w) {
        c(w$pt[2], w$y)
    }))
    x.pts <- unlist(lapply(object, function(w) {
        w$pt[1]
    }))
    y.pts <- unlist(lapply(object, function(w) {
        w$pt[2]
    }))
    rx <- range(x.all)
    ry <- range(y.all)
    plot(x.all, y.all, type = "n", asp = asp,...)
    polycol <- apply(col2rgb(polycol, TRUE), 2, function(x) {
        do.call(rgb, as.list(x/255))
    })
    polycol <- rep(polycol, length = length(object))
    hexbla <- do.call(rgb, as.list(col2rgb("black", TRUE)/255))
    hexwhi <- do.call(rgb, as.list(col2rgb("white", TRUE)/255))
    ptcol <- ifelse(polycol == hexbla, hexwhi, hexbla)
    lnwid <- ifelse(polycol == hexbla, 2, 1)
    for (i in 1:n) {
        inner <- !any(object[[i]]$bp)
        if (close | inner) 
            polygon(object[[i]], col = polycol[i], border = ptcol[i], 
                lwd = lnwid[i])
        else {
            x <- object[[i]]$x
            y <- object[[i]]$y
            bp <- object[[i]]$bp
            ni <- length(x)
            for (j in 1:ni) {
                jnext <- if (j < ni) 
                  j + 1
                else 1
                do.it <- mid.in(x[c(j, jnext)], y[c(j, jnext)], 
                  rx, ry)
                if (do.it) 
                  segments(x[j], y[j], x[jnext], y[jnext], col = ptcol[i], 
                    lwd = lnwid[i])
            }
        }
        if (verbose & showpoints) 
            points(object[[i]]$pt[1], object[[i]]$pt[2], pch = pch, 
                col = ptcol[i])
        if (verbose & i < n) 
            readline("Go? ")
    }


if (showpoints) {

pmag <- rep(1,n)

if( !is.null(vectrec)){
pmag <- 2 * vectrec/max(vectrec)}


        for (j in 1:n) points(x.pts[j], y.pts[j], pch = 16, cex = pmag[j], col=ptcol[j])
        if (!is.null(ptext)) {
            text(x.pts, y.pts, ptext[1:n], cex = labsize, adj = 1)
        }
    }

  #  if (showpoints) 
   #     points(x.pts, y.pts, pch = pch, col = ptcol)



    invisible()
}

























