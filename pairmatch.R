#Ville's code to find best matching pairs
pairmatch <- function(x, y) {
  mx <- nrow(x)
  my <- nrow(y)
  
  # Prepare edge list.
  edges <- matrix(NA, nrow=mx*my, ncol=3)
  
  # Calculate Euclidian distances.
  nelem <- 0
  for(i in 1:mx) {
    xi <- x[i, ]
    for(j in 1:my) {
      nelem <- (nelem + 1)
      d <- sum((y[j, ] - xi)^2, na.rm=TRUE)
      edges[nelem,] <- c(i, j, d)
    }
  }

  # Sort by distance.
  sorted <- order(edges[,3])
  edges <- edges[sorted,]

  # Pick a distinct batch of pairings.
  pairs <- matrix()
  isfirst <- TRUE
  while(TRUE) {
    bitmap <- pairmatch_pick(edges, mx, my)
    excl <- as.integer(which(!bitmap))
    incl <- as.integer(which(bitmap))
    if(isfirst) {
      pairs <- edges[incl,]
      isfirst = FALSE
    }
    else {
      pairs <- rbind(pairs, edges[incl,])
    }
    if(length(excl) == 0) break
    if(length(excl) == 1) {
      pairs <- rbind(pairs, edges)
      break
    }
    edges <- as.matrix(edges[excl,])
  }

plot(pairs[,3])
return (pairs)
}

#----------------------------------------------------------------------------

pairmatch_pick <- function(edges, mx, my) {
  nedges <- nrow(edges)  
  xbits <- rep(FALSE, length.out=mx)
  ybits <- rep(FALSE, length.out=my)
  flags <- rep(FALSE, length.out=nedges)  
  for(k in 1:nedges) {
    i <- edges[k, 1]
    j <- edges[k, 2]
    if(xbits[i]) next
    if(ybits[j]) next
    xbits[i] <- TRUE
    ybits[j] <- TRUE
    flags[k] <- TRUE 
  }
  return(flags)
}
