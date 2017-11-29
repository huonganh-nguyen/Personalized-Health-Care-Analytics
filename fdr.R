##
## extract p-value cutoff for E[FDP] < q
## 
## also plot only p-values that are non-zero
fdr_cut <- function(pvals, q){
  pvals <- pvals[!is.na(pvals)]
  n <- length(pvals)
  
  j <- rank(pvals, ties.method="min")
  sig <- pvals <= q*j/n
  sig[pvals<max(pvals[sig])] <- TRUE
  
  
  ind <- which( pvals > 0 )
  o <- order(pvals)
  oo <- o[o %in% ind]
  
  par(mar=c(1.5,1.5,1.5,1.5))
  par(mai=c(1.5,1.5,1.5,1.5))
  plot( (n-length(oo)+1):n, pvals[oo], log="xy", col=c("grey60","red")[sig[o]+1], pch=20, 
       ylab="p-values", xlim = c(1,n), xlab="Tests ordered by p-value", main = paste('FDR with q =',q))
  lines(1:n, q*(1:n)/n)
  
  return(max(pvals[sig]))
}

