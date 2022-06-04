# these are functins that you will need. Copy and paste into the console
sma.df <- function(df){
  df.cor <- stats::cor(df, use="pairwise.complete.obs")
  df.var <- stats::cov(df, use="pairwise.complete.obs")
  df.sd <- sqrt(diag(df.var))
  r.rf2 <-
    (outer(diag(df.var), diag(df.var), "-")^2 ) /
    (outer(diag(df.var), diag(df.var), "+")^2 - 4 * df.var^2 )
  diag(r.rf2) <- 0
  res.dof <- nrow(df) - 2
  F <- r.rf2/(1 - r.rf2) * res.dof
  list(b=sign(df.cor) * outer(df.sd, df.sd, "/"),
       p=1 - pf(F, 1, res.dof),
       r2=df.cor^2)
}
lt.row.min <- function(X){
  result <- numeric(nrow(X) - 1)
  for(i in 2:nrow(X)){
    result[i-1] <- min(X[i, 1:i-1])
  }
  result
}
propr.phisym <- function (X)
{
  Cov <- stats::var(X)
  tmp <- 2 * Cov / outer(diag(Cov), diag(Cov), "+")
  return((1-tmp)/(1+tmp))
}
############ end functions
##