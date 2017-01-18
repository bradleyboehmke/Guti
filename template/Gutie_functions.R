#This file contains all the functions necessary to perform Horn's Curve, Factor Analysis and Mahalanobis Distance.

library(MASS)

#Horn's Curve
Hornscurve <- function(N,p) {
  
  K=1000
  Eigvals_master = matrix(0,K,p)
  
  for (i in 1:K){

    M = matrix(rnorm(N*p),N,p)
    C = cov(M)
    Eigvals_C = eigen(C)$values
    tmp = sort(Eigvals_C,decreasing = TRUE)
    Eigvals_master[i,] = tmp
    next
  }
  curvepoints = colMeans(Eigvals_master)
  return(curvepoints)
}


Factor_Analysis <- function(data,HC_points) {
  data = as.matrix(data)
  N = nrow(data)
  M = ncol(data)
  R = cor(data)
  tmp = eigen(R)
  tmp2 = sort(tmp$values,decreasing = TRUE,index.return = TRUE)
  eigval = tmp2$x
  eigvec = tmp$vectors[,order(tmp2$ix)]
  
  num_factors = sum(eigval >= HC_points)
  
  xbar = colMeans(data)
  Xd = data - matrix(1,N,1)%*%t(xbar)
  v = diag(M)*diag(1/sqrt(var(data)))
  Xs = Xd %*% v
  
  eigval2 = eigval[1:num_factors]
  eigvec2 = eigvec[,1:num_factors]
  lambda_mat = matrix(0,M,num_factors)
  for (i in 1:num_factors) {
    lambda_mat[,i] = sqrt(eigval2[i]) %*% eigvec2[,i]
    next
  }
  
  fa_scores = Xs %*% ginv(R) %*% lambda_mat
  
  rotation = varimax(lambda_mat)
  B = lambda_mat %*% rotation$rotmat
  fa_scores_rotated = Xs %*% ginv(R) %*% B
  
  output = list(fa_loadings=lambda_mat,fa_scores=fa_scores,fa_loadings_rotated=B,fa_scores_rotated=fa_scores_rotated,num_factors=num_factors)
  
  return(output)
  
}

IFS2 <- function(L) {
  
  N = nrow(L)
  M = ncol(L)
  t1 = matrix(0,N,1)
  t2 = matrix(0,N,1)
  sum1 = 0
  sum2 = 0
  
  for (i in 1:N) {
    for (j in 1:M){
      t1[i,1] = t1[i,1] + L[i,j]^4
      t2[i,1] = t2[i,1] + L[i,j]^2
    }
  }
  
  for (i in 1:M){
    sum1 = sum1 + (M*t1[i,1] - t2[i,1]^2)
    sum2 = sum2 + (M -1)* (t2[i,1]^2)
  }
  
  result = sqrt(sum1/sum2)
  output = result
  
}

mahalanobis3 <- function(data) {
  
  N = nrow(data)
  M = ncol(data)
  md = as.vector(rep(0,N),mode = "numeric")
  bd = matrix(rep(0,N),nrow = N,ncol = M)
  C = cov(data)
  IC = ginv(C)
  CM = as.matrix(colMeans(data))
  
  
  for (i in 1:N){
    D = (data[i,] - t(CM))
    md[i] = D %*% IC %*% t(D)
    bd[i,] = abs(D /sqrt(diag(C)))
  }
  
  colnames(bd) = colnames(data)
  
  tmp = sort(md,decreasing = TRUE,index.return = TRUE)
  md_sort = tmp$x
  md_index = tmp$ix
  
  
  
  output = list(md = md,md_sort = md_sort, md_index = md_index,bd=bd)
  # output = list(md = md,bd=bd)
  
  return(output)
  
}

bd_row <- function(data,row) {
  
  C = cov(data)
  CM = as.matrix(colMeans(data))
  D = (data[row,] - t(CM))
  bd = D /sqrt(diag(C))
  bd_abs = abs(bd)
  tmp = sort(bd_abs,decreasing = TRUE,index.return=TRUE)
  bd_sort = tmp$x
  bd_index = tmp$ix
  
  output = list(bd = bd,bd_abs = bd_abs,bd_sort = bd_sort, bd_index = bd_index)
  
  return(output)
  
}

get_all_factors <- function(n)
{
  prime_factor_tables <- lapply(
    setNames(n, n), 
    function(i)
    {
      if(i == 1) return(data.frame(x = 1L, freq = 1L))
      plyr::count(as.integer(gmp::factorize(i)))
    }
  )
  lapply(
    prime_factor_tables, 
    function(pft)
    {
      powers <- plyr::alply(pft, 1, function(row) row$x ^ seq.int(0L, row$freq))
      power_grid <- do.call(expand.grid, powers)
      sort(unique(apply(power_grid, 1, prod)))
    }
  )
}