TVA <- function(data,block_length,num_IPs,IP_names) {

num_rows = nrow(data)
num_blocks = as.numeric(floor(num_rows / block_length))
numeric_vars = as.numeric(sum(sapply(data,is.numeric)==TRUE))
mylist = which(sapply(data,nlevels) < 100,arr.ind = TRUE)
num_levels = as.numeric(sum(sapply(data[mylist],nlevels)))
block_width = num_levels + numeric_vars + num_IPs*2
State_Vector = matrix(nrow = num_blocks, ncol = block_width)

i=1
start = 1
for (i in 1:num_blocks){
  stopp = block_length*i
  assign("temp",data[start:stopp,])
  # assign(paste("block",i,sep = ""),temp)
  temp_fac = temp %>% select_if(is.factor)
  mylist = which(sapply(temp_fac,nlevels) < 100,arr.ind = TRUE)
  vec1 = sapply(temp_fac[mylist],summary)
  if (IP_names[1] %in% names(temp) ==TRUE) vec3 = head(summary(temp[,IP_names[1]]),n=num_IPs)
  if (IP_names[2] %in% names(temp) ==TRUE) vec3 = c(vec3,head(summary(temp[,IP_names[2]]),n=num_IPs))
  temp_num = temp %>% select_if(is.numeric)
  vec2 = sapply(temp_num,sum)
  vec0 = c(unlist(vec1),unlist(vec2))
  if (exists("vec3")) vec0 = c(vec0,vec3)
  State_Vector[i,] = vec0
  start = stopp + 1
  next
}
namelist = c(unlist(sapply(temp_fac[mylist],levels)),names(temp_num),paste("S",1:num_IPs,sep=""),paste("D",1:num_IPs,sep=""))
colnames(State_Vector) = namelist
return(State_Vector)

}

PCA <- function(data) {
  data = as.matrix(data)
  N = nrow(data)
  M = ncol(data)
  R = cor(data)
  tmp = eigen(R)
  tmp2 = sort(tmp$values,decreasing = TRUE,index.return = TRUE)
  eigval = tmp2$x
  eigvec = tmp$vectors[,order(tmp2$ix)]
  
  #loadings matrix
  pca_loadings = matrix(0,M,M)
  for (i in 1:M) {
    pca_loadings[,i] = sqrt(eigval[i]) %*% eigvec[,i] 
    next
  }
  
  #component scores
  xbar = colMeans(data)
  Xd = data - matrix(1,N,1)%*%t(xbar)
  v = diag(M)*diag(1/sqrt(var(data)))
  Xs = Xd %*% v
  Y = Xs %*% eigvec
  pca_scores = Y
  
  output = list(eigval=eigval,eigvec=eigvec,pca_loadings=pca_loadings,pca_scores=pca_scores)
  
  return(output)
  
}

Factor_Analysis <- function(data,num_factors) {
  data = as.matrix(data)
  N = nrow(data)
  M = ncol(data)
  R = cor(data)
  tmp = eigen(R)
  tmp2 = sort(tmp$values,decreasing = TRUE,index.return = TRUE)
  eigval = tmp2$x
  eigvec = tmp$vectors[,order(tmp2$ix)]
  
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
  
  #generalized inverse is necessary to avoid matrix close to singularity
  fa_scores = Xs %*% ginv(R) %*% lambda_mat
  
  rotation = varimax(lambda_mat)
  B = lambda_mat %*% rotation$rotmat
  fa_scores_rotated = Xs %*% ginv(R) %*% B
  
  output = list(fa_loadings=lambda_mat,fa_scores=fa_scores,fa_loadings_rotated=B,fa_scores_rotated=fa_scores_rotated,num_factors=num_factors)
  
  return(output)
  
}

MC_Adjust <- function(data,delta1,delta2){
  col2rmv = which(colVars(data) < delta1)
  newdata = subset(data,select = -col2rmv)
  col2rmv = caret::findLinearCombos(newdata)$remove
  newdata = subset(newdata,select = -col2rmv)
  
  C = cor(newdata)
  samp = data.frame(which(apply(abs(C),MARGIN = 2,function(x) between(x,(1-delta2),1.0)),arr.ind = TRUE))
  mylist = data.frame(matrix(nrow = nrow(C),ncol=2))
  colnames(mylist) = c("num","name")
  mylist[,1] = 1:ncol(C)
  mylist[,2] = colnames(newdata)
  temp = lookup(samp,mylist)
  col2rmv = which(colnames(newdata) %in% temp[duplicated(temp)])
  newdata = subset(newdata,select = -col2rmv)
  return(newdata)
}