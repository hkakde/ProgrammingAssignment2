## This function  
#Set a matrix
#Get a matrix
#Computes the matrix inverse
#Gets the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  myinmatrix <- matrix(x[NULL], nrow(x),ncol(x))
  setmatrix <-function (y=matrix()){
    x<<-y
    myinmatrix <- matrix(x[NULL], nrow(x),ncol(x))
  }
  getmatrix <- function()x
  setmatrixinv <- function(solve) myinmatrix <<- solve(x)
  getmatrixinv <- function() myinmatrix
  list(setmatrix=setmatrix, getmatrix = getmatrix, setmatrixinv = setmatrixinv, getmatrixinv=getmatrixinv)
}


## Returns the inverse of the matrix from cache if available, otherwise generates the inverse matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  myinmatrix<-x$getmatrixinv()
  #print(myinmatrix)
  if(!all(is.na(myinmatrix))){
    message("Getting matrix inv from cache")
    return(myinmatrix)
  }
  mydata <-x$getmatrix()
  #print(mydata)
  myinmatrix <- solve(mydata, ...)
  x$setmatrixinv(myinmatrix)
  #print(myinmatrix)
  myinmatrix
}
