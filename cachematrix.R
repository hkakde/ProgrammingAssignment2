## This function  
#Set a matrix
#Get a matrix
#Computes the matrix inverse
#Gets the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
  myinvmatrix <- matrix(data = NA, nrow(x),ncol(x))
 
  setmatrix <- function (y=matrix()){
    x <<- y
    myinvmatrix <- matrix(data = NA, nrow(x),ncol(x))
  }
  
  getmatrix <- function() x
  
  setmatrixinv <- function(invmatrix) myinvmatrix <<- invmatrix
  
  getmatrixinv <- function() myinvmatrix
 
  list(setmatrix = setmatrix, getmatrix = getmatrix, setmatrixinv = setmatrixinv, getmatrixinv=getmatrixinv)
}


## Returns the inverse of the matrix from cache if available, otherwise generates the inverse matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  myinvmatrix <- x$getmatrixinv()
  #print(myinmatrix)
  
  if(!all(is.na(myinvmatrix))){
    message("Getting matrix inv from cache")
    return(myinvmatrix)
  }
  
  mymatrix <- x$getmatrix()
  #print(mydata)
  myinvmatrix <- solve(mymatrix, ...)
  x$setmatrixinv(myinvmatrix)
  #print(myinmatrix)
  myinvmatrix

}
