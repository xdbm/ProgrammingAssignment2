## Solution to Coursera R Programming Assignment 2 

## makeCacheMatrix: converts a matrix parameter into a list of functions 
##    suitable for data access and caching of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) 
  {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setInv <- function(inv) inv <<- inv
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve: retrieve the cached value of matrix inverse, if it is available
##    otherwise, compute and cache the matrix inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matInv <- x$getInv()
  if(!is.null(matInv))
    return(matInv)
  
  mat <- x$get()
  matInv <- solve(mat, ...)
  x$setInv(matInv)
  matInv
}
