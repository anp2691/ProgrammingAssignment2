## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly. 
## The funcion belows cache the inverse of Matrix 
## Below are pair of functions that stores a matrix and caches its inverse. 
## makeCacheMatrix stores a matrix X in memory 
makeCacheMatrix <- function (x = Matrices()) {
  n <- NULL
  set <- function (y){
    x <<- y
    n <<- NULL
  }
  get <- function() x 
  setinverse <- function(mean) n <<- mean
  getinverse <- function() n 
  list (set = set, get = get, 
        setinverse= setinverse, getinverse = getinverse)
}
## The function below will compute the inverse of the special "matrix" 
## Created by "makeCacheMatrix" above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x,...) { 
  ## Below will be returned a matrix that is the inveerse of 'x' 
  n <- x$getinverse() 
  if (!is.null(n)) { 
    message("getting cached data")
    return(n)
  }
  mat.data <- x$get()
  n <- solve(mat.data,...)
  x$setinverse(n)
  n
}
