
###########################

makeCacheMatrix <- function(x=matrix()){
  ### Makes a matrix cache and does the getting and setting
  
  
  inverse<-NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
  
  
}


cachesolve <- function(x, ...) {
  
  # Solves the matrix
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  mat <- x$get()
  inverse <- solve(mat, ...)
  x$setinverse(mat)
  inverse
}

