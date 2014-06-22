##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix<- function(x = matrix()) {
    ##Given a matrix add the ability to cache it's inverse
    ##Set up the environment. 
    inv <- NULL
    ##Begin method definitions.
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv<<- inverse 
    getInverse <- function() inv
    ##list the methods we wish to export. 
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  
}


##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    inverse  <- solve(data, ...)
    x$setInverse(inverse)
    x$getInverse()
}
