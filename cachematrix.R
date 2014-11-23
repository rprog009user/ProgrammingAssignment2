## This functions allow to store in cache value of inverted matrix
## instead of calculate it each time. Their implementation based on 
## sample functions which cache mean value of vector.

## Put inverted matrix into cache. That cached value can be retrived
## later.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i 
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Get inverted matrix. Cache will be used if inverted matrix
## has been already calculated before.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
