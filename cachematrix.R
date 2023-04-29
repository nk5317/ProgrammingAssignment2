## Put comments here that give an overall description of what your
## Two functions that cache the inverse of matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
                  t <- NULL
                set <- function(y) {
                  x <<- y 
                  t <<- NULL
                }
                get <- function() x
         setInverse <- function(inverse) t <<- inverse
         getInverse <- function() t
         list(set = set, get = get, 
              setInverse = setInverse, 
              getInverse = getInverse)
}


## This fx computes the inverse of the special "matrix" returned by above fx.
## If the inverse has already been calculated (with unchanged matrix), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        t <- x$getInverse()
        if(!is.null(t)) {
          message("getting cached data")
          return(t)
        }
        m <- x$get()
        t <- solve(m,...)
        x$setInverse(t)
        t
}
