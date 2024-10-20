## These functions compute and cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
          x <<- y
          i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        ## check if the inverse is cached, and return it.
        if(!is.null(i)) {
          message("getting cached inverse")
          return(i)
        }
        mat <- x$get()
        ## Computing the inverse of the matrix.
        i <- solve(mat, ...)
        ## cache the inverse.
        x$setinverse(i)
        i
}
