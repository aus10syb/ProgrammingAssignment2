## Put comments here that give an overall description of what your
## functions do

## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        invs <- NULL
        set <- function(y) {
                x <<- y
                invs <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() invs
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)    

}


## This function computes the inverse of the 
## "matrix" returned by makeCacheMatrix above. If the
## inverse has already been calculated (and the matrix
## has not changed), then the cachesolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invs <- x$getinverse()
        if(!is.null(invs)) {
                message("getting cached data")
                return(invs)
        }
        data <- x$get()
        invs <- solve(data, ...)
        x$setinverse(invs)
        invs
        
}
