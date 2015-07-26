## The functions below are for Coursera Course - R Programming
## The purpose of the functions is to take a square invertible matrix 
## and calculate the inverse.  The inverse will then be cached to be
## usable again without needing to recalculate the invesre.

## makeCacheMatrix creates and returns functions that
## are used by cachSolve to get the inverted matrix in cache
## or set it 

makeCacheMatrix <- function(x = matrix()) {
        cachedinverse <- NULL
        set <- function(y) {
                x <<- y
                cachedinverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) cachedinverse <<- inverse
        getinverse <- function() cachedinverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## cacheSolve evaluates the matrix created in makeCacheMatrix 
## if the inverted matrix exists in cache it returns the value
## otherwise it is created and the inverted value is stored in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'


cachedinverse <- x$getinverse()
        if(!is.null(cachedinverse)) {
                message("getting cached data")
                return(cachedinverse)
        }
        newmatrix <- x$get()
        cachedinverse <- solve(newmatrix, ...)
        x$setinverse(cachedinverse)
        cachedinverse
}
