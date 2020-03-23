## These functions solve for the inverse of a matrix, and cache the inverse in memory
## The inverse matrix in memory can be updated to the inverse of a new matrix

## This function creates a list containing four functions. 
## The 'get' function returns the current matrix.
## The 'set' function will change the current matrix to a new one
## The 'setinverse' saves the inverse matrix calculated in the 'cachesolve' function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cachesolve returns the value of the inverse matrix. If the value is already saved, it 
## returns this value after returning "getting cached data"
##otherwise, it solves for the inverse of the matrix

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
