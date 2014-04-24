#@@ -2,14 +2,33 @@
## functions do

## Write a short comment describing this function

###makeCacheMatrix  saves data  in a new environment
makeCacheMatrix <- function(x = matrix()) {

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


## Write a short comment describing this function
#On the first run, the cacheSolve calulates the inverse of matrix and stores the result in the environment created by the makeCacheMatrix. On the subsequient runs of the same matrix, the funtions checks if the inverse had been already calculated. If it did, the function returns the inverse. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
