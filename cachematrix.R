## Put comments here that give an overall description of what your
## functions do
##The first function, makeCacheMatrix creates a "vector", which is really a list containing a function to

##set the value of the vector
##get the value of the vector
##set the value of the inverse matrix
##get the value of the inverse matrix

## Write a short comment describing this function

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


## Write a short comment describing this function
##This function calculate the inverse of the cached matrix. However it first check is the inverse available in the cache.
##If it is available in the cache it get the value stored in cache and skip the computation.

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
