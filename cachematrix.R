## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        my_inverse <- NULL
        set <- function (y){
                my_inverse <<- NULL
                x <<- y
        }
        get <-function() x
        setinverse <- function(inverse) my_inverse <<- inverse
        getinverse <- function() my_inverse
        list (set = set, get = get, setinverse = setinverse, 
              getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.If the inverse is not yet calculated or the matrix was changed
## the new inverse is calculated and stored in the cache.

cacheSolve <- function(x, ...) {
        my_inverse <- x$getinverse()
        if (!is.null(my_inverse)){
                message("getting cached data")
                return(my_inverse)
        }
        data <- x$get()
        my_inverse <- solve(data) 
        x$setinverse(my_inverse)
        my_inverse
}
