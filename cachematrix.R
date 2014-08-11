## FUNCTIONS in File: makeCacheMatrix() & cacheSolve(x,..)
## PURPOSE: cache the inverse of a matrix rather than compute it repeatly

## create a special cache matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        x_inv <- NULL
        
        # set matrix value (assign value to object in parent env. for cache)
        set <- function(y) {
                
                x <<- y 
                x_inv <<-NULL
        }
        
        # get matrix 
        get <- function() x
        
        # set matrix inverse (assign value to object in parent env. for cache)
        setinverse <- function (in_matrix_inv) x_inv <<- in_matrix_inv
        
        # get matrix inverse
        getinverse <- function () x_inv
        
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## computes the inverse of the matrix returned by makeCacheMatrix
## if the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        x_inv <- x$getinverse() 
        
        #check if the inverse has already been calculated
        if (!is.null(x_inv)) {
                # if inverse exists, get cached inverse matrix
                message("getting cached data")
                return (x_inv)
        }
        
        # if inverse doesn't exist, calculate inverse
        data <- x$get()
        x_inv <- solve(data, ...)
        x$setinverse(x_inv)
        x_inv
}
