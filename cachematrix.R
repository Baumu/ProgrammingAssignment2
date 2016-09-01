## The following two functions cache the inverse of a matrix.

## The function makeCacheMatrix creates an object that caches its inverse. 
## makeCacheMatrix contains 4 functions: set, get, setmean, getmean.
### get: returns the vector x stored in the main function.
### set: changes the vector stored in the main function.
### getmean and setmean: store the value of the input in the variable m.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
## The function cacheSolve computes the inverse of the matrix returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache. 
## If the inverse has not been calculated, data gets the matrix stored with makeCacheMatrix, 
## m calculates the inverse, and x$setmean(m) stores it in the object m in makeCacheMatrix.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
            message("getting cached data.")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
