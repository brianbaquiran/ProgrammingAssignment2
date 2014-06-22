## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# creates a special "matrix" which is a list containing setter 
# and getter functions for the cached matrix and inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setinv <- function(inverse) {
                inv <<- inverse
        }
        
        getinv <- function() inv
        
        list(set=set, get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function
# extracts the cached inverse of a "matrix" created with 
# makeCacheMatrix() if the cache exists, otherwise 
# computes the inverse and caches it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinv()
        if (!is.null(x$getinv())) {
                return(inverse)
        } else {
                m <- x$get()
                inverse <- solve(m)
                x$setinv(inverse)
                return(inverse)
        }
}
