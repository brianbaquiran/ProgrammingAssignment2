# makeCacheMatrix 
# Creates a special "matrix" structure which is a list containing setter 
# and getter functions for the cached matrix and inverse. The structure 
# contents can be accessed via regular list accessors
# 
# Usage:
# > cached_matrix <- function(my_matrix)
# > cached_matrix$get()
# > cached_matrix$set(some_other_matrix)
# > cached_matrix$getinv()
# [1] NULL (because we haven't computed the inverse yet)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        # setter function. replaces the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # getter function. returns the matrix
        get <- function() x
        
        # inverse setter function
        setinv <- function(inverse) {
                inv <<- inverse
        }
        
        # inverse getter function
        getinv <- function() inv
        
        # return the list structure consisting of the 
        # setter, getter, inverse setter and inverse getter functions
        list(set=set, get=get,setinv=setinv,getinv=getinv)
}


# cacheSolve
# Extracts the cached inverse of a "matrix" created with 
# makeCacheMatrix() if the cache exists, otherwise 
# computes the inverse and caches it. The ... argument lets 
# callers pass additional arguments to solve().
#
# Usage: 
# > cached_matrix <- function(my_matrix)
# > cached_matrix$getinv()
# NULL
# > cached_inverse <- cacheSolve(cached_matrix)
# # is the inverse cached?
# > identical(cached_inverse, cached_matrix$getinv())
# [1] TRUE
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinv()
        if (!is.null(x$getinv())) {
                # the inverse has already been cached 
                # so we can just return it
                return(inverse)
        } else {
                # inverse not yet set. compute it, cache it 
                # and return it
                m <- x$get()
                inverse <- solve(m, ...) # also pass any extra args to solve
                x$setinv(inverse)
                return(inverse)
        }
}
