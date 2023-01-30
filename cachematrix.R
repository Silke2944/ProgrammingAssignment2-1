## Below are two functions to chache the inverse of a matrix. 
## This first function creates a matrix that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(solveMatrix) inv <<- solveMatrix
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
} 


## This second function computes the inverse of the matrix that is returned by the first function. The inverse is calculated but only stored in the cache. CacheSolve will retrieve the inverse from the cache.
## This pair of function is only applicable for squared matrices. To calculate the inverse for a non squared matrix the MASS pacakge should be installed. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv      
}
