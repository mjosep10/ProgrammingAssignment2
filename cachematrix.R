## Put comments here that give an overall description of what your
## functions do

## For this funciton, makeCacheMatrix, I ensure that the matrix in invertible by representing all NULL variables by "invrs".
## Then the set the value of the matrix is set using naother function with double assignment.
## The following functions get the value of the matrix, set the inverse,and get the value of the inverse for the cache.

makeCacheMatrix <- function(x = matrix()) {
    invrs <- NULL
    set <- function(y){
            x <<- y
            invrs <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invrs <<- inverse
    getinverse <- function() invrs
    list(set = set, get = get, setinverse = setinverse,
         getinverse = getinverse)
}
    




## This function computes the inverse of the makeCacheMatrix function above
## Returns a matrix that is the inverse and assigns it to invrs
## If the inverse of the matrix has already been calculated by the makeCacheMatrix, it will use that value
## The value is returned as invrs

cacheSolve <- function(x, ...) {
    invrs <- x$getinverse()
    if(!is.null(invrs)){
          message("getting cached data")
          return(invrs)
    }
    data <- x$get()
    invrs <- solve(data, ...)
    x$setinverse(invrs)
    invrs
}
