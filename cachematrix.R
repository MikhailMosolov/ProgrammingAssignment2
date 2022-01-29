## First function is the functions' vector, which can set and get a matrix, 
## setInverse and getInverse of that matrix.

## Considered that the matrix supplied is always invertible, we don't have to 
## focus on math part(determinant calcululation) and draw an analogy from the 
## example.

makeCacheMatrix <- function(x = matrix()) { 
    # the matrix shoul be squared,i.e., ncol==nrow
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv<<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Second function checks wether the inversed matrix is stored in the first 
## function' object. If so, it prints out its value and saves time. If not, 
## computes the inverse matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv<-x$getInverse()
    if(!is.null(inv)) {
        message("getting cached matrix")
        return(inv)
    }
    matrix<-x$get()
    inv<-solve(matrix, ...)
    x$setInverse(inv) # caches the inverted matrix
    inv 
    
}
