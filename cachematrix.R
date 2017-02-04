## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## set the invmatrix variable to NULL, initialize it
        invmatrix <- NULL
        set <- function(y) {
                x <<- y
                invmatrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invmatrix <<- inverse
        getinverse <- function() invmatrix
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## retrieve the matrix
        ## if not NULL display message and return matrix
        invmatrix <- x$getinverse()
        if(!is.null(invmatrix)) {
                message("getting cached data.")
                return(invmatrix)
        }
        ## otherwise get the matrix
        ## store it in m
        ## solve m to invert the matrix
        ## return the result
        m <- x$get()
        invmatrix <- solve(m)
        x$setinverse(invmatrix)
        invmatrix
}
