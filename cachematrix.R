## Inversing a matrix considering that it is a square matirx

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setrev <- function(mean) m <<- solve(x)
        getrev <- function() m
        list(set=set, get=get,
             setrev=setrev,
             getrev=getrev)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix above. 
##If the inverse has already been calculated and the matrix has not changed, then 
## the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getrev()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setrev(m)
        m
}
