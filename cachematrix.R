## Inversing a matrix considering that it is a square matirx

## This function gets and sets the matrix inverse

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


## This function caches the inverse matrix 

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
