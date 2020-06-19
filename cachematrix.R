## Jaames Bennett
##R Programing
##Objective:  Cache the inverse of a Matrix 

## makeCacheMatrix is a function that generates a special "Matrix" object that 
## can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
                
        }
        
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list (set = set, get = get, 
              setinverse = setinverse,
              getinverse = getinverse)

}


## Function cacheMatrix uses the special matrix returned by the makeCacheMatrix function. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
                if (!is.null(m)){
                        message("getting cache data")
                        return(m)
                }
        data <- x$get()
       # squareddata <- apply(data, c(1,2), function(x) x^2)
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}

