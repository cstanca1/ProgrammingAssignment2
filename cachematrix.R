## One function caching the matrix inverse and the other function retrieve it in a later run.

## Matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
    mat.inv  <- NULL 
    set  <- function(y){ 
      x <<- y 
      mat.inv <<- NULL  
    } 
    get  <- function() x 
    setinverse  <- function(inverse) mat.inv <<- inverse 
    getinverse  <- function() mat.inv 
    list(set= set, get = get, setinverse = setinverse, getinverse = getinverse) 
}


## Retrieve the calculated inverse from cache when matrix did not change
cacheSolve <- function(x, ...) {
  
    ## Matrix that is the inverse of x
    mat.inv  <- x$getinverse() 
    if (!is.null(mat.inv)){ 
      message("getting cached matrix inverse") 
      return(mat.inv) 
    } 
    data  <- x$get() 
    mat.inv  <- solve(data, ...) 
    x$setinverse(mat.inv) 
    mat.inv 
}


## Test:
## > x = rbind(c(1, 2), c(3, 4)) 
## > mat = makeCacheMatrix(x)
## > mat$get()
## [,1] [,2]
## [1,]    1    2
## [2,]    3    4
## first run - no cache
## > cacheSolve(mat)
## [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5
## second run - from cache
## > cacheSolve(mat) 
## getting cached matrix inverse
## [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5