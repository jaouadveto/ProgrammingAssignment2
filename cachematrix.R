makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}
cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}
a <- makeVector(c(1,2,3,4))
a$get()
a$getmean()
cachemean(a)
a$getmean()
a$set(c(10,20,30,40))
a$getmean()
cachemean(a)
cachemean(a)
a$get()
a$setmean(0)  
a$getmean()
a$get()
cachemean(a)
a <- makeVector(c(5, 25, 125, 625))
a$get()
cachemean(a)
cachemean(a)
## Together, makeCacheMatrix and cacheSolve solve the inverse
## of an invertible matrix, store the inverse in cache and 
## return the cached matrix when the inverse is requested again.
## This eliminate's the need to process a computer-intensive task
## multiple times.

## makeCacheMatrix creates a list of functions which allow
## for the storage of the inverse of a matrix in cache. X
## and s are defined so that they can be accessed outside of
## the function.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setInv <- function(solve) s <<- solve
    getInv <- function() s
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## cacheSolve checks to see if the inverse of a requested matrix
## is stored in cache.  If it is, it retrieves it.  If it is not,
## it computes it and then calls on the setInv() function from the
## list created by makeCacheMatrix to cache the result.  In either
## circumstance, it returns the matrix's inverse.
cacheSolve <- function(x, ...) {
    s <- x$getInv()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setInv(s)
    s
}
M <- matrix(1:4, ncol=2, nrow=2)
cacheM <- makeCacheMatrix(M)
cacheSolve(cacheM)
cacheM$get()
cacheM$getInverse()
cacheSolve(cacheM) 
