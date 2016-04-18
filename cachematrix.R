## The functions in this module are used to cache matrix and its inverse. 
## They are especially useful if matrix is large and creating an inverse can be time consuming
## Usage:
## 1. Create or load a non-empty matrix
## 2. Pass the matrix as a parameter to makeCacheMatrix and store the result in another variable - it will be a list where the values will be cached.
## 3. Pass the list variable as a parameter to cacheSolve and either retrieve the inverse matrix from cache, or, 
##    if it does not exist, calculate it and store it in cache.
## Note: Running cacheSolve with a 'normal' matrix will cause an error. The variable which is passed to this funciton needs to be a list created first 
##       using makeCacheMatrix.


## Function: makeCacheMatrix
## Creates a list which allows caching matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- matrix()
    set <- function(y) {
        x <<- y
        m <<- matrix()
    }
    get <- function() x
    setinverse<- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function: cacheSolve 
## Takes a special list from makeCacheMatrix as a parameter
## Either retrieves the inverse of matrix from cache or calculates it and stores in cache.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!all(is.na(m)) ){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
