
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The function creates a matrix object x and makes use of set and get functions to set and get the value of the vector
##set the value of the vector
##get the value of the vector

makeCacheMatrix <- function(x = matrix()) {
              
        ## Init the cache m 
        m <- NULL
        set <- function(y) {  ## set the value of y to store it
                x <<- y ## assign the input matrix y to the variable x in the
                ## parent environment with <<- used to assign a value to an object in an environment that is different from the current environment
                m <<- NULL ## since the matrix is assigned a new value, flush the cache i.e re-initialize m in the parent environment to null
        }
        get <- function() x ## return the matrix x
        setinverse <- function(inverse) m <<- inverse ## set the cache m equal
        ## to the inverse of the matrix x
        getinverse <- function() m ## return the cached inverse of x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function calculates the inverse of an invertible matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        if(!is.null(m)) { ## test if the mean has already been calculated
                message("getting cached data") ##case the mean exist and and the matrix has not change, if so skip computation and obtain the inverse from the cache
                return(m)
        }
        data <- x$get() ##case the mean does not exist in the cache 
        m <- solve(data, ...) ## matrix supplied is invertible so the function solve is ok to calcule the matrix inverse and solve() returns its inverse
        x$setinverse(m) ## The value of the inverse calculated is set in the cache with the function setinverse
        m
}