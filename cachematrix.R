## This is the second assignment in R Programming course
## Author: George Dai
## Date: April 16, 2020
##

## This special matrix is actually a list with four functions, get, set,
## getinverse and setinverse. The set and get are just get and set the
## matrix, while getinverse and setinverse are functions to get and set
## the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse
        )
}

## This is the cache inverse function, given the special matrix as an input
## from the above special matrix, if its inverse has been calculated bdfore
## it will simply return that inverse from cashe. Only if it is the first
## time the inverse will be calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

## This is the testing part of the routine
## First, create an inverseable matrix, then construct a special matrix
## using the makeCacheMatrix function, then feed it into cashSolve
## function. Use it a second time and this time the inverse is retrieved
## from cash, instead of calculating it

# > m = matrix(c(4,2,7,1,8,6,3,5,9), ncol=3, nrow=3)
# > cc = makeCacheMatrix(m)
# > inv <- cacheSolve(cc)
# > inv <- cacheSolve(cc)

