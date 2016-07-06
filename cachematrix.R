## There are two functions designed to use the cache functionaility to return
## a matrix inverse calculation.
##
## The first function is makeCacheMatrix.  The function takes the argument of a
## matrix of some value.  The function then creates 4 different local functions
## and returns a "matrix" of the local functions.
##
## The second function is cacheSolve that takes the previously created "matrix"
## argument.  The function evaluates to determine if the inverse has already been
## calculated.  If it has then it retrieves the cached value.  Otherwise it
## uses the solve function to calculate and return the  inverse matrix.


makeCacheMatrix <- function(x = matrix()) {
    minverse <<- NULL    ## Sets minverse "global" value to Null to designate
    ## inverse matrix has not been calculated yet.
    get <- function () x
    setinverse <- function (solve) minverse <<- solve
    getinverse <- function () minverse
    list (get = get, setinverse = setinverse, getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    minverse <- x$getinverse()  #Gets cached value if available
    
    ## Tests if cached value was already calculated.  If it was then it returns
    ## the cached value.
    if(!is.null(minverse)){
        message ("Getting cached data.")
        return(minverse)
    }
    
    ## If cached value was not calculated, inverse is caclulated using solve
    ## function then "set" into the global value and returned.
    data <- x$get()
    minverse <- solve(data)
    x$setinverse(minverse)
    minverse
}

