## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix that takes a matrix as its input.
## It is going to set the value of the matrix, get the value of the matrix, 
## set the value of the inverse of the matrix and get its value.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function takes the special matrix as its input and checks if the inverse has
## already been calculated. If that's not the case then it proceeds to calculate it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("The inverse of this matrix has been cached. Getting...")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
