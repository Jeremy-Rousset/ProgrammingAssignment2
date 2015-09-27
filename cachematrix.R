## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function create an object that can cache the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(A) {

                x <<- A
                I <<- NULL
        }
        get <- function() x
        setinv <- function(inv) I <<- inv
        getinv <- function() I
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
#This function computes the inverse of the matrixreturned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed)
#then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        I <- x$getinv()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(I)
        I
}
