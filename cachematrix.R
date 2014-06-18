## Two functions that can create a matrix and also cache its inverse

## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(m = matrix()) {
        
                ## Set the inverse property to null
                i <- NULL
                
                ## Function to 'set' the matrix
                set <- function(matrix) {
                        m <<- matrix
                        i <<- NULL
                }
                
                ## Function to 'get' the matrix
                get <- function() {
                        ## Return the matrix
                        m
                }
                
                ## Function to 'set the inverse' of the matrix 
                setInverse <- function(inverse) {
                        i <<- inverse
                }
                
                ## Function to 'get the inverse' of the matrix
                getInverse <- function() {
                        ## Return the inverse
                        i
                }
                
                ## Return a list of the different function outputs
                list(set = set, get = get,
                     setInverse = getInverse,
                     getInverse = getInverse)
}


## Calculate the inverse of the matrix returned from the 'makeCacheMatrix'
## function. If the inverse has already been calculated and if the matrix
## has not been changed, then return the inverse from cache.
cacheSolve <- function(x, ...) {
        
                ## Return a matrix that is the inverse of 'x'
                m <- x$getInverse()
                
                ## Only return the inverse if its already set
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                
                ## 'Get' the matrix from our object
                data <- x$get()
                
                ## Calculate the inverse using matrix multiplication
                m <- solve(data) %*% data
                
                ## Set the inverse to the object
                x$setInverse(m)
                
                ## Show the matrix
                m
}