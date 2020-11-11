## This two functions cache the inverse of the matrix that is given as imput


## Creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        set <- function( matrix ) {
                m <<- matrix
                i <<- NULL
        }
        
        get <- function() {
                m
        }
        
        setInverse <- function(inverse) {
                i <<- inverse
        }
        
        getInverse <- function() {
                i
        }
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## Calculate the inverse of the matrix returned by the function above.
## If the inverse of the matrx has been calculated already, this function 
## retrieves it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        
        if( !is.null(m) ) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        
        m <- solve(data) %*% data
        
        x$setInverse(m)
        
        m
}
