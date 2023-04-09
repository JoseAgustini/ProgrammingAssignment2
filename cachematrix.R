# Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {

    i <- NULL

    # Creates the Matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    # Returns the matrix
    get <- function() {
        m
    }

    # Set inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    # Return the inverse
    getInverse <- function() {
        i
    }
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

# Creates the cache matrix
cacheSolve <- function(x, ...) {

    m <- x$getInverse()

 
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    # Get the matrix from our object
    data <- x$get()

    # Calculate the inverse using matrix multiplication
    m <- solve(data) %*% data

    # Set the inverse to the object
    x$setInverse(m)

    # Return the matrix
    m
}