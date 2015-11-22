# Caching the inverse of a matrix
# This program creates a matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<-y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



# Calculating the inverse of the matrix given from function above
# If inverse has already been calculated, the following function will return the inverse from the cache

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        
        if(!is.null(inv)) {
                message("getting cached data from memory")
                return(inv)
        }
        
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
