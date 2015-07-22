## The two functions help in saving time when inverse of a matrix is computed.

## makeCacheMatrix creates a special vector, which stores 4 subfunctions
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }                                              # subfunction 1
    get <- function() x                            # subfunction 2
    setinverse <- function(invert) inv <<- invert  # subfunction 3
    getinverse <- function() inv                   # subfunction 4
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)                  # store the 4 subfunctions
}


## CacheSolve returns the inverse of the matrix. 
## If the inverse already exists in the cache, it skips the computation.
## Else, it calculates the inverse and sets the inverse in the cache.
## Finally, it returns the inverse in both the cases mentioned above.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }                                # return the inverse stored in cache
    
    data <- x$get()                  # get the matrix
    inv <- solve(data)               # compute the inverse
    x$setinverse(inv)                # set the inverse in cache for future use
    inv                              # return the inverse
}
