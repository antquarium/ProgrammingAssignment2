## 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function takes a matrix whose inverse has been made cachable using the 
## makeCacheMatrix function, and returns its inverse (from cache where possible)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        ## Returns previously calculated inverse where possible
        if(!is.null(i)) {
                message ("Getting cached matrix inverse")
                return(i)
        }
        ## If not previously stored in cache, calculates & returns inverse
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

