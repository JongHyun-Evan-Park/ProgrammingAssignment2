# Week 3 - programming assignment 
# lexical scoping - caching functions
# take advantage of the scoping rules of the R language and how they can be manipulated to preserve state inside of an R object.

# use <<- operator to create a special object that stores a matrix and cache's its inverse.

###Caching a matrix###

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL 
        set <- function(y){ 
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv < -function() inv
        list(set = set, 
             get = get,
             setinv = setinv,
             getinv = getinv)
}

### Return a matrix that is the inverse of x ###

cacheSolve <- function(x = matrix(), ...) {
        # first, pass the value of "inv" to the current function
        inv <- x$getinv() 
        # second, check the value of the ojbect inv and if there is one, return inv and shut down the function
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        } 
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinv(inv) # store the value of inv
        inv
}
