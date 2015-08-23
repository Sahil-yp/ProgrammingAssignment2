## This script caches the inverse of a square matrix if the inverse matrix 
## has already been computed for a given matrix. If the inverse has not been 
## computed before it computes the required inverse and stores its value.
## The script utilizes two functions, viz., makeCachematrix and cacheSolve.


## The first function is a list of four sub functions; set, get, setinv,
## and getinv.


## The user can input a square matrix using makeCachematrix.

makeCachematrix <- function(x = matrix()) {
        
## After the user inputs the square matrix the function sets m as a Null
## value. This value will later be set as the inverse matrix.
        
        m <- NULL
        
## The purpose of the set function here is to set a new input matrix.
## Strictly speaking for this program, the set function can be discarded
## as the task of setting a new function can be performed by calling the 
## makeCachematrix. However, for a more complex program with multiple
## objects being calculated, the set function would be used.        
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
## The get function calls the x value of the makeCachematrix funciton, 
## i.e. the matrix input. This is used by the CacheSolve function to 
## call the matrix input by the user.
        
        get <- function() x
        
## The setinv function sets the value of the inverse matrix. It does not
## compute the value, but simply stores the computed value.

        setinv <- function(solve) m <<- solve
        
## Similar to the get function, the getinv function calls the value of
## computed inverse matrix.
        
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The second major function, cacheSolve computes the inverse of the input
## when run for the first time with a particular input. When the value has
## already been computed before it returns the previously calculated/stored value

cacheSolve <- function(x, ...) {

## The variable m gets the value called from the getinv function
        m <- x$getinv()

## The value of m is checked. If it is not a null value (from a 
## a previously calculated inverse) a message is displayed and the 
## stored inverse matrix is returned.

        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

## If the the m value is null, i.e. this is the first run of computation
## for the input matrix value, the inverse is calculated and stored into
## setinv from the makeCachematrix function.

        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
