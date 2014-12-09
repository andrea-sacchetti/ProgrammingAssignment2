# Function to create the matrix and return a list of functions:
#     set: sets the value of the matrix
#     get: returns the current (possibly cached) value of the matrix
#     setinverse: sets the value of the inverse matrix
#     getinverse: returns the (possibly cached) value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    # Initialize inverse I to NULL
    I <- NULL
    # function set: caches the argument matrix y into symbol x and caches NULL into symbol I
    set <- function(y) {
        x <<- y
        I <<- NULL
    }
    # function get: returns the matrix as the symbol x (hence the cached value)
    get <- function() x
    # function setinverse: caches the inverse matrix into the symbol I
    setinverse <- function(inverse) I <<- inverse
    # function getinverse: returns the inverse matrix as the symbol I (hence the cached value)
    getinverse <- function() I
    # Return a list of the above functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# Function to compute the matrix inverse or return its cached value if existing
#     Takes the output of makeCacheMatrix as input

cacheSolve <- function(x, ...) {
    # Call the getinverse function that returns the cached inverse matrix if any
    I <- x$getinverse()
    # If the cached values exists return it and exit function
    if(!is.null(I)) {
        message("getting cached data")
        return(I)
    }
    # Otherwise extract the data with the get function, 
    message("computing new data")
    data <- x$get()
    # compute the inverse, 
    I <- solve(data, ...)
    # store into cache value I with the setinverse function, and return it
    x$setinverse(I)
    I
}

# Using example as "Unit Test" - Comment out if not needed
x <- matrix(c(1,0,0,0,2,0,0,0,3), 3, 3)
s <- makeCacheMatrix(x)
print(cacheSolve(s))
print(cacheSolve(s))
print(cacheSolve(s))