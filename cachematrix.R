## makeCacheMatrix(x) creates a matrix from x that is able to cache its inverse.
## cacheSolve(x) checks if matrix x has cached inverse. If cached inverse is
## present, the function will just print it instead of recalculating the inverse.

## makeCacheMatrix(x) creates a list containing 4 functions:
## 1. first function is set(y), which allows for setting a new matrix y,
##    This function also reset the value of "Inv" to NULL to ensure that new
##    inverse matrix will be based on matrix "y" and not the old matrix "x"
## 2. second funtion is get(), which just prints the matrix "x"
## 3. third function is setInv(), which cache the solved inverse as Inv
## 4. fourth function is getInv(), which just prints the inversed matrix "Inv"

makeCacheMatrix <- function(x = matrix()) {
    Inv <- NULL 
    set <- function(y) {
        x <<- y
        Inv <<- NULL
    }
    get <- function() x
    setInv <- function(solve) Inv <<- solve
    getInv <- function() Inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## cacheSolve(x) has 2 uses:
## 1. If an inverse for matrix "x" has been solved in the cache, it will fetch
##    the data using the getInv() funtion and return the inverse matrix "m"
## 2. Otherwise, it will solve the inverse, cache it using setInv() function,
##    and return the inverse matrix "m"

cacheSolve <- function(x) {
    Inv <- x$getInv() ## Defining Inv as cached Inverse mtx (if present)
    if(!is.null(Inv)) {
        message("getting cached data")
        return(Inv) ## This if-loop checks if inverse mtx "Inv" is solved or NULL
    }
    data <- x$get() 
    Inv <- solve(data)
    x$setInv(Inv)
    Inv ## Return a matrix that is the inverse of 'x'
}
