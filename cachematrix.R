## Author: Jagan Challa
## Date Nov 23, 2014
## functions to create a encapsulated version of a matrix with getters and setters for matrix and inverse of the matrix
## and function to solve the matrix
## forked from rdpeng/ProgrammingAssignment2 (https://github.com/rdpeng/ProgrammingAssignment2.git)

## Creates a list with getters (accessors) and setters(mutators) for a given matrix (encapsulates matrix object)
## 'x' and inverse of 'x' are computed and cached (if already computed)
## This is like an instance of a class in Java or an object in Javascript (more similar to Javascript's prototypical object, not sure if it's prototypical in 'R')
makeCacheMatrix <- function(x = matrix()) {
    #inverse of 'x' is initialized to NULL
    invX <- NULL
    #setter for x
    set <- function(y) {
        #sets x of makeCacheMatrix to whatever is passed to set()
        #probably a check for matrix inversibility (singularity) can be added before 'x' is set
        x <<- y
        #resets inverse of x whenever x is reset
        invX <<- NULL
    }
    #getter for 'x'
    get <- function() {x}
    #setter for inverse of 'X'
    setinvX <- function(inverseX) {invX <<- inverseX}
    #getter for inverse of 'X'
    getinvX <- function() {invX}
    #returns a list of getters and setters for 'x' and inverse of 'x'
    list(set = set, get = get,
         setinvX = setinvX,
         getinvX = getinvX)
}


## computes inverse of 'x' 
cacheSolve <- function(x, ...) {
    #if inverse of 'x' is available returns it, else computes inverse of 'x' and sets it usung x$setinvX()
    invX <- x$getinvX()
    if(!is.null(invX)) {
        message("getting cached data")
        return(invX)
    }
    dataX <- x$get()
    #compute inverse of 'x'
    invX <- solve(dataX)
    #sets invers of 'x'
    x$setinvX(invX)
    invX #x$getinvX()
}
