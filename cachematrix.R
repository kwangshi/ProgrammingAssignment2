## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##
## This is just like a class object in Java or C++
## you have 2 class members to cache the info
##  1. inv -- the inverse matrix
##  2. x -- the inpuit matrix data
## and define 4 functions to manipulate them
##  1. set -- set the matrix x and clear the inv
##  2. get -- return the matrix x
##  3. setinverse -- set the inv value
##  4. getinverse -- return the cached inv value
##
##  this function return a list that contains these 4 functions
##
##  Usage:
##    mat <- matrix(c(1,  10, 7, 25), ncol=2)
##    myMat <- makeCacheMatrix(mat)
##    myMat$get()
##    myMat$setinverse(solve(mat))
##    myMat$getinverse()
##

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
##
##  Given a "makeCacheMatrix" object, return the inverse of the matrx
##  Logical flow:
##  1. if the cached inverse matrix exists, i.e. x$getinverse() is not NULL,
##	then put out a message and return the cached inverse matrx
##  2. Otherwise, get the matrix data thru get() function,
##	compute the inverse matrix thru "solve" function,
##	cache the inverse matrix to the "makeCacheMatrix" object (for future usage),
##	return the inverse matrix
##
##	Usage:
##	  cacheSolve(myMat)
##    	  mat2 <- matrix(c(4,  11, 9,  55), ncol=2)
##        myMat2 <- makeCacheMatrix(mat2)
##	  cacheSolve(myMat2)	## need to calculate the inverse matrix and cache it
##	  cacheSolve(myMat2)    ## Using cached result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if (!is.null(inv)) {
           message("getting cached inversed matrix")
           return (inv)
        }
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
