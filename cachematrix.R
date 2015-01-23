##
## Following code contains functions that shows how to cache results
## of some potentially time-consuming computations.
##
## Provided functions cache result of inverse matrix computation.
## It's is a lazy computation (only when requested).
## Firstly object makeCacheMatrix is created, containing functions:
##     set, get, setSolve, getSolve.
## Object also stores matrix data.
##
## Then when result of the calculation of is requested for the first time
## inverse matrix is calculated and stored in cache.
## All following requests pull results from cache.
##
## For usage please check function testCache()
##


# clean all
rm(list=ls())        # remove all variables
dev.off(dev.list())  # clear all plots
cat("\014")          # clear the console

## Function creates list of functions used to store/get matrix
## and its inverse matrix to/from cache.
##
## Args:
##   x is a square numeric or complex matrix containing the coefficients of the linear system.
##
## Returns:
##   list containing functions:
##   * set(y)             - stores new matrix
##   * get()              - returns matrix from which inverse matrix is calculated
##   * setSolve(solution) - stores calculated inverse matrix
##   * getSolve()         - returns inverse matrix
makeCacheMatrix <- function(x = matrix())
{
    m <- NULL
    set <- function(y)
        {
            x <<- y
            m <<- NULL
        }
    get <- function() x
    setSolve <- function(solution) m <<- solution
    getSolve <- function() m

    ## return list of functions
    list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## Function returns a matrix that is the inverse of 'x'
##
## Args:
##   x is an object created by makeCacheMatrix function
##
## Returns:
##   matrix that is the inverse of 'x'
cacheSolve <- function(x, ...)
{
    # is it in cache?
    m <- x$getSolve()
    if (!is.null(m))
    {
        # yes, then done
        message("getting cached data")
        return(m)
    }

    # no, then calculate it and store it
    m <- solve(x$get(), ...)
    x$setSolve(m)

    m
}


## Function demonstrates usage of functions makeCacheMatrix and cacheSolve
## to calculate and cache inverse matrices.
testCache <- function()
{
    # create testing matrix 8x8 Hilbert matrix (http://en.wikipedia.org/wiki/Hilbert_matrix)
    hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
    h8 <- hilbert(8);

    # create wrapping object
    mat <- makeCacheMatrix(h8)

    # How does it look?
    print(str(mat))
    # Where is original stored?
    print(mat$get())

    # calculate
    sol <- cacheSolve(mat)
    # please notice that no message was printed

    # try again
    sol <- cacheSolve(mat)
    # now please notice message: 'getting cached data'
    # no calculation was necessary

    # is the result correct?
    identical(round(sol %*% h8, 3), diag(8))

    # set new matrix and once again
    mat$set(2*h8)
    sol <- cacheSolve(mat)
    sol <- cacheSolve(mat)
    identical(round(sol %*% (2*h8), 3), diag(8))
}