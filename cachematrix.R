## Put comments here that give an overall description of what your 
## functions do
## This code contains 2 functions:
## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

## Example for testing provided by Allan M. Due from Community TA:

# set.seed(42)
# mymat <- matrix(rnorm(25),5,5)
# mymat
# [,1]        [,2]       [,3]       [,4]       [,5]
# [1,]  1.3709584 -0.10612452  1.3048697  0.6359504 -0.3066386
# [2,] -0.5646982  1.51152200  2.2866454 -0.2842529 -1.7813084
# [3,]  0.3631284 -0.09465904 -1.3888607 -2.6564554 -0.1719174
# [4,]  0.6328626  2.01842371 -0.2787888 -2.4404669  1.2146747
# [5,]  0.4042683 -0.06271410 -0.1333213  1.3201133  1.8951935
# 
# solve(mymat)
# [,1]      [,2]      [,3]       [,4]      [,5]
# [1,]  0.68183594 -1.078746 -1.142347  0.7439971 -1.484073
# [2,]  0.09613058 -1.419227 -2.292037  1.3808827 -2.411346
# [3,] -0.06909654  1.819392  2.192748 -1.1807929  2.654590
# [4,]  0.14181901 -1.118677 -1.694580  0.7147536 -1.640329
# [5,] -0.24590901  1.090359  1.502458 -0.6939420  2.093756
# 
# solve(solve(mymat))
# [,1]        [,2]       [,3]       [,4]       [,5]
# [1,]  1.3709584 -0.10612452  1.3048697  0.6359504 -0.3066386
# [2,] -0.5646982  1.51152200  2.2866454 -0.2842529 -1.7813084
# [3,]  0.3631284 -0.09465904 -1.3888607 -2.6564554 -0.1719174
# [4,]  0.6328626  2.01842371 -0.2787888 -2.4404669  1.2146747
# [5,]  0.4042683 -0.06271410 -0.1333213  1.3201133  1.8951935
# 
# 
# cach1 <- makeCacheMatrix(mymat)
# cacheSolve(cach1)
# [,1]      [,2]      [,3]       [,4]      [,5]
# [1,]  0.68183594 -1.078746 -1.142347  0.7439971 -1.484073
# [2,]  0.09613058 -1.419227 -2.292037  1.3808827 -2.411346
# [3,] -0.06909654  1.819392  2.192748 -1.1807929  2.654590
# [4,]  0.14181901 -1.118677 -1.694580  0.7147536 -1.640329
# [5,] -0.24590901  1.090359  1.502458 -0.6939420  2.093756
# 
# 
# cach1$get()
# [,1]        [,2]       [,3]       [,4]       [,5]
# [1,]  1.3709584 -0.10612452  1.3048697  0.6359504 -0.3066386
# [2,] -0.5646982  1.51152200  2.2866454 -0.2842529 -1.7813084
# [3,]  0.3631284 -0.09465904 -1.3888607 -2.6564554 -0.1719174
# [4,]  0.6328626  2.01842371 -0.2787888 -2.4404669  1.2146747
# [5,]  0.4042683 -0.06271410 -0.1333213  1.3201133  1.8951935
# cach1$getinverse()
# [,1]      [,2]      [,3]       [,4]      [,5]
# [1,]  0.68183594 -1.078746 -1.142347  0.7439971 -1.484073
# [2,]  0.09613058 -1.419227 -2.292037  1.3808827 -2.411346
# [3,] -0.06909654  1.819392  2.192748 -1.1807929  2.654590
# [4,]  0.14181901 -1.118677 -1.694580  0.7147536 -1.640329
# [5,] -0.24590901  1.090359  1.502458 -0.6939420  2.093756


## Write a short comment describing this function
## makeCacheMatris is a function that contains 4 functions: set, get, setinverse, getinverse.
## Get function role is to return the matrix stored in the main function
## Set function changes the matrix stored in the main fucntion
## Setinverse and getinverse functions are similar to set and get. They don't compute the inverse, just store 
## the input value into a variable called m (setinverse) and return its value (getinverse)
## All these 4 functions are stored in the main function makeCacheMatrix using list() function. When makeCacheMatrix
## function is assigned to a variable that variable has all the 4 functions.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) m <<- inv
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function
## The first thing cacheSolve function does is to verify the value m, stored previously with getinverse 
##function, exists and is not NULL. If the value of m exists in memory, it simply returns a message and the value m, 
## that is supposed to be the inverse, but not necessarily.
## Then data gets the matrix stored with makeCacheMatrix, m calculates the inverse of the matrix using solve() function
## and x$setinverse(m) stores it in the object generated assigned with makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
