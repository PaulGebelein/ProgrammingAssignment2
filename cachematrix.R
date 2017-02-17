## Put comments here that give an overall description of what your
## functions do

## The functions makeCacheMatrix and cacheSolve together allow to 
## store a matrix, calculate its inverse and retrieve both. The 
## inverse is calculated only if it is not cached (inverse == NULL).

## Write a short comment describing this function

## The function makeCacheMatrix creates an object containing a list 
## of four functions and two matrices ("x" and "inverse") for 
## internal use. The functions are:
## 1. "set": set the value of the matrix
## 2. "get": get the value of the matrix
## 3. "setinverse": set the value of the inverse
## 4. "getinverse": get the value of the inverse
## The functions can be used by x$"functionName()"


makeCacheMatrix <- function(x = matrix()) {     #argument: any invertible matrix
        
        inverse <- NULL                 #initialize "inverse"
        set <- function(y){
                x <<- y                 #assigns y to x of parent environment 
                                        #(i.e. makeCacheMatrix), so that it can be 
                                        #used after "set" is finished
                inverse <<- NULL        #deletes old inverse (again: parent environment)
        }
        get <- function() x             #returns the value of "x"
        setinverse <- function(matrixinverse) inverse <<- matrixinverse #puts the inverse 
                                        #into cache ("inverse")
        getinverse <- function() inverse        #returns inverse
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)   #creates the object as a list of functions
}


## Write a short comment describing this function
## cacheSolve takes an object created by makeCacheMatrix 
## and returns the inverse. If the inverse has been 
## calculated before cacheSolve just reads and returns the inverse. 
## If not cacheSolve calculates the inverse and updates the original object.


cacheSolve <- function(x, ...) {                #arguments: object created by "makeCacheMatrix()"
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()                       #get value of inverse
        if(!is.null(inverse)) {                         #check if inverse exists
                message("Getting cached inverse")
                return(inverse)
        }
        #if inverse == NULL meaning the inverse hasn't been calculated so far
        matrix <- x$get()                               #get value of matrix
        inverse <- solve(matrix, ...)                   #calculate inverse
        x$setinverse(inverse)                           #put result into object
        inverse
        
}
