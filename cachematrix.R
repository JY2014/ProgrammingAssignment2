## Here are two functions that can read a matrix, create a special vector and calculate the inverse of the matrix 
## The value of the inverse is cached for future use to avoid repeating the same calculation 

######
## This function takes in the target matrix, and creates a special list
## The list contains functions to
## 1) set the value of the matrix, and clear the space for its inverse
## 2) get the value of the matrix   
## 3) set the value of the inverse
## 4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


#######
## This function calculate the inverse of the matrix stored in the previous function
## It first checks if the inverse already exists. If so, it directly uses the cached value
## Otherwise, the function calculates the inverse, and returns the value and stores it in the list

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setinv(inv)
    inv
}
