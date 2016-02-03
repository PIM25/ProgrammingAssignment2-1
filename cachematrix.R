## cachematrix : programming assignment 2 - lexical scoping

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL # set the inverse to NULL as a placeholder for a future value
    
    # define a function to set the a new matrix, y, and reset the inverse, m, to NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x # return the matrix, x
    setinverse <- function(solve) m <<- solve # set the inverse, m, to solve
    getinverse <- function() m # return the inverse, m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse) # return the 'special matrix' with the functions just defined
    
}


## This function computes the inverse of the special "matrix"
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse() 
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
    
}



## Exemples :
# > mat1<-c(1,0,0,1)
# > dim(mat1)<-c(2,2)
# > xx<-makeCacheMatrix(mat1)
# > cacheSolve(xx)
# [,1] [,2]
# [1,]    1    0
# [2,]    0    1
# > mat2<-c(0,1,1,0)
# > xx<-makeCacheMatrix(mat2)
# > cacheSolve(xx)
# Error in solve.default(data, ...) : 'a' (4 x 1) must be square > dim(mat2)<-c(2,2)
# > dim(mat2)<-c(2,2)
# > xx<-makeCacheMatrix(mat2)
# > cacheSolve(xx)
# [,1] [,2]
# [1,]    0    1
# [2,]    1    0
# > mat2 %*% mat2
# [,1] [,2]
# [1,]    1    0
# [2,]    0    1
# > mat3<-4*mat1
# > mat3
# [,1] [,2]
# [1,]    4    0
# [2,]    0    4
# > xx<-makeCacheMatrix(mat3)
# > mat4<-cacheSolve(xx)
# > mat4 %*% mat3
# [,1] [,2]
# [1,]    1    0
# [2,]    0    1