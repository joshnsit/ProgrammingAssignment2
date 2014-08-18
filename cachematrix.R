#------------------------------------------------------------------
#Execution steps are given at the last in comments for getting 
#inverse of matrix. Multiplication of matrix with its inverse has 
#also been shown to get unit matrix for the purpose of validation.
#makeCacheMatrix is the function which takes the input matrix
#and cacheSolve function checks for presence of the inverse matrix
#at global level and obviates its calculation if already present.
#------------------------------------------------------------------

#------------------------------------------------------------------
#makeCacheMatrix function returns a list of functions including 
#the function to the inverse. 
#------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix(,nrow=1,ncol=1)) {
        inv <- NULL
        message("Hope you have Ensured that matrix is square as inverse for a matrix is possible only with square matrix")
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
                message("inside set matrix function")
        }
        get <- function() x
        setinv <- function(mean) {
                inv <<- mean
                message("inside set matrix function")
        }        
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


#------------------------------------------------------------------
#cacheSolve function actually calls inverse function and sets the 
#matrix object at global level with the inverse object. This function
#takes the argument as another function listed in makeCaheMatrix
#------------------------------------------------------------------
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached INVERSE MATRIX data")
                return(inv)
        }
        smatrix <- x$get()
        inv <- solve(smatrix, ...)
        x$setinv(inv)
        inv
}

#> p<-makeCacheMatrix(matrix(1:4,2,2))
#Hope you have Ensured that matrix is square as inverse for a matrix is possible only with square matrix
#> cacheSolve(p)
#inside setmean function
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> cacheSolve(p)
#getting cached INVERSE MATRIX data
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> matrix(1:4,2,2)%*%cacheSolve(p)
#getting cached INVERSE MATRIX data
#[,1] [,2]
#[1,]    1    0
#[2,]    0    1