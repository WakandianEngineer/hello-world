## This functions facilitate and optimizes the calculation of the inverse of an
## square matrix. 

## This function creates a special object that stores the value of the
## original matrix, the value of the inverse matrix and all the fuctions
## that are needed to access or modify this values

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  #initializes the values x and inv
        set <- function(y) {    # modifies the values x and inv
                x <<- y
                inv <<- NULL
        }
        get <- function() x #returns the value of the matrix x
        setinv <- function(solve) inv <<- solve  #modifies the value of the inverse matrix
        getinv <- function() inv  #returns the value of the inverse matrix
        list(set = set, get = get,  setinv = setinv,  getinv = getinv)

}


## Computes the inverse matrix of a matrix and stores it in the cache

cacheSolve <- function(x, ...) {
        inv <- x$getinv()  # get the value of the inverse matrix an stores it in inv
        if(!is.null(inv)) {    # if the value stored in inv is NOT NULL, then returns
                # the inverse matrix
                message("getting cached data")
                return(inv)
        }
        d <- x$get() #stores the input matrix in d
        inv <- solve(d, ...)  # computes the inverse matrix of the input
        x$setinv(inv)          #stores the value of the inverse matrix in inv
        inv                    # returns the inverse matrix
        
        
}

