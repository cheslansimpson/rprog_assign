## Two functions, "makeCacheMatrix" and "cacheSolve" are listed below. They work together to check
## if a matrix's inverse is stored into memory and if not, calculated and stored. This helps to avoid
## needless recalculation and thus speeds up computation time. 

## sets value of inverse and stores it into memory 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set_inverse <- function(solve) m <<- solve
        get_inverse <- function() m
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## Checks memory to see if inverse is present. If not, calcualtes inverse and stores it inmto 
## memory

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x[[get_inverse()]]
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x[[get()]]
        m <- solve(data, ...)
        x$set_inverse(m)
        m
}
