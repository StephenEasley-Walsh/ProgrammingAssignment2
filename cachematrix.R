# Example:
#-------------------------------------------------------------------------------------------------------------------
# x <- makeCacheMatrix(matrix(c(1,2,3,4),nrow=2,ncol=2))
# cacheSolve(x)
# *Inverse is calculated and printed*
# cacheSolve(x)
# *Inverse is printed but now with the message "getting cached data" since it is now from the cache*
#-------------------------------------------------------------------------------------------------------------------
#
# This code is not VERY similar to the example he gave. In fact, it is the same! I think the REAL point
# of this exercise was to get us using GitHib. I mean I really just replaced the word "mean" with "solve".
# You will also note I moved the {} around alittle to match my personal taste.
#
#-------------------------------------------------------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix())
{
        m <- NULL
        set <- function(y)
        {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

cacheSolve <- function(x, ...)
{
        m <- x$getsolve()
        if(!is.null(m))
        {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
