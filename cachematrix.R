## Put comments here that give an overall description of what your
## functions do

#A pair of functions that calculate and cache the inverse of a matrix

###### How to Use ######
# 1.Call makeCacheMatrix()
#		Mat<-makeCacheMatrix()
# 2.Build the matrix:
#		Mat$setM()
# 3.Call cacheSolve()
#		cacheSolve(Mat)

## Write a short comment describing this function
# Function for setting and getting a matrix
# and his inverse, using cache
makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL
	#Function for building the matrix. A vector containing the numbers,
	#and the dim of the matrix are mandatory.
	setM<-function(y,r,c,...){
		x<<-matrix(y,r,c,...)
		inv<<-NULL
	}
	#Function that return the matrix
	getM<-function() x
	#Setting the Inverse
	setInv<-function(inver) inv<<-inver
	#Getter for the inverse
	getInv<-function() inv
	#List containing the requested data
	list(setM=setM,getM=getM, setInv=setInv, getInv=getInv)
}


## Write a short comment describing this function
# Function for obtaining the inverse of a matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	#Storing the inverse, if possible
	inv<-x$getInv()
	#Checking if already have the inverse
	if(!is.null(inv)){
		message("retrieving data")
		return (inv)
	}
	#If not calculate
	data<-x$getM()
	inv<-solve(data)
	#and stored in the list
	x$setInv(inv)
	inv
}
