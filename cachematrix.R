## make_Cache_Matrix takes the value of the matrix as an input, sets the value of the matrix, gets the inverse of the matrix and sets the value of the invertible matrix

make_Cache_Matrix <- function(x = matrix()) {
        inverse_Matrix <- NULL #initially assuming that the inverse of the matrix is an empty matrix
        Set_Matrix <- function(y) #defining the matrix by setting it equal to y
                x <<- y #defining object in a different enviornment
        inverse_Matrix <<- NULL #assuming inverse of the matrix to be empty in another enviornment
Get_Matrix<- function()x  #getting the value of the matrix
Set_Inverse<- function(inverse) inverse_Matrix<<- inverse #setting the value of invertible matrix
Get_Inverse<- function() inverse_Matrix  #getting the value of the invertible matrix
list(Set_Matrix = Set_Matrix, Get_Matrix =Get_Matrix, Set_Inverse = Set_Inverse, Get_Inverse = Get_Inverse) #getting a list of matrix and its invertibles
} 

## Cache_Solve takes the output of the previous function as a matrix and returns the inverse of the matrix, if it exists

Cache_Solve <- function(x, ...) {
        inverse_Matrix <- x$Get_Inverse() #setting the value of invertible matrix
        if(!is.null(inverse_Matrix)){ #if the matrix is not empty
                message("Getting Cached Invertible Matrix") #get the following message
                return(inverse_Matrix)	#calculates the inverse of the matrix
        }
        Matrix_Data<- x$Get_Matrix() #gets the original data
        inverse_Matrix<- solve(Matrix_Data, ...) #solve the original data to get the inverse
        x$Set_Inverse(inverse_Matrix) #sets the invertible matrix
        return(inverse_Matrix) #returns invertible matrix as an output
}


#Output
##Test1- Inverse of the matrix has been calculated
> Test_Matrix<- matrix(1:4,2,2)
> Test_Matrix
[,1] [,2]
[1,]    1    3
[2,]    2    4
> Cache_Matrix<- make_Cache_Matrix(Test_Matrix)
> Cache_Matrix$Get_Matrix()
[,1] [,2]
[1,]    1    3
[2,]    2    4
> Cache_Matrix$Get_Inverse()
NULL
> Cache_Solve(Cache_Matrix)
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5


#Test 2- inverse of the matrix can not be calculated

> Test_Matrix <- matrix(4:12,3,3)
> Test_Matrix
[,1] [,2] [,3]
[1,]    4    7   10
[2,]    5    8   11
[3,]    6    9   12
> Cache_Matrix <- make_Cache_Matrix(Test_Matrix)
> Cache_Matrix$Get_Matrix()
[,1] [,2] [,3]
[1,]    4    7   10
[2,]    5    8   11
[3,]    6    9   12
> Cache_Matrix$Get_Inverse()
NULL
> Solve_Cache(Cache_Matrix)
Error in Solve_Cache(Cache_Matrix) : 
        could not find function "Solve_Cache"
> Cache_Solve(Cache_Matrix)
Show Traceback

Rerun with Debug
Error in solve.default(Matrix_Data, ...) : 
        system is computationally singular: reciprocal condition number = 3.17207e-17 

#Test3 - inverse of the matrix can not be calculated
> Test_Matrix <- matrix(1:16,4,4)
> Test_Matrix
[,1] [,2] [,3] [,4]
[1,]    1    5    9   13
[2,]    2    6   10   14
[3,]    3    7   11   15
[4,]    4    8   12   16
> Cache_Matrix<- make_Cache_Matrix(Test_Matrix)
> Cache_Matrix$Get_Matrix()
[,1] [,2] [,3] [,4]
[1,]    1    5    9   13
[2,]    2    6   10   14
[3,]    3    7   11   15
[4,]    4    8   12   16
> Cache_Matrix$Get_Inverse()
NULL
> Cache_Solve <- 
        + Cache_Solve(Cache_Matrix)
Show Traceback

Rerun with Debug
Error in solve.default(Matrix_Data, ...) : 
        Lapack routine dgesv: system is exactly singular: U[3,3] = 0 
> Cache_Solve(Cache_Matrix)
Show Traceback

Rerun with Debug
Error in solve.default(Matrix_Data, ...) : 
        Lapack routine dgesv: system is exactly singular: U[3,3] = 0 
       

#Test 4- Inverse of the matrix can be calculated
> Test_Matrix <- matrix(c(1,1,3,3,6,7,5,4,9,0,2,1,4,5,6,8),4,4)
> Test_Matrix
[,1] [,2] [,3] [,4]
[1,]    1    6    9    4
[2,]    1    7    0    5
[3,]    3    5    2    6
[4,]    3    4    1    8
> Cache_Matrix <- make_Cache_Matrix(Test_Matrix)
> Cache_Matrix$Get_Matrix()
[,1] [,2] [,3] [,4]
[1,]    1    6    9    4
[2,]    1    7    0    5
[3,]    3    5    2    6
[4,]    3    4    1    8
> Cache_Matrix$Get_Inverse()
NULL
> Cache_Solve(Cache_Matrix)
[,1]        [,2]        [,3]        [,4]
[1,] -0.15449438 -0.26966292  0.91573034 -0.44101124
[2,] -0.01404494  0.15730337  0.17415730 -0.22191011
[3,]  0.11516854 -0.08988764 -0.02808989  0.01966292
[4,]  0.05056180  0.03370787 -0.42696629  0.39887640





