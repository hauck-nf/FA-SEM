#Operations with matrices
#The simple codes below comprise exercises intended to help students 
#develop a matrix understanding of factor analysis models

#creating a matrix
x=matrix(c(1,3,5,7,11,13),nrow=2,ncol=3)#column-wise
matrix(c(1,3,5,7,11,13),nrow=2,ncol=3,byrow=TRUE)#row-wise

x=cbind(c(1,3),c(5,7),c(11,13))
y=cbind(c(8,10,12),c(2,4,6))

#if you want to assign row and column names
colnames(x) <- c("C1","C2","C3")
rownames(x) <- c("R1","R2")
x
y

#exhibits the diagonal elements
diag(x)

#transpose a matrix
t(x)

#multiply matrices
x%*%y
y%*%x

#gives the inverse of a square matrix
z=cbind(c(2,1),c(5,9))
z
w=solve(z)
w
i=z%*%w
i
t(i)

#Understanding a factor model with matrices
###data available from: http://www.usp.br/nereus/wp-content/uploads/Johnson-Wichern-6th-Edition-2007-pp-502-559.pdf

#Defining the structural matrices of the factor model
Sigma=cbind(c(19,30,2,12),c(30,57,5,23),c(2,5,38,47),c(12,23,47,68))
L=cbind(c(4,7,-1,1),c(1,2,6,8))
Lt=t(L)
Phi=cbind(c(1,0),c(0,1))
Psi=cbind(c(2,0,0,0),c(0,4,0,0),c(0,0,1,0),c(0,0,0,3))

#calculating the Sigma matrix from the factor model
#specifying Phi
Implied=L%*%Phi%*%Lt+Psi
#simply assuming Phi is an identity matrix and removing it from the equation
Implied=L%*%Lt+Psi

#Estimating residuals of the factor model
Fit=Sigma-Implied
RMSR=sqrt(mean(Fit^2))
RMSR
