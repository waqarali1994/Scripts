# Base R learning
 
#Variables naming
x<-5
yield.project<-c("hello")
ear_yield<-10

#Assignment to variables
yield<-11
multispeq<-20
print(multispeq)
var1<- 10
var2<-20
cat(var1, var2)  #for combining multiple variables printing together
print(var1, var2) # for printing single variables values


# datatypes in R
# numeric, logical, character, integer, complex, raw0
# by default data type is numeric
numeric<-10.267
integer<-10L
complex<- 5+2i
logical<-TRUE
character<- "hello"

typeof(numeric) #to check datatype another function like class
typeof(integer)

chr<- as.character(numeric)
class(chr)
intl<- as.integer(numeric)
class(logical)
class(complex)
class(intl)

class(numeric) #to check variable/datatype in R, we use class function
class(integer)


# converting data types in R


#converting to numeric types
num2<- as.numeric(integer)
class()
num3<-as.numeric(23i)
class(num3)
num4<- as.numeric(TRUE)
num5<- as.numeric("hello")
class(num5)
num6<- as.numeric("123456")

#converting to integer types

int3<- as.integer(685.90)
class(int3)
int3<- as.integer(TRUE)
int4<- as.integer(231-68)
int5<- as.integer("123456")
int6<- as.integer("123456aaaa")

#conversion to complex

comp1<- as.complex(45)
comp2<- as.complex(FALSE)
comp3<- as.complex("12345")
comp4<- as.complex("21243556dddd")
class(comp4)

#conversion to logical

logi1<- as.logical(35)
logi2<- as.logical("124345")
logi3<- as.logical(0)
logi4<- as.logical(40i)
logi5<- as.logical("hello")


#converting to characters

chr3<- as.character(49)
chr4<- as.character(5i)
chr5<- as.character(TRUE)

# Operators in R

#Arithmetic + - * / ^ %% %/%
#logical 
#assignment 
#relational < > == <= >= !=

#Arithmetic 
a<-5
b<-2
print(a+b)
print(a-b)
print(a*b)
print(a/b)
print(a^b)
print(a%%b)
print(a%/%b)


#arithmetic on vectors

v1<-c(2,4,6,8,10)
v2<-c(1,3,5,7,9)

print(v1+v2)
print(v1%/%v2)
print(v1^v2)
print(v1-v2)
print(v1*v2)
print(v1/v2)
print(v1%%v2)
print(v1^v2)


#relational on vectors
print(v1<v2)
print(v1>v2)
print(v1==v2)
print(v1>=v2)
print(v1<=v2)
print(v1!=v2)


#logical on vectors & | ! || &&
d<- c(3.5)
e<- c(4.7) # & and | can be used to compare vectors/variables containing more than one elements
#but || and && can be used for comparing vectors/variables containing only single elements
print(d&&e)
print(!v2) # changes the output of a expression from true to false and vice versa


#assignment operators on vectors = <- -> <<- ->>
v1<-c(2,4,6,8,10) # used to assign values to variable
v2<-c(1,3,5,7,9)




# paste and paste0

d3<- c("apple", "berry", "cherry", "citrus")
typeof(d3)

d7<- paste("appleberrycherrycitrus", collapse = ",")

print(d7)
d4<- paste(d3, sep = "", collapse = ";")

print(d4)

d5<- paste0("apple", "berry", "cherry", "citrus")
print(d5)

# Data structures in R

# in R we have vectors, matrix, array, list and data frames

# vector is a sequence of elements which share the same data types, all the elements
# of vectors are known as components
# length() function finds the number of elements in the vector
# atomic vector and list (parts of vector)
# how to create vector
# we normally use c() to create vector

a<-c(1,2,3,4,5,6,7)
b<- -3:5 # creating vector through colon operator
print(b)

f<- seq(1,5) # seq() is used to create vector also
print(f)
g<-seq(14,24, by=0.35) # by is used to give precise internal/gap
print(g)

h<-seq(1,4,length.out=5) # length.out is used to provide total # of values in the expression

# atomic vector has 4 types
# i) numeric vector
num11<- c(11,17,22,29,37)
class(num11)

#ii) integer
int11<- as.integer(num11)
class(int11)
print(int11)

#iii) character we always use strings "" to define character vector in R
chr21<- c(1,5,7,8,9)
chr22<- as.character(chr21)
typeof(chr22)
chr23<- c("honesty","is","the","best","policy")
chr24<- paste0(chr23)

class(chr23)
print(chr24)


#iv) logical



# vector operations

# indexing

# accessing the elements of a vector is called indexing
# first method is by indexing [] (square brackets) 
#in R programming indexing starts from 1 not 0

# numeric indexing
chr21[3]

#character vector indexing
policy<- c("honesty", "is", "best", "policy")

policy[-1]
policy[1:3]
policy[c(3,4,4,3,2,1,3,42,7)]


#logical vector indexing
c2<- c(1,2,3,4,5,6)
c2[c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)]


# combining vector
v3<- c(4,67,3,5,3,4)
v4<- c(v2,v3)
print(v4)

#arithmetic operations
v6<- c(v2+v3)
print(v6)

# naming of vector
z<- c("ear weight", "percent moisture", "kernel row number", "percent filling")
names(z)<-c("t1","t2","t3")
print(z)


# Lists in R (type of data stucture in R)

 # lists are used to store an hold different types of elements
# to create list we use list() function
# number of elements can be different across different vector types to hold in list

dec<- c(3,4,5,6,3,4,4) # numeric vector
chracterv1<- c("Mohsin", "Hamid", "Asad", "Ali")# character vector
logicv1<- c(TRUE,FALSE, TRUE,FALSE,TRUE) # logic vector
list1<- list(dec, chracterv1,logicv1)  # creating list
print(list1)

list2<- list("Sajid","James",c(1,3,56,34,6),TRUE,FALSE,57.96,45L)

# naming of list

list3<-list(c("Naeem","Altaf","Ramesh"),c(234,3,34),list("BA","PhD","MS"))
list3
names(list3)<-c("students","marks","degrees")
list3

#Accessing/indexing the list
list3[3]
list3[1]

#by naming
list3["marks"]
list3$students
list3$degrees

#converting list to vector using unlist() function to perform arithmetic operations

list4<- list(29:36)

list5<- list(45:52)

#arithmetic operation on vectors converted from list
g1<-unlist(list4)
g2<-unlist(list5)
typeof(g1)
addition_g1_g2<- g1+g2
addition_g1_g2

# merging the lists

merged_lists<- list(list4,list5)
print(merged_lists)


# Arrays (type of data structure in R)
# used to store data in more than 2 dimensions


# Matrix in R we use matrix() function to create matrix in tow dimensional order
#matrix(data,nrow,ncol,byrow,dim_nmae)

#matrix creation
mat1<- matrix(c(7:18),nrow = 4,byrow = TRUE)
mat2<-matrix(c(6:13),nrow = 4, ncol = 4, byrow = FALSE)
mat2




#naming of matrix with dim_name parameter

mat3<- matrix(c(5:16),nrow = 4,ncol = 3,byrow = TRUE)
mat4<- matrix(c(7:18),nrow = 4,ncol = 3,byrow = TRUE)

row_name<-c("r1","r2","r3","r4")
col_names<-c("c1","c2","c3")
matrix_naming<- matrix(c(7:18),nrow = 4,ncol=3, byrow= FALSE,
                       dimnames = list(row_name,col_names))

matrix_naming

#ACESSING/INDEXING MATRIX

matrix_naming[3,1] #indexing specific row and column
matrix_naming[2,] #indexing whole row
matrix_naming[,3] # indexing whole column


# assignment operator used to modify matrix content
matrix_naming
matrix_naming[4,3]<-0 #using assignment operator
matrix_naming[matrix_naming==11]<-0 #using relational operator
matrix_naming[matrix_naming<15]<-0

#cbind() and rbind() are used to add column and row

matrix_naming<-rbind(matrix_naming,c(2,3,4))
matrix_naming

matrix_naming<-cbind(matrix_naming,c(8,5,2,0))
matrix_naming

# t() is used to transpose matrix
t(matrix_naming)


# addition,subtraction,division of matrix

q1<-matrix(c(4,5,6,7),nrow = 4,ncol = 3,byrow = TRUE)
q2<- matrix(c(8,9,10,7),nrow = 4,ncol = 3,byrow = TRUE)

sum_q1_q2<- q1+q2
sum_q1_q2

subtract_q1_q2<- q1-q2
subtract_q1_q2

multiply_q1_q2<- q1*q2
multiply_q1_q2

divison_q1_q2<- q1/q2
divison_q1_q2



# Data frame in R is a two dimensional structure in the format of column and row, \\
# data #stored in column of data frame can be of various type like list
# in data frame, column contains value of one variable and row contains one set 
# of values from each column
# for different variables and unlike list, components of data frame has equal 
# length to operate


# creating data frame
Genotype_id<-as.character(c(1480:1489))
plot_id<- c(1:10)
Ear_length<-seq(15.5,17.5,length.out=10)
Ear_length
Ear_width<- seq(3.5,5.3,length.out=10)
Ear_width


yield_traits_dataframe<- data.frame(plot_id=plot_id, Genotype_id=Genotype_id,
                                    Ear_length=Ear_length,Ear_width=Ear_width)


yield_traits_dataframe






#Factors in R
#categorical variables
#types ordinal and nominal
#nominal variable doesn't follow any ordering (like male and female)
# ordinal follow ordering like (low temp, medium,. high)
# R use factors to store categorical data as levels we can factorize character and integr
#type sof data

dir<- c("north","west","east","south")
is.factor(dir)
factor(dir)
factor(dir,levels = c("north","west","south","east"),labels = c("N","W","S","E"))

factor(dir,levels = c("north","west","south","east"))

#making levels of factor
v1<- gl(3,4,labels = c("Mohsin","Hasnat","Ahmad")) # n is number of levels and
# k is # of copies of labels
v1
data<-factor(dir) #making factor
data
data[c(2:4)] #indexing
data[-1]
data[2]<-"east" #modification
data[2]
#is.factor()
#is.ordered # check the ordering of a vector
#as.factor changes the vector to factor
#as.ordered arranges the vector in a order


# Function in R (is  aset of statement organized tpgether to perform a some specfic task)
# user defined function and built in Functions
#function() is used to create a function

#fun_name<-function(argu1,arhu2.....){} function body

new_function<-function(){    # creating a function
  for(i in 1:5){
    print(i^2)
  }
}
 new_function() # calling function


new.function<- function(x,y,z){ #giving arguments
  res<-x+y+z
  print(res)
}
new.function(4,5,6)

new_function2<- function(x=10,y=40){
  res<-x*y
  print(res)
}

new_function2(5,6) #R will overwrite the previous default arguments values


 
 
 
 
 
 







#Conditional statements


#if and else statement
y<- 100
if(is.integer(y))
{
  print("y is an interger")
}else
  {
  print("y is not an integer")
}
y<-c("Hardwork", "is", "the", "key", "of", "success")
print(y)

if("vector" %in% y)
{
  print("key is found in our vector")
}else
{
  print("key is not found in our vector")
}

#if with nesting
marks<-55
if(marks>83){
  print("First Grade")
}else if(marks>75){
  print("Second Grade")
}else if(marks>60){
  print("Third Grade")
}else{
  print("Fail")
}


#for loop general

for (y in 1:50) {
  print(y)
}


#for loop with vector
f<-c("pencil", "pen", "keyboard")

for (i in f) {
  print(f)
}



#while loop

v<-c("hello", "R", "Programming")
x<-2
while (x<6) {
  print(v)
  x<-x+1
}





#repeat loop

v<-c("hello", "how", "are", "you")
x<-2
repeat{
  print(v)
  x<-x+1
  if(x>5){
    break
  }
}