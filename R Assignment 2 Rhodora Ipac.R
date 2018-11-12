# Assignment No. 2 for Computing in Analytics 
#Submitted by : Rhodora B. IPac
#Date : November 12, 2018


#------------------------
#Problem 1 : Define an R function that removes NA values from a vector.


remove_NA <- function (vec3) {
  #vec3 <- c('a', NA, 'b','c', NA, 'd','e','f','g', NA)
  vec4 <- c()
  length_vec3 <- length(vec3)
  count <- 1 
  temp_vec <- is.na(vec3)
  while (length_vec3 != 0) {
    if  
     (temp_vec[count] != "FALSE")  { 
       count <-count + 1 
       length_vec3 <- length_vec3 - 1
 } else
     {
       vec4[count] <- vec3[count] 
       count <- count + 1  
       length_vec3 <- length_vec3 - 1
      }
  }
}

remove_NA (c('a', NA, 'b','c', NA, 'd','e','f','g', NA))
vec4


#Option 2 : Easier Way to remove NA 

miss_NA <- function(input_vector) {
    output_vector <- na.omit(input_vector)
    output_vector    
}

miss_NA(c("a", "b", "c", "d", NA, "e"))



#------------------------
# Problem 2 : Define an R function that computes the factorial of a given integer arguement. The output should be a vector of length 1 
# take input from the user

num <- c(0)
factorial <- 1
#num <- as.integer(readline(prompt="Enter a number: "))

fact_integer <- function(num){
  # check is the number is negative, positive or zero
  if(num <= 0) {
    print("Sorry, factorial does not exist for negative numbers")
  } else if(num == 0) {
    print("The factorial of 0 is 1")
  } else {
    for(i in 1:num) {
      factorial = factorial * i
    }
    print(paste("The factorial of", num ,"is",factorial))
  }  
}

fact_integer(1)

#----------------------------
#Problem # 3: Define an R function that computes the determinant
#of a given matrix.  The output should be a vector of length 1

determinant_X<-function(prm_matrix,key_row) {
  if (dim(prm_matrix)[1] == 1 && dim(prm_matrix)[2] == 1)
    return(prm_matrix[1,1])
  if (dim(prm_matrix)[1] == 2 && dim(prm_matrix)[2] == 2)
    return(prm_matrix[1,1]*prm_matrix[2,2]-prm_matrix[1,2]*prm_matrix[2,1])
  else
    s = 0
  for (i in 1:dim(prm_matrix)[2]) {
    s = s + prm_matrix[key_row,i]*(-1)^(key_row+i)*
      determinant_X(prm_matrix[-key_row,-i],key_row)
  }
  return(s)
}



#-------------------------
#Problem No. 4 Define an R function that sorts a given vector in decreasing order. The output should be a vector of the same length.  
# It should accept both numeric and character vector 


vec1 <- c(1,8,3,2,10,6,11,5)
vec2 <- c()

sort_vec <- function(vec1){
length_vec1 <- length(vec1)
a<-1
counter <- 0

  while (length_vec1 != 0){
    b <- max(vec1)
    vec2[a] <- b
    vec1 = vec1[vec1!=b]
    a=a+1
    counter=counter+1
    length_vec1 = length(vec1)
  }
 return(vec2)
}
sort_vec(vec1)




#---------------------------
# Problem No. 8 Create a function that computees the compound interest of an investment
#given the rate, time and initial amount or principal

comp_int <- function(principal_amount, interest_rate, n, term_principal ) {


interest_rate <- interest_rate/100
A <- 1+interest_rate/n
B <- n * term_principal
Amount <- 0
Amount <- principal_amount *(A)^(B)
Amount

}

comp_int()

#---------------------------
# Problem No. 9 Create a function isPrime(n) that accepts an integer and outputs a 
#boolean value (TRUE or FALSE) depending whether the integer is prime or not


is.prime <- function(num) {
  if (num == 2) {
    TRUE
  } else if (any(num %% 2:(num-1) == 0)) {
    FALSE
  } else { 
    TRUE
  }
}

is.prime(1)


#---------------------------
# Problem No. 9 Define an R function that accepts a POSIXct as argument and outputs the day of the week as characters.

give_date = as.POSIXct(as.Date("04/19/1972", format = "%m/%d/%Y"))
weekday = function(give_date) {c("Thursday", "Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday")[((unclass(give_date)/86400) %% 7) + 1]}

weekday(give_date)






