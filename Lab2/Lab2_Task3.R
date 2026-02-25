#TASK3

name <- readline("Enter your name: ")
phone <- readline("Enter phone number: ")

name <- toupper(name)
first3 <- substring(phone, 1,3)
last4 <- str_sub(phone,-4, )

cat("Hi, ", name, ". A verification code has been sent to ", first3, "- XXXXX ",last4)