while(TRUE) {
  x = as.integer(readline(prompt = "Input the number: "))
  if (x <= 0) break
  
  if (x == 1) 
      print("1")
  else if (x == 2) 
      print("1, 1")
  else{
    output = "1, 1"
    b1 <- 1
    b2 <- 1
    for (i in 3:x){
      b3 <- b1+b2
      rate <- b2/b1
      output <- paste(output,b3,sep=", ")
      b1 <- b2
      b2 <- b3
    }
    print(output)
    print(b2/b1)
  }
}