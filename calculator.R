
k=1
while(k)
{
  print("1.add\n2.substuct\n3.multiplication\n4.devison\n5.exit")
  choice=as.integer(readline("Enter your choice: "))
  if(choice==1)
  {
    a=as.integer(readline("Enter 1 st number: "))
    b=as.integer(readline("Enter 2 nd number: "))
    print(paste("a+b=",a+b))
  }
  else if(choice==2)
  {
    a=as.integer(readline("Enter 1 st number: "))
    b=as.integer(readline("Enter 2 nd number: "))
    print(paste("a-b=",a-b))
  }
  else if(choice==3)
  {
    a=as.integer(readline("Enter 1 st number: "))
    b=as.integer(readline("Enter 2 nd number: "))
    print(paste("a*b=",a*b)) 
  }
  else if(choice==4)
  {
    
    a=as.integer(readline("Enter 1 st number: "))
    b=as.integer(readline("Enter 2 nd number: "))
    if(b==0)
      print("Devision by 0 not possible:")
    else
      print(paste("a/b=",a/b)) 
  }
  else if(choice==5)
  {
    print("BYe BYE!!")
    k=0
  }
  else
  {
    print("Invalid choice")
  }
}







