
v=sample(1:100,20)
print(v)


nth_highest=function(v)
{
  print(v)
  n=as.numeric(readline(prompt = "Enter a number: "))
  if(n<=length(v) && n>=0)
  {
    sorted_v=sort(v,decreasing = TRUE)
    print(sorted_v)
    nth_high=sorted_v[n]
    print(paste("nth highest number is: ",nth_high))
  }
}
nth_highest(v)
