a1=array(5L,dim=c(5,5,1))
a1
a2=array(-10,dim=c(10,1,1))
a2

typeof(a1)
typeof(a2)

a3=array(c(-1,0,11,-1,0,12,-7,10,7,-1,0,9,-1,0,100),dim=c(3,5,1))
a3

is.matrix(a3)
a3=as.matrix(a3)
is.matrix(a3)


a=c(1,2,3)
b=c(4,5,6)
c=c(7,8,9)

a4=array(c(a,b,c),dim=c(3,3,1))
a4

print(head(letters,10))
print(head(LETTERS,10))

print(tail(LETTERS,5))
print(letters[2:10])


data=c(19,40,62,3,13,43,64,86,32,25)

max1=function(data)
{
  temp=data[1]
  for(val in data)
  {
    if(val>temp)
      temp=val
    
  }
  print(temp)
}
max1(data)


data2=data.frame(sl_no=c(1,2),name=c("KOUSHIK","ABC"),age=c(19,-1))
data2


data2[which(data2$name=="KOUSHIK"),"age"]=20
data2


data2$state=c("Medinipur","XXX")
data2


data2$age=NULL
data2


name=c("Abisekh","Ushs","ABC","PQR")
physics=c(88,81,90,87)
chemistry=c(82,91,85,89)
math=c(95,97,89,91)

data3=data.frame(name,physics,chemistry,math)
data3

data3$biology=c(82,81,90,87)
data3$aggregate=0

for(i in 1:nrow(data3))
{
  sum=0
  for(j in 2:ncol(data3))
  {
    sum=sum+data3[i,j]
  }
  data3[i,"aggregate"]=sum/(ncol(data3)-1)
}
data3

name=c("ABC","EFG","IJK")
age=c(20,19,21)
hight=c(163,167,139)
weight=c(57,69,32)
gender=c("F","M","M")
data4=data.frame(name,age,hight,weight,gender)
data4
print(data4$name)

data4=data4[order(data4$age),]
data4

gender1=factor(data4$gender)
gender1

