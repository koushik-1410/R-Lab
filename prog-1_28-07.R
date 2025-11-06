col1=c("i","I",1,"a")
col2=c("NA","NA","NA","NA")
col3=c("iii","III",3,"c")
col4=c("iv","IV",4,"d")
col5=c("NA","NA","NA","NA")
col6=c("vi","VI",6,"f")
col7=c("NA","NA","NA","NA")
col8=c("viii","VIII",8,"h")
col9=NA
col10=c("x","X",10,"j")

print(is.na(as.numeric(col9[4])))

df=data.frame(col1,col2,col3,col4,col5,col6,col7,col8,col9,col10)
df

for(i in 1:nrow(df))
{
  num=c()
  for(j in 1:ncol(df))
  {
    m=0
    
    if(!is.na(as.numeric(df[i,j])))
      num=c(num,as.numeric(df[i,j]))
    
  }
  if(length(num)>0)
  {  
  m=mean(num)
  for(j in 1:ncol(df))
  {
    
    if(is.na(as.numeric((df[i,j]))))
      df[i,j]=m
  }
  
  }
}
df
