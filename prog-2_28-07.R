

id=c(1,2,3)
name=c("ABC","EFG","XYZ")
salary=c(20000,35000,60000)
joining_date=c("12/8/2025","13/8/2025","14/8/2025")
dept=c("IT","IT","ECE")

df=data.frame(id,name,salary,joining_date,dept)

range_salary=df[which(df$salary>=30000 & df$salary<=40000),]
range_salary

employ_name=df$name[which(df$salary==max(df$salary))]
employ_name

all_emp_it=df$name[which(df$dept=="IT")]
all_emp_it=df[which(df$dept=="IT"),2]
all_emp_it

