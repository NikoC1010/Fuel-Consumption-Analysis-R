


#Build a function
fun2<-function(x){
  name1<-paste0('Car',as.character(x),'.csv')
  return(name1)
}

#Extract file data
yv<-c()
for (i in 1:6){
  y<-ifelse(i!=5,i,next)
  yv<-append(yv,y)
  if (length(yv)==5){
    yv<-matrix(yv)
    yv<-t(yv)
    file_name<-apply(yv,2,fun2)
    for (h in 1:5){
      j<-1
      if (h == 1){
        data_car1<-read.csv(file_name[h])
      }else{
        while(j==1){
          data_car2<-read.csv(file_name[h])
          data_car1<-cbind(data_car1,data_car2)
          j<-2
        }
      } 
     }
   }
}
c_data<-data.frame(data_car1)


#View data variables using the Datatable
library(DT)
datatable(c_data)


#Calculate 5 number summary
summary(c_data$highwaympg)

#Plot the frequency histogram of fuel consumption
Fuel_consumption<-c(c_data$highwaympg)
hist(Fuel_consumption,
     xlab='Fuel consumption',
     col='green',
     border='red',
     xlim = c(0,60))

#Plot the Line Chart of the engine size
Engine_size<-c(c_data$enginesize)
plot(Engine_size,
     type="o",
     main="Line Chart of the engine size") 

#Plot the Bar Chart of the fuel system
freq_system<-table(c_data$fuelsystem)
barplot(freq_system,
        ylab='Frequency',
        main='Bar chart of fuel system')

#Plot the Bar Chart of the fuel type
freq_type<-table(c_data$fueltype)
barplot(freq_type,
        ylab='Frequency',
        main='Bar chart of fuel type')

#Regress highwaympg against engine size. 
regression_modle<-lm(highwaympg~enginesize,data = c_data)
summary(regression_modle)
plot(highwaympg~enginesize,data = c_data)
abline(regression_modle)


#Perform a two-sample test of highwaympg across fueltype
highwaympg_ttest<-t.test(highwaympg~fueltype,data = c_data)
highwaympg_mwutest<-wilcox.test(highwaympg~fueltype,data = c_data)
highwaympg_ttest
highwaympg_mwutest

#Boxplot
data(c_data)
boxplot(highwaympg~fueltype,
        data = c_data,
        xlab = 'Fuel type',
        ylab = 'Fuel consumption',
        main = 'Car Data',
        notch = TRUE,
        varwidth = TRUE,
        col = c('green','red'))



