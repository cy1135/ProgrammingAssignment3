# r programming quiz 1

mean(hw1_data$Ozone[!bad])
no_na_data <- hw1_data[!bad,]
no_na_data2 <- no_na_data[no_na_data$Ozone>31,]
no_na_data3 <- no_na_data[no_na_data$Temp>90,]
mean(no_na_data3$Solar.R)
month_data <- hw1_data[hw1_data$Month==6,]
mean(month_data$Temp)
month_data <- hw1_data[!bad,]
month_data <- month_data[hw1_data$Month==5,]
max(month_data$Ozone)