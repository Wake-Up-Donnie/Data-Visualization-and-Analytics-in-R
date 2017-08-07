#GT Account Name: tbobik3
#Name: Tyler Bobik
#Assignment HW2: Data Visualization
library(ggplot2)
library(dplyr)
data(midwest)
data(diamonds)

#Problem 1

#Aggregation for each state combining values percprof from each county and using popadults : Interpretation A
percprofIL = (sum(midwest$percprof[midwest$state == "IL"] * midwest$popadults[midwest$state == "IL"]) / sum(midwest$popadults[midwest$state == "IL"]))
print (percprofIL)
percprofIN = (sum(midwest$percprof[midwest$state == "IN"] * midwest$popadults[midwest$state == "IN"]) / sum(midwest$popadults[midwest$state == "IN"]))
print (percprofIN)
percprofMI = (sum(midwest$percprof[midwest$state == "MI"] * midwest$popadults[midwest$state == "MI"]) / sum(midwest$popadults[midwest$state == "MI"]))
print (percprofMI)
percprofOH = (sum(midwest$percprof[midwest$state == "OH"] * midwest$popadults[midwest$state == "OH"]) / sum(midwest$popadults[midwest$state == "OH"]))
print (percprofOH)
percprofWI = (sum(midwest$percprof[midwest$state == "WI"] * midwest$popadults[midwest$state == "WI"]) / sum(midwest$popadults[midwest$state == "WI"]))
print (percprofWI)


#Putting Results into a vector
Percentage_Total_Pop_Prof_Employ_All_States = c(percprofWI,
                                                percprofOH,
                                                percprofIN,
                                                percprofMI,
                                                percprofIL)
#Putting labels into a vector
state_Labels = c("WI", "OH", "IN", "MI", "IL")

#Barplot of percprof by state using above data
barplot(unlist(Percentage_Total_Pop_Prof_Employ_All_States), ylim=c(0,1.1*max(unlist(Percentage_Total_Pop_Prof_Employ_All_States))), names.arg = state_Labels, main = "percprof by state", ylab = "Percent Professional Employment by State", xlab="State")

#printing results
print (percprofIL)
print (percprofMI)
print (percprofIN)
print (percprofOH)
print (percprofWI)


#Problem #2
#Aggregation for each state combining perchsd values from each county and using popadults : Interpretation A
perchsdIL = (sum(midwest$perchsd[midwest$state == "IL"] * midwest$popadults[midwest$state == "IL"]) / sum(midwest$popadults[midwest$state == "IL"]))
perchsdIN = (sum(midwest$perchsd[midwest$state == "IN"] * midwest$popadults[midwest$state == "IN"]) / sum(midwest$popadults[midwest$state == "IN"]))
perchsdMI = (sum(midwest$perchsd[midwest$state == "MI"] * midwest$popadults[midwest$state == "MI"]) / sum(midwest$popadults[midwest$state == "MI"]))
perchsdOH = (sum(midwest$perchsd[midwest$state == "OH"] * midwest$popadults[midwest$state == "OH"]) / sum(midwest$popadults[midwest$state == "OH"]))
perchsdWI = (sum(midwest$perchsd[midwest$state == "WI"] * midwest$popadults[midwest$state == "WI"]) / sum(midwest$popadults[midwest$state == "WI"]))

#Putting Results into a vector
Percentage_Total_Pop_Pershsd_All_States = c(perchsdIN,
                                            perchsdOH,
                                            perchsdIL,
                                            perchsdMI,
                                            perchsdWI)
#Putting labels into a vector
state_Labels = c("IN", "OH", "IL", "MI", "WI")

#Barplot of pershsd by state using above data
barplot(unlist(Percentage_Total_Pop_Pershsd_All_States), ylim=c(0,1.1*max(unlist(Percentage_Total_Pop_Pershsd_All_States))), names.arg = state_Labels, main = "Pershsd by state", ylab = "Percent Population with High School Diploma by State", xlab="State")

#Aggregation for each state combining percollege values from each county and using popadults : Interpretation A
percollegeIL = (sum(midwest$percollege[midwest$state == "IL"] * midwest$popadults[midwest$state == "IL"]) / sum(midwest$popadults[midwest$state == "IL"]))
percollegeIN = (sum(midwest$percollege[midwest$state == "IN"] * midwest$popadults[midwest$state == "IN"]) / sum(midwest$popadults[midwest$state == "IN"]))
percollegeMI = (sum(midwest$percollege[midwest$state == "MI"] * midwest$popadults[midwest$state == "MI"]) / sum(midwest$popadults[midwest$state == "MI"]))
percollegeOH = (sum(midwest$percollege[midwest$state == "OH"] * midwest$popadults[midwest$state == "OH"]) / sum(midwest$popadults[midwest$state == "OH"]))
percollegeWI = (sum(midwest$percollege[midwest$state == "WI"] * midwest$popadults[midwest$state == "WI"]) / sum(midwest$popadults[midwest$state == "WI"]))

#Putting Results into a vector
Percentage_Total_Pop_Percollege_All_States = c(percollegeIN,
                                               percollegeOH,
                                               percollegeMI,
                                               percollegeWI,
                                               percollegeIL)
#Putting labels into a vector
state_Labels = c("IN", "OH", "MI", "WI", "IL")

#Barplot of percollege by state using above data
barplot(unlist(Percentage_Total_Pop_Percollege_All_States), ylim=c(0,1.1*max(unlist(Percentage_Total_Pop_Percollege_All_States))), names.arg = state_Labels, main = "Percollege by state", ylab = "Percent Population with College Degree by State", xlab="State")

#Qplot for perchsd and precollege by county in each state: Interpretation B
qplot(x = perchsd,
      y = percollege,
      facets = .~state,
      data = midwest,
      main = "percollege vs. perchsd")  +
        stat_smooth(se = FALSE)

#Problem #4
sizes = (seq(1000,125000,5000))

#Generate and save Jpeg images with N randomly uniformly distributed values
size_jpeg = c()
i=1
for (n in sizes) {
        
        DF1 = data.frame(runif(n),runif(n))
        plot_n = ggplot(DF1, aes(x = runif(n), y = runif(n))) + geom_point()
        file_n = paste("file1", i, ".jpeg", sep = "")
        ggsave(file_n, plot_n)
        file_list=file.info(file_n)
        size_jpeg[i] = file_list[1]
        file.remove(file_n)
        # file.remove('jpeg_pics/rplot', i, ".jpeg", sep = "")
        i = i + 1
}
#print (size_jpeg)

#Generate and save Ps images with N randomly uniformly distributed values
size_ps = c()
for (n in sizes) {
        DF1 = data.frame(runif(n),runif(n))
        plot_n = ggplot(DF1, aes(x = runif(n), y = runif(n))) + geom_point()
        file_n = paste("file1", i, ".ps", sep = "")
        ggsave(file_n, plot_n)
        file_list=file.info(file_n)
        size_ps[i] = file_list[1]
        file.remove(file_n)
        # file.remove('jpeg_pics/rplot', i, ".jpeg", sep = "")
        i = i + 1
}

#Generate and save pdf images with N randomly uniformly distributed values
size_pdf = c()
for (n in sizes) {
        DF1 = data.frame(runif(n),runif(n))
        plot_n = ggplot(DF1, aes(x = runif(n), y = runif(n))) + geom_point()
        file_n = paste("file1", i, ".pdf", sep = "")
        ggsave(file_n, plot_n)
        file_list=file.info(file_n)
        size_pdf[i] = file_list[1]
        file.remove(file_n)
        # file.remove('jpeg_pics/rplot', i, ".jpeg", sep = "")
        i = i + 1
}

#Generate and save PNG images with N randomly uniformly distributed values
size_png = c()
for (n in sizes) {
        DF1 = data.frame(runif(n),runif(n))
        plot_n = ggplot(DF1, aes(x = runif(n), y = runif(n))) + geom_point()
        file_n = paste("file1", i, ".png", sep = "")
        ggsave(file_n, plot_n)
        file_list=file.info(file_n)
        size_png[i] = file_list[1]
        file.remove(file_n)
        # file.remove('jpeg_pics/rplot', i, ".jpeg", sep = "")
        i = i + 1
}

#Put sizes in a vector
size_jpeg = as.vector(size_jpeg)
size_ps = as.vector(size_ps)
size_pdf = as.vector(size_pdf)
size_png = as.vector(size_png)

#Put store the sequences for each file type
sizes_jpeg = (seq(1000,125000,5000))
sizes_ps = (seq(1000,125000,5000))
sizes_pdf = (seq(1000,125000,5000))
sizes_png = (seq(1000,125000,5000))
n_randomly_uniformly_generated_points = c(sizes_jpeg, sizes_ps, sizes_pdf, sizes_png)

#create with the vector generated above
file_Size_and_Type_Df = stack(list(jpeg = unlist(size_jpeg), ps = unlist(size_ps), pdf = unlist(size_pdf), png = unlist(size_png)))

names(file_Size_and_Type_Df)[1] = "File_size_in_bytes"

#Plot all file sizes against the type of file
qplot(x = n_randomly_uniformly_generated_points,
      y = File_size_in_bytes,
      facets = ind~.,
      data = file_Size_and_Type_Df,
      geom = "line",
      main = "n randomly uniformly generated points vs. file size in bytes") 


#Problem 5
DF = diamonds[, c("carat", "price", "color")]

#Price and Color Distribution
ggplot(diamonds,aes(x=price, fill = color)) +
        geom_histogram(binwidth=100) +
        ggtitle('Price and Color Distribution')

ggplot(diamonds,aes(x=price, fill = color)) +
        geom_histogram(binwidth=.025) +
        scale_x_log10()+
        ggtitle('Price (log_10) and Color Distribution')

#Carat and Color Distribution
qplot(carat, data = diamonds, facets = color ~ .,
      geom = "histogram", binwidth = 0.1, xlim = c(0, 3))

ggplot(diamonds,aes(x=carat)) +
        geom_histogram(binwidth=.025) +
        ggtitle('Carat Distribution')

ggplot(diamonds,aes(x=carat, fill = color)) +
        geom_histogram(binwidth=.050) +
        ggtitle('Carat and Color Distribution') 

#Price Distribution Log 10
qplot(data = diamonds, x = price, binwidth = .01) + ggtitle("Price log_10") + scale_x_log10()

#Price per carat
ggplot(data=diamonds) + geom_histogram(binwidth=.025, aes(x=diamonds$price/diamonds$carat)) + 
        ggtitle("Diamond Price (log_10) per Carat Distribution by Color") +
        xlab("Diamond Price per Carat ($) - log_10_Scale") + 
        ylab("Count") +
        facet_wrap(~color) + 
        scale_x_log10()


#Pairwise plots
ggpairs(DF, 1:3, columnLabels = c("carat", "price", "color")) 


ggplot(aes(x = carat, y = price, color = color), data = diamonds) + 
        geom_point() + 
        scale_y_log10() +
        ggtitle("Price log_10 vs Carat and Color")

