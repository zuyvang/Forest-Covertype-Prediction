#this line will not work, please manually import data at environment tab
covtype <- read.csv("C:/Users/nguye/OneDrive/Máy tính/CSC 3220/covertype/covtype.data", header=FALSE)

#shape of dataframe
cat("The data frame has", nrow(covtype), "rows and", ncol(covtype), "columns.")


#manually setting the column names
column_names <- c("Elevation", "Aspect", "Slope", "Horizontal_Distance_To_Hydrology", "Vertical_Distance_To_Hydrology", "Horizontal_Distance_To_Roadways", "Hillshade_9am", "Hillshade_Noon", "Hillshade_3pm", "Horizontal_Distance_To_Firepoints", "Wilderness_Area1", "Soil_Type1", "Soil_Type2","Soil_Type3","Soil_Type4","Soil_Type5","Soil_Type6","Soil_Type7","Soil_Type8","Soil_Type9","Soil_Type10","Soil_Type11","Soil_Type12","Soil_Type13","Soil_Type14","Soil_Type15","Soil_Type16","Soil_Type17","Soil_Type18","Soil_Type19","Soil_Type20","Soil_Type21","Soil_Type22","Soil_Type23","Soil_Type24","Soil_Type25","Soil_Type26","Soil_Type27","Soil_Type28","Soil_Type29","Soil_Type30","Soil_Type31","Soil_Type32","Soil_Type33","Soil_Type34","Soil_Type35","Soil_Type36","Soil_Type37","Soil_Type38","Soil_Type39","Soil_Type40","Wilderness_Area2","Wilderness_Area3","Wilderness_Area4", "Cover_Type")
colnames(covtype) <- column_names
#show column names
colnames(covtype)

#number of unique values of each features
unique_counts <- sapply(covtype, function(x) length(unique(x)))
print(unique_counts)


##################Histogram of non binary#######################
par(mfrow = c(3, 4), mar = c(4, 4, 2, 1)) # create grid frame

# Loop through the first 11 columns and plot histograms
for (i in 1:10) {
  hist(covtype[[i]], main = paste("Histogram of", names(covtype)[i]), xlab = names(covtype)[i])
}


########################Box plots of non binary #######################
par(mfrow = c(3, 4), mar = c(4, 4, 2, 1)) # create grid frame

# Loop through the first 11 columns and plot box plot
for (i in 1:10) {
  boxplot(covtype[[i]], main = paste("Boxplot of", names(covtype)[i]), xlab = names(covtype)[i])
}


###############################SOILTYPE##############################
soil_type_col <- covtype[, 12:51]

count_ones <- sapply(soil_type_col, function(x) sum(x == 1))

# Create the bar plot
bar_positions <- barplot(count_ones, names.arg = names(soil_type_col), col = "blue",
                         main = "Soil_type Frequency",
                         xlab = "Columns", ylab = "Frequency", las = 2)  # las = 2 for vertical x-axis labels

# Add the count values on top of each bar
text(bar_positions, count_ones, labels = count_ones, pos = 3, cex = 0.8)  # pos = 3 places text above the bars


############WILDERNESS AREA#################
# Select columns 11 and 52 to 54
WILDERNESS <- covtype[, c(11, 52:54)]

# Count the number of 1's in each of the selected columns
count_ones <- sapply(WILDERNESS, function(x) sum(x == 1))

#names(count_ones) <- c("Wilderness_Area1", "Wilderness_Area2", "Wilderness_Area3", "Wilderness_Area4")

bar_positions <- barplot(count_ones, names.arg = names(count_ones), col = "blue",
                         main = "Wilderness Area Frequency",
                         xlab = "Columns", ylab = "Frequency", las = 2)  # las = 2 for vertical x-axis labels

# add actual value
text(bar_positions, count_ones, labels = count_ones, pos = 3, cex = 0.8)


#####################Covertype against Elavation#############################
boxplot(Soil_Type3~Cover_Type, data=covtype, main="Cover Type According to Elevation", xlab="Cover Type", ylab="Elevation")


#################COVERETYPE AGAINST SOILTYPES#############
par(mfrow = c(5, 8), mar = c(3, 3, 2, 1))  # Adjust margins if needed

# Loop through columns Soil_Type1 to Soil_Type40
for (i in 12:51) {  # Assuming Soil_Type columns are from 12 to 51
  boxplot(covtype[[i]] ~ covtype$Cover_Type, 
          main = paste("Soil_Type", i - 11),  # Adjust title based on the column index
          xlab = "Cover Type", ylab = "Soil Type")
}


###############HEATMAP################
# Install required packages if necessary
# install.packages("ggplot2")
# install.packages("reshape2")
# install.packages("RColorBrewer")
# Install required packages if necessary
# install.packages("ggplot2")
# install.packages("RColorBrewer")
# Standardize the data (mean=0, sd=1)


library(ggplot2)
library(RColorBrewer)

# Compute the correlation matrix for numeric columns only
cor_matrix <- cor(covtype[, sapply(covtype, is.numeric)])

# Manually convert the correlation matrix to long format
cor_matrix_melted <- as.data.frame(as.table(cor_matrix))

# Rename the columns for clarity
colnames(cor_matrix_melted) <- c("Var1", "Var2", "Correlation")

# Plot the heatmap using ggplot2
ggplot(cor_matrix_melted, aes(Var1, Var2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradientn(colors = rev(brewer.pal(9, "RdBu"))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Correlation Matrix Heatmap")

########SCALE###########
covtype[, 1:10] <- scale(covtype[, 1:10])

