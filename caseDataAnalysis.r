

library(stargazer)

dataFrame <- read.csv(file="/Users/NicholasBocchini/Documents/Bocchini_Case_Data.csv", header=TRUE, sep=",")

dataFrame$Diff_Contrast <- round(dataFrame$Contrast_B - dataFrame$Contrast_A, 3)
dataFrame$Diff_LDF <- round(dataFrame$LDF_B - dataFrame$LDF_A, 3)
dataFrame$Diff_HDF <- round(dataFrame$HDF_B - dataFrame$HDF_A, 3)
dataFrame$Diff_TotalF <- round(dataFrame$TotalF_B - dataFrame$TotalF_A, 3)


# variable basic stats

# Diff variable basic stats
# Contrast A and B stats
Contrast_A <- c(round(mean(dataFrame$Contrast_A), 3)
				 ,round(sd(dataFrame$Contrast_A), 3)
				 ,round(min(dataFrame$Contrast_A), 3)
				 ,round(max(dataFrame$Contrast_A), 3)
				 ,round(median(dataFrame$Contrast_A), 3)
				 )
Contrast_B <- c(round(mean(dataFrame$Contrast_B), 3)
				 ,round(sd(dataFrame$Contrast_B), 3)
				 ,round(min(dataFrame$Contrast_B), 3)
				 ,round(max(dataFrame$Contrast_B), 3)
				 ,round(median(dataFrame$Contrast_B), 3)
				 )
				 
# LDF A and B stats
LDF_A <- c(round(mean(dataFrame$LDF_A), 3)
				 ,round(sd(dataFrame$LDF_A), 3)
				 ,round(min(dataFrame$LDF_A), 3)
				 ,round(max(dataFrame$LDF_A), 3)
				 ,round(median(dataFrame$LDF_A), 3)
				 )
LDF_B <- c(round(mean(dataFrame$LDF_B), 3)
				 ,round(sd(dataFrame$LDF_B), 3)
				 ,round(min(dataFrame$LDF_B), 3)
				 ,round(max(dataFrame$LDF_B), 3)
				 ,round(median(dataFrame$LDF_B), 3)
				 )
				 
# HDF A and B stats
HDF_A <- c(round(mean(dataFrame$HDF_A), 3)
				 ,round(sd(dataFrame$HDF_A), 3)
				 ,round(min(dataFrame$HDF_A), 3)
				 ,round(max(dataFrame$HDF_A), 3)
				 ,round(median(dataFrame$HDF_A), 3)
				 )
HDF_B <- c(round(mean(dataFrame$HDF_B), 3)
				 ,round(sd(dataFrame$HDF_B), 3)
				 ,round(min(dataFrame$HDF_B), 3)
				 ,round(max(dataFrame$HDF_B), 3)
				 ,round(median(dataFrame$HDF_B), 3)
				 )
				 
# Total F A and B stats
TotalF_A <- c(round(mean(dataFrame$TotalF_A), 3)
				 ,round(sd(dataFrame$TotalF_A), 3)
				 ,round(min(dataFrame$TotalF_A), 3)
				 ,round(max(dataFrame$TotalF_A), 3)
				 ,round(median(dataFrame$TotalF_A), 3)
				 )
TotalF_B <- c(round(mean(dataFrame$TotalF_B), 3)
				 ,round(sd(dataFrame$TotalF_B), 3)
				 ,round(min(dataFrame$TotalF_B), 3)
				 ,round(max(dataFrame$TotalF_B), 3)
				 ,round(median(dataFrame$TotalF_B), 3)
				 )

statDataFrame = data.frame(Contrast_A, Contrast_B, LDF_A, LDF_B, HDF_A, HDF_B, TotalF_A, TotalF_B)	
rownames(statDataFrame) = c('Mean', 'StandardDeviation', 'Minimum', 'Maximum', 'Median')	
#output table
stargazer(statDataFrame, type="text", summary=FALSE, header=FALSE)

#Diff variable stats			 				 	
contrastDiff <- c(round(mean(dataFrame$Diff_LDF), 3)
				 ,round(sd(dataFrame$Diff_LDF), 3)
				 ,round(min(dataFrame$Diff_LDF), 3)
				 ,round(max(dataFrame$Diff_LDF), 3)
				 ,round(median(dataFrame$Diff_LDF), 3)
				 )	 				 				 
LDFDiff 	<- c(round(mean(dataFrame$Diff_LDF), 3)
				 ,round(sd(dataFrame$Diff_LDF), 3)
				 ,round(min(dataFrame$Diff_LDF), 3)
				 ,round(max(dataFrame$Diff_LDF), 3)
				 ,round(median(dataFrame$Diff_LDF), 3)
				 )
HDFDiff 	<- c(round(mean(dataFrame$Diff_HDF), 3)
				 ,round(sd(dataFrame$Diff_HDF), 3)
				 ,round(min(dataFrame$Diff_HDF), 3)
				 ,round(max(dataFrame$Diff_HDF), 3)
				 ,round(median(dataFrame$Diff_HDF), 3)
				 )
TotalFDiff 	<- c(round(mean(dataFrame$Diff_TotalF), 3)
				 ,round(sd(dataFrame$Diff_TotalF), 3)
				 ,round(min(dataFrame$Diff_TotalF), 3)
				 ,round(max(dataFrame$Diff_TotalF), 3)
				 ,round(median(dataFrame$Diff_TotalF), 3)
				 )
				 
statDiffDataFrame <- data.frame(contrastDiff, LDFDiff, HDFDiff, TotalFDiff)
rownames(statDiffDataFrame) <- c('Mean', 'StandardDeviation', 'Minimum', 'Maximum', 'Median')
#output table
stargazer(statDiffDataFrame, type="text", summary=FALSE, header=FALSE)

#Perform Wilcoxon signed-rank tests for dependent variables on all data
#Contrast amount
testContrast <- wilcox.test(dataFrame$Contrast_A, dataFrame$Contrast_B, paired=TRUE)

#LDF amount
testLDF <- wilcox.test(dataFrame$LDF_A, dataFrame$LDF_B, paired=TRUE)

#HDF amount
testHDF <- wilcox.test(dataFrame$HDF_A, dataFrame$HDF_B, paired=TRUE)

#Total F amount
testTotalF <- wilcox.test(dataFrame$TotalF_A, dataFrame$TotalF_B, paired=TRUE)

#Prepare new data frames to run the tests again with outliers removed
#Find outliers for Contrast variables
outlierValuesContrastA <- boxplot.stats(dataFrame$Contrast_A)$out
outlierValuesContrastB <- boxplot.stats(dataFrame$Contrast_B)$out
#ID rows with Contrast outliers
dataFrame$ContrastOutlier <- ifelse(dataFrame$Contrast_A %in% outlierValuesContrastA == TRUE, 1,
									 ifelse(dataFrame$Contrast_B %in% outlierValuesContrastB == TRUE, 1, 0))
#create new dataframe without outliers									 
dataFrameNoOutContrast <- subset(dataFrame, ContrastOutlier == 0)

#Find outliers for LDF variables
outlierValuesLDFA <- boxplot.stats(dataFrame$LDF_A)$out
outlierValuesLDFB <- boxplot.stats(dataFrame$LDF_B)$out
#ID rows with LDF outliers
dataFrame$LDFOutlier <- ifelse(dataFrame$LDF_A %in% outlierValuesLDFA == TRUE, 1,
									 ifelse(dataFrame$LDF_B %in% outlierValuesLDFB == TRUE, 1, 0))
#create new dataframe without outliers
dataFrameNoOutLDF <- subset(dataFrame, LDFOutlier == 0)

#Find outliers for LDF variables
outlierValuesHDFA <- boxplot.stats(dataFrame$HDF_A)$out
outlierValuesHDFB <- boxplot.stats(dataFrame$HDF_B)$out
#ID rows with LDF outliers
dataFrame$HDFOutlier <- ifelse(dataFrame$HDF_A %in% outlierValuesHDFA == TRUE, 1,
									 ifelse(dataFrame$HDF_B %in% outlierValuesHDFB == TRUE, 1, 0))
#create new dataframe without outliers
dataFrameNoOutHDF <- subset(dataFrame, HDFOutlier == 0)

#Find outliers for TotalF variables
outlierValuesTotalFA <- boxplot.stats(dataFrame$TotalF_A)$out
outlierValuesTotalFB <- boxplot.stats(dataFrame$TotalF_B)$out
#ID rows with LDF outliers
dataFrame$TotalFOutlier <- ifelse(dataFrame$TotalF_A %in% outlierValuesTotalFA == TRUE, 1,
									 ifelse(dataFrame$TotalF_B %in% outlierValuesTotalFB == TRUE, 1, 0))
#create new dataframe without outliers
dataFrameNoOutTotalF <- subset(dataFrame, TotalFOutlier == 0)

#Perform Wilcoxon signed-rank tests for dependent variables on no outlier data
#Contrast amount no outliers
contrastOutput <- subset(dataFrame, ContrastOutlier==1, select = c(PatientID,Contrast_A, Contrast_B, ContrastOutlier))
stargazer(contrastOutput, type="text", summary=FALSE)

testContrastNoOut <- wilcox.test(dataFrameNoOutContrast$Contrast_A, dataFrameNoOutContrast$Contrast_B, paired=TRUE)

#LDF amount
LDFOutput <- subset(dataFrame, LDFOutlier==1, select = c(PatientID,LDF_A, LDF_B, LDFOutlier))
stargazer(LDFOutput, type="text", summary=FALSE)

testLDFNoOut <- wilcox.test(dataFrameNoOutLDF$LDF_A, dataFrameNoOutLDF$LDF_B, paired=TRUE)

#HDF amount
testHDFNoOut <- wilcox.test(dataFrameNoOutHDF$HDF_A, dataFrameNoOutHDF$HDF_B, paired=TRUE)

#Total F amount
testTotalFNoOut <- wilcox.test(dataFrameNoOutTotalF$TotalF_A, dataFrameNoOutTotalF$TotalF_B, paired=TRUE)


