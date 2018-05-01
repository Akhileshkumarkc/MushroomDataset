###################################################################
# Final Project Part 2: Mushroom Classigication.
# Member: 
# 1. Akhilesh Kumar Kagalvadi Chinnaswamy (axk167131)
# 2. Shalin Anilkumar Amin (saa161030)
# 3. Saikrishna Kanukuntla (sxk160432)
# 4. Nanditha Valsaraj (nxv160930)
##################################################################
# Mushroom Dataset contains 24 attribute, with target attribute class.
# This is carried out in following steps:
# Step 1: Read the dataset.
# Step 2: Clean the dataset.
# Step 3 : DO SVM
# step 4: Do Random Forest.
# Package: attach - e1071, random forest package.
###################
# Step 1: Read the dataset.
#Read in the mushroom dataset f
mushrooms = agaricus.lepiota.data
col_names = c("class","cap.shape","cap.surface","cap.color","bruises","odor"
              ,"gill.attachment","gill.spacing","gill.size", "gill.color","stalk.shape"
              ,"stalk.root","stalk.surface.above.ring","stalk.surface.below.ring" ,"stalk.color.above.ring","stalk.color.below.rin"
              ,"veil.type","veil.color","ring.number","ring.type","spore.printcolor"       
              ,"population","habitat")
colnames(mushrooms) = col_names
myData = mushrooms
summary(myData)

# Step 2: Clean the dataset.

#We can see that we have all the attributes as Factors i.e categorical.
#Out of all the attributes, veil.type has only one value, it can be ignored.
myData$veil.type = NULL
#since it is a classification problem with 2 values we can use SVM.
dim(myData)
#[1] 8124   23
#Lets make a random sample of 75% of the data as train set.
#have to attach package e1071
train.svm = sample(8124, 8124 * 0.75)

# Step 3 : DO SVM

#creating a model using SVM
mush.svm = svm(class~. , data = myData[train.svm,], kernel = "linear", cost = 1)
mush.svm.pred = predict(mush.svm, myData[-train.svm,])
table(mush.svm.pred, myData[-train.svm,] $class)

#mush.svm.pred    e    p
#             e 1035    0
#             p    0  996

#SVM gives a 100% aacuracy on the dataset.

# step 4: Do Random Forest.

#Let try it out with random forest as it is a more of categorical classification.
#load random forest package.
train.rf = sample( 8124, 8124 * 0.75)
mush.rf = randomForest(class~. , data = myData  , subset = train.rf)
mush.rf
#
#Call:
#  randomForest(formula = class ~ ., data = myData, subset = train.rf) 
#Type of random forest: classification
#Number of trees: 500
#No. of variables tried at each split: 4
#
#OOB estimate of  error rate: 0%
#Confusion matrix:
#      e      p     class.error
# e   3096    0           0
# p    0    2904          0 
#No errots while on in the train set.
mush.rf.pred = predict(mush.rf, myData[-train.rf,])
table(mush.rf.pred, myData[-train.rf,] $class)
#
# mush.rf.pred    e        p
#              e 1112      0
#              p    0    1012
#Random Forest also gives 100% accuracy.
#Upon trying both the methods, we can say we have successfully classified mushroom dataset with 100% accuracy. 
