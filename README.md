### 🚀 Functional-Ordinal Canonical Correlation Analysis (foCCA)
FoCCA performs dimensionality reduction on observable features while maximizing their capacity to distinguish between successive levels of an ordinal target variable. Unlike many other methods, FoCCA operates entirely in closed form and does not rely on numerical optimization, which ensures both computational efficiency and global optimality. By embedding the ordinal target within the Guttman space, FoCCA fully captures its ordered structure—allowing the model to represent the relative dissimilarities between adjacent target levels and to explain these variations through the underlying functional features.

##### 🧪 Usage
⚙️ Getting Started: git clone [https://github.com/your-username/project-name.git](https://github.com/gPatstat/foCCA.git)

⚙️ Commands for importing the useful functions in R:
source("foCCA.fd.R") 
source("foCCA.scores.R") 
source("foCCA.CV.R") 
source("foCCA.confmat.R")

foCCA.fd() : it performs foCCA on a set of functions, given their corresponding levels.
foCCA.scores() : computes the scores for new functions, given a trained foCCA dimensionality reduction, namely once selected the canonical directions from a training set.
foCCA.CV(): it performs k-fold cross validation to select the smoothing penalization parameters (λ1 and λ2).
foCCA.confmat(): it computes the leave k-fold out confusion matrix using the trained foCCA scores and the $nearest centroid classifier$.
