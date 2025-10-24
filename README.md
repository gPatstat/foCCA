### 🚀 Functional-Ordinal Canonical Correlation Analysis (foCCA)
FoCCA performs dimensionality reduction on observable features while maximizing their capacity to distinguish between successive levels of an ordinal target variable. Unlike many other methods, FoCCA operates entirely in closed form and does not rely on numerical optimization, which ensures both computational efficiency and global optimality. By embedding the ordinal target within the Guttman space, FoCCA fully captures its ordered structure—allowing the model to represent the relative dissimilarities between adjacent target levels and to explain these variations through the underlying functional features.

#### 🧪 Usage
⚙️ Getting Started: git clone [https://github.com/your-username/project-name.git](https://github.com/gPatstat/foCCA.git)

⚙️ Commands for importing the useful functions in R:

source("foCCA.fd.R") <br>
source("foCCA.scores.R") <br>
source("foCCA.CV.R") <br>
source("foCCA.confmat.R")<br>

-foCCA.fd() : it performs foCCA on a set of functions, given their corresponding levels.<br>
-foCCA.scores() : computes the scores for new functions, given a trained foCCA dimensionality reduction, namely once selected the canonical directions from a training set.<br>
-foCCA.CV(): it performs k-fold cross validation to select the smoothing penalization parameters (λ1 and λ2).<br>
-foCCA.confmat(): it computes the k-fold confusion matrix using the trained foCCA scores and the _nearest centroid classifier_.<br>

📖 Analogous functions for the state-of-the-art dimensionality reduction methods:

source("pca_scores.R")<br>
source("pca.CV.R")<br>
source("pca.confmat.R")<br>
source("foFD.fd.R")<br>
source("foFD.scores.R")<br>
source("foFD.CV.R")<br>
source("foFD.confmat.R")<br>

🔍 K-fold CV Mean Absolute Error (MAE)

source("MAE_calculator.R")

- MAE: given a functional-ordinal dataset it computes the k-fold CV MAE for foCCA, fPCA, foFD, foLR and NCCA.
  
🔬 Simulations 

source("scenarioA.R")<br>
source("scenarioB.R")<br>

**Scenario A**: the lower $ratio$, the lower the expected ratio between the variance explained by the ordinal variable and the global variance. Lower ratio-> higher difficulty.

**Scenario B**: the lower $ratio$, the less uniform separation between consecutive levels, illustrating greater disparity in the increments for higher levels compared to lower levels. Lower ratio-> higher difficulty.

💻 Run Simulation_study.R to see the comparison and to have an example of usage.


