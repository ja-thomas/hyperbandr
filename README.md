# hyperband in R6 

### This is a very generic R6 implementation of the hyperband algorithm for hyperparameter optimization (https://arxiv.org/pdf/1603.06560.pdf)

### The project is not yet finished but can already be used on your own problems and should work with any other R package/algorithm as long as it is suitable for hyperband.

### Check the vignette folder for a very in-depth explanation + four exhaustive examples on how to use the package.

### Furthermore, the example folder contains eight detailed examples: 

* neural networks:
    + hyperband to tune hyperparameters with mxnet and mlr
    + combine hyperband and MBO to tunehyperparameters with mxnet, mlr and mlrMBO
* gradient boosting:
    + hyperband to tune hyperparameters with xgboost and mlr
    + combine hyperband and MBO to tunehyperparameters with xgboost mlr and mlrMBO
* single- and multi-objective functions:
    + hyperband to tune hyperparameters with smoof and mlr
    + combine hyperband and MBO to tunehyperparameters with smoof mlr and mlrMBO
