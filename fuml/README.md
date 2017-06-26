## Fuml

Constructor kit for machine learning and predictive analytics in Haskell.

Fuml is a Haskell library that presents a uniform interface to four key 
tasks in machine learning:

* **Classification** - predicting binary or discrete outcomes
* **Regression** - predicting continuous outcomes
* **Clustering** - associate data points with one of many groups
* **Dimensionality reduction** - compress many dimensional data points into a smaller number of dimensions

These operations constitute the bread and butter of applied supervised 
(classification and regression) and unsupervised (clustering and 
dimensionality reduction) machine learning.
By providing a uniform interface to these operations, Fuml greatly facilitates
exploring different algorithms for specific datasets. We also provide 
functionality for feature engineering and assessing model accuracy.

Currently, Fuml only operates on continuous predictors (labels, aka outcomes, 
may be discrete or continuous or absent). There are functions to help turn 
discrete predictors into continuous predictors. This may or may not be 
appropriate for your problem. 

### Core types

All Fuml learners return a value of type `Predict p a`, which is a pair 
of two objects: the internal model state, which has type `p` and a specific to 
every learning method, and a prediction function which maps vectors of 
predictors to `a`, the type which is the predicted outcome for that 
learning method.

```haskell
data Predict p a = Predict
  { model :: p                    -- ^ the internal state of the model
  , predict :: Vector Double -> a -- ^ the prediction function
  }
```

The central type in Fuml is that of the `Supervisor`, which can 
produce `Predict` values:

```haskell
{-
                   +-------- monad in which learning runs.
                   |
                   | +------ observed outcome type
                   | | 
                   | | +---- internal model state type
                   | | | 
                   | | | +-- predicted outcome type
                   | | | |
                   v v v v                                -}
newtype Supervisor m o p a = Supervisor {
  runSupervisor 
     :: Maybe p                -- previous model state
     -> [(Vector Double, o)]   -- predictors and outcome
     -> m (Predict p a) }      -- new model state and predict function
     
```

Confused?

* `m`: the learning method runs in some monad. Often this is the 
  identity monad (m = `Identity`). If the operation is distributed
  and requires network interaction, or accessing and updating a cache, 
  `m` could be `IO`. If the method requires randomness, for instance 
  as in stochastic gradient descent, `m` could be `RVar`.
* `o`: the observed outcome for every data type. For instance, for binary 
  classification this could be `Bool`.
* `p`: the internal state of the model, for storing parameters. You may 
  need to access this to interpret the model, but it may heavily depend on
  the different learning methods. For instance, for linear regression
  this might be a vector of regression coefficients.
* `o`: the predicted outcome. Why are predicted and observed outcomes different? This allows 
  methods to return information about the uncertainty in the prediction. 
  For instance, if your observed outcome is true and false (a classifier; 
  for instance, predict whether individuals are men or women), a prediction
  of a probability is better (more informative) than a prediction of a true 
  or false value. This allows the method to communicate how confident 
  it is in the prediction. 
* `Maybe p`: if you have a previous model fit for data that is close, and the 
  method that can benefit from a close solution, you can pass this in here.
* `[(Vector Double, o)]`: lists of pairs of predictors and the observed 
  outcome. This argument is likely be more general in future versions
  of Fuml.
  
The instantiation of `o` and `a` determine what kind of machine learning 
you're doing. Here are some examples:


| Task          | `o`          | `a`          |
|---------------|--------------|-------------|
| Classification | `Bool`       | `Bool` or `Double` |
| Regression    | `Double`      | `Double` or `(Double, Double)` |
| Clustering    | `()`          | `Int` |
| Dimensionality reduction | `()`   | `Vector Double` | 
  
Within each task, there are other possibilities. Classification 
could be done on any categorical type (if multiclass) and then the outcome
might be a vector or map of probabilities of class membership. 
  
### Hyper parameters

Some methods have hyper parameters, that is parameters that cannot be learnt 
directly from the data but determine or parametrises the learning method. We
encode this as a function from the hyper parameters to the `Supervisor` value.

For instance, ridge regression depends on a hyper parameter that penalises the 
regression coefficients if they deviate from zero (also known as regularisation).

```haskell
ridge :: Double -> Supervisor Identity Double (Vector Double) Double
```
