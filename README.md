<a id="top"></a>

# DUST

### Simon QUERNE
#### LaMME, Evry University
### July 2, 2024

___ 

> [Introduction](#intro)

> [Quickstart](#quickstart)

> [Pruning Capacity](#pruning)

___ 

<a id="intro"></a>

## Introduction

The `dust` package contains methods for detecting multiple change-points within time-series based on the optimal partitioning algorithm with pruning. 

The proposed algorithm is a pruned dynamic programming algorithm optimizing a penalized likelihood **using a new pruning rule**, different from PELT or FPOP. The **DUST** pruning rule, standing for **Du**ality **S**imple **T**est, filters indices for potential last change-point by considering some constrained optimization problem. For each potential last change-point index, evaluating its associated dual's maximum value enables a fast and efficient pruning test.

[Back to Top](#top)

___ 

<a id="quickstart"></a>


## Quickstart


### Data generation

The package contains the function **generate_random_changepoints** which generates a gaussian data vector that contains random changepoints associated with random shift values. It takes input `n` the size of the output vector, `K` the number of changepoints, `mu` the main gaussian mean (i.e. the baseline mean of the data), `data_variance` the piecewise variance of each segment, `changepoint_variance` the variance of the changepoint generator (controls the size of the changepoint 'jumps'). The function outputs a list containing `data` the generated data vector, `true_cp` the true location of the changepoints, `true_means` the true underlying parameter of each generated segment.


### Simple changepoint computation

The main function of the package is the **DUSTv** function. It applies the **DUST algorithm** to the input `data` vector with optional parameter `penalty`. If no `penalty` is passed, the function computes the **MAD estimator**, a robust deviation estimator, in linear time. The **DUSTv** function efficiently computes the **exact OP solution** and outputs a list with items `changepoints` the location of the changepoints in the optimal segmenation of the data, `lastIndexSet` the indices that were never pruned throughout the execution of **DUSTv**, `costQ` the cost of the optimal segmentation at each time step.


### Examples

The following code generates gaussian white noise data with no changepoints and applies the **DUSTv** function to it.

```
n = 1e3
data = rnorm(n)
out = DUSTv(data)
out$changepoints
```

The following code generates guassian data with random changepoints and applies the **DUSTv** function to it.

```
n = 1e3
data = generate_random_changepoints(n = n, K = n/50, mu = 0, data_variance = 1, changepoint_variance = 1)$data
out = DUSTv(data)
out$changepoints
```
                            
                        
[Back to Top](#top)


<a id="pruning"></a>

___ 

## Pruning Capacity

The **DUST algorithm** has strong pruning capacity on **gaussian 1D data**. 


[Back to Top](#top)

