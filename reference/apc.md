# Age-Period-Cohort model estimation

Fits an Age-Period-Cohort (APC) model using orthogonal polynomial
contrasts to handle the linear identification problem. The period linear
effect is constrained to zero, allowing estimation of age and cohort
linear trends.

## Usage

``` r
apc(data, formula)
```

## Arguments

- data:

  data.frame or data.table with APC variables

- formula:

  Formula specifying `outcome ~ age + period + cohort` structure

## Value

S3 object of class `apc_model` with components:

- `model_period_zero`: fitted lm object with period linear effect set to
  zero

- `thetas`: named vector of theta parameters (age and cohort linear
  slopes)

- `contrasts`: list of orthogonal polynomial contrast matrices

- `values`: unique values for age, period, and cohort
