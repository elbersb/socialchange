# Plot a CR/IC decomposition

Plots cumulative IC, CR, and (where applicable) residual contributions
over time for each decomposition method, with one facet per method.

## Usage

``` r
# S3 method for class 'cr_ic_decomposition'
plot(x, total = TRUE, methods = NULL, ...)
```

## Arguments

- x:

  A \`cr_ic_decomposition\` object returned by \[cr_ic()\].

- total:

  Logical. If \`TRUE\` (default), adds a facet showing the total change
  in the outcome alongside the decomposition methods.

- methods:

  Character vector of method names to include (\`"LD"\`, \`"AD"\`,
  \`"AD+"\`, \`"Model"\`). If \`NULL\` (default), all available methods
  are shown.

- ...:

  Not used.

## Value

A \`ggplot\` object.
