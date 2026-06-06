# Contour plot of a 2D GAM smooth surface

Visualizes the predicted surface of a GAM with a 2D smooth term as a
filled contour plot, overlaid with observed data density.

## Usage

``` r
plot_gam_surface(model, smooth = 1, resolution = 1)
```

## Arguments

- model:

  A fitted [`mgcv::gam`](https://rdrr.io/pkg/mgcv/man/gam.html) object
  with at least one 2D smooth term.

- smooth:

  Integer selecting which smooth term to visualize (default: 1).

- resolution:

  Numeric step size for the prediction grid (default: 1).

## Value

A ggplot object.
