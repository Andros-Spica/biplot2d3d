# biplot2d3d: creation of customizable biplots, both in two and three dimensions

R package combining functionalities of other packages to create highly-customizable biplots. 

Tested in Windows 7 and Mac OS X 10.9.5. 

Examples in the documentation were callibrated in Windows 7, thus some aspects may be uglier than intended.

Dependencies (packages you must install before installing biplot2d3d):
* ade4,
* heplots,
* rgl 0.98.1 (Mac users must download the latest version from https://cran.r-project.org/web/packages/rgl/index.html),
* extrafont

## How to cite

If you use this package, cite it like this:

> Andreas Angourakis. (2017, September 20). biplot2d3d - an R package for generating highly-customizable biplots. Zenodo. http://doi.org/10.5281/zenodo.897603

You may also consult and cite our publication using this package:

> Angourakis, Andreas, Verònica Martínez Ferreras, Alexis Torrano, and Josep M. Gurt Esparraguera. 2018. “Presenting Multivariate Statistical Protocols in R Using Roman Wine Amphorae Productions in Catalonia, Spain.” Journal of Archaeological Science 93 (May): 150–65. https://doi.org/10.1016/j.jas.2018.03.007.

## How to install

You can install the development version of this package from GitHub by running these lines in your console:

```
if (!require("devtools")) install.packages("devtools") # this will install devtools package, if not installed already
devtools::install_github("Andros-Spica/biplot2d3d")  
```

## How to use

In the future, this package will have a tutorial referenced here. For now, please read the examples contained in the documentation of the two main functions, typing ?biplot_2d and ?biplot_3d after installing and loading the package.

Please see the tutorial on cerUB package (<https://github.com/Andros-Spica/cerUB_tutorial>), which contains several examples of biplot2d3d applications.
