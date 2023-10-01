# vfc.3dscatter: A Simple 3D Visualization R Package for Numeric and Ordinal Data

## Installation
This package depends on: `rgl`, `MASS`, `stringr`.
```
install.packages("rgl")
install.packages("MASS")
install.packages("stringr")
devtools::install_github("vladflorinchelaru/vfc.3dscatter")
```

## Use
```
library(vfc.3dscatter)

d<-testData.allNumeric(30)
drawTogether.allNumeric(d)
rotateTogether(theta_delta = 2,phi_delta = 1)

d<-testData.allOrdinal(30)
drawTogether.allOrdinal(d)
rotateTogether(theta_delta = 2,phi_delta = 1)
```

## Graphic examples
Note that some artefacts are induced by gif compression.
<img src="https://raw.githubusercontent.com/vladflorinchelaru/vfc.3dscatter/main/example_images/1.png" width="500" height="500"/>
<img src="https://raw.githubusercontent.com/vladflorinchelaru/vfc.3dscatter/main/example_images/2.png" width="500" height="500"/>
<img src="https://raw.githubusercontent.com/vladflorinchelaru/vfc.3dscatter/main/example_images/1.gif" width="500" height="500"/>
<img src="https://raw.githubusercontent.com/vladflorinchelaru/vfc.3dscatter/main/example_images/2.gif" width="500" height="500"/>

## Copyright and Citation
© Vlad-Florin Chelaru, 2023-10-01, Cluj-Napoca, Romania. GNU GPL V3.0

Please cite as: _Chelaru VF. vfc.3dscatter: A Simple 3D Visualization R Package for Numeric and Ordinal Data. 2023. R package version 0.1.0. Available from:
  https://github.com/vladflorinchelaru/vfc.3dscatter_
