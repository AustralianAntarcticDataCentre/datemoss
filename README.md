# DateMoss

R package for MCMC estimation of chronologies and growth rates of moss
(or similar) samples based on radiocarbon dating.

## Installing

The package source is available from RForge for easy installation in R:

```s
install.packages(c("coda","stringr","zoo","Hmisc","XLConnect"))
install.packages("DateMoss",repos="http://rforge.net/",type="source",INSTALL_opts="--no-multiarch")
```

Warning: only tested using Windows and 64-bit R


## TODO

- Sampler
  - Allow for multiple chains
  - Coda conversion
- Data
  - Read data format
  - Interpolating area under curve



