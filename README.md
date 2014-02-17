# DateMoss

R package for MCMC estimation of chronologies and growth rates of moss
(or similar) samples based on radiocarbon dating.

## Installing

Binaries are available from RForge for easy installation in R:

```s
install.packages(c("coda","stringr","zoo","Hmisc"))
install.packages("DateMoss",repos="http://rforge.net/")
```

## TODO

- Sampler
  - Allow for multiple chains
  - Coda conversion
- Data
  - Read reference data
  - Read data format
  - Interpolating area under curve



