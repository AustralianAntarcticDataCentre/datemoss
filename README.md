# DateMoss

R package for MCMC estimation of chronologies and growth rates of moss
(or similar) samples based on radiocarbon dating.

## Installing

Binaries are available from RForge for easy installation in R:

```s
install.packages(c("coda","stringr","zoo","Hmisc"))
install.packages("DateMoss",repos="http://rforge.net/")
```

On Windows machines, the second line may need to be replaced with:

```s
install.packages("DateMoss",repos="http://rforge.net/",type="source",INSTALL_opts="--no-multiarch")
```


## TODO

- Sampler
  - Allow for multiple chains
  - Coda conversion
- Data
  - Read data format
  - Interpolating area under curve



