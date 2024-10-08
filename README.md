[![CRAN](http://www.r-pkg.org/badges/version/synthesizer)](http://cran.r-project.org/package=synthesizer/)
[![Downloads](https://cranlogs.r-pkg.org/badges/synthesizer)](http://cran.r-project.org/package=synthesizer/)
[![status](https://tinyverse.netlify.app/badge/synthesizer)](https://CRAN.R-project.org/package=synthesizer)



# synthesizer

Create synthetic data based on the Empirical inverse CDFs and Rank Matching

## Installing

The latest CRAN release can be installed as usual
```r
install.packages("synthesizer")
```

The git version can be installed by cloning the repo and using `make`.

```bash
git clone https://github.com/markvanderloo/synthesizer
cd synthesizer
make install
```

No guarantees that it will actually build, install, or give correct results.
(This is after all the place where development takes place).


## Example

```r
library(synthesizer)
synth_iris <- synthesize(iris)
```


See [the introductory vignette](https://cran.r-project.org/web/packages/synthesizer/vignettes/introduction.html).


## Licence

This software is released under [EUPL](https://commission.europa.eu/content/european-union-public-licence_en).



