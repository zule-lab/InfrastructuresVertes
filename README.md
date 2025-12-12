# Green alleys in Quebec provide variable biodiversity support and ecosystem services

[![DOI](https://zenodo.org/badge/644920466.svg)](https://doi.org/10.5281/zenodo.15304732)

## Authors
Isabella C Richmond, Kayleigh Hutt-Taylor, Johanna Arnet, Lauren Bianco, Antonia Vieira Zanella, François Bérubé, Paola Faddoul, Étienne Perreault-Mandeville, Nathalie Boucher, Thi Thanh Hiên Pham, Carly D Ziter

## Abstract
Green infrastructure is increasing in popularity in cities globally because of its potential to improve urban sustainability and resident quality of life. In this paper, we studied green alleys in two Quebec cities, one with a resident-led green alley program and one with a municipally led program. Green alleys are conceptualized and promoted as green infrastructure that provide many benefits for urban residents. Using ecological methods supported by qualitative interviews, we assessed 53 green alleys’ capacity to support biodiversity and provide ecosystem services, alongside 23 grey alleys and 76 streets. We interviewed residents to identify the ecosystem services that were most relevant to people living around green alleys and then measured indicators of ecosystem service capacity with ecological techniques, harnessing an approach that incorporated both ecological data and resident preferences. Green alleys provided more biodiversity support than grey alleys and adjacent street segments but did not consistently increase the capacity for ecosystem services. Vegetative complexity and proportion of native tree species are both higher in green alleys than traditional grey alleys and adjacent streets. The proportion of flowering trees was one indicator of ecosystem services that was consistently higher in green alleys. Resident-led vs municipally led creation and management of green alleys resulted in differences, where resident-led alleys were more able to target the needs of residents but had  high levels of variation in both support for biodiversity and ecosystem services. We recommend ongoing funding paired with technical expert support to increase the impact of green alleys.

## Repository Use 
This repository is built on a `{targets}` workflow and uses a `{renv}` environment. To install all necessary packages, run `renv::restore()` before running the workflow. For any issues related to `{cmdstanr}` installation, refer to their [help page](https://mc-stan.org/cmdstanr/). For any issues related to `{zarg}`, install based on instructions at the [repository](https://github.com/robitalec/zarg).

After all packages have been installed, the entire workflow can be run by opening the R projet and running `targets::tar_make()`. For more details on `{targets}` workflows, check out the `{targets}` [book](https://books.ropensci.org/targets/)

To access data only, raw data and metadata can be found in the `input/` folder. The only exception is the Québec census data, which is downloaded in the census-prep.R script.
