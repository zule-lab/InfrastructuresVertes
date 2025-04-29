# Green alleys in Quebec provide variable biodiversity support and ecosystem services


[![DOI](https://zenodo.org/badge/644920466.svg)](https://doi.org/10.5281/zenodo.15304732)



## Authors
[Isabella C. Richmond](https://github.com/icrichmond/), [Kayleigh Hutt-Taylor](https://ca.linkedin.com/in/kayleigh-hutt-taylor-a85981101?trk=public_post_feed-actor-name), [Johanna Arnet](https://paqlab.uqam.ca/members/johanna-arnet.php?lang=en), Lauren Bianco, Antonia Vieira Zanella, François Bérubé, Paola Faddoul, Étienne Perreault-Mandeville, [Nathalie Boucher](https://organismerespire.com/index.html), [Thi Thanh Hiên Pham](https://professeurs.uqam.ca/professeur/pham.thi_thanh_hien/), [Carly D. Ziter](https://www.carlyziter.com/)

## Abstract
Green infrastructure is increasing in popularity in cities globally because of its potential to improve urban sustainability and resident quality of life. In this paper, we studied green alleys in two Quebec cities, one with a resident-led green alley program and one with a municipally led program. Green alleys are conceptualized and promoted as green infrastructure that provide many benefits for urban residents. Using mixed social and ecological methods, we assessed 53 green alleys’ capacity to support biodiversity and provide ecosystem services, alongside 23 grey alleys and 76 streets. We interviewed residents to select the ecosystem services that were most relevant to people living around green alleys and then measured indicators of ecosystem service capacity with traditional ecological techniques, harnessing an interdisciplinary approach to ecosystem service assessment. Green alleys provided more biodiversity support than grey alleys and adjacent street segments but did not consistently increase the capacity for ecosystem services. Vegetative complexity and proportion of native tree species are both higher in green alleys than traditional grey alleys and adjacent streets. The proportion of flowering trees was one indicator of ecosystem services that was consistently higher in green alleys. Resident-led vs municipally led creation and management of green alleys resulted in different results, where resident-led alleys were more able to target the needs of residents but resulted in high levels of variation in both support for biodiversity and ecosystem services. We recommend ongoing funding paired with technical expert support to increase the impact of green alleys.

## Repository Use
This repository is built on a `{targets}` workflow. To run all analyses done for the paper, download and open the project, [install `{targets}`](https://books.ropensci.org/targets/), and run `targets::tar_make()`.

To access data only, raw data can be found in the `input/` folder. The only exception is the Québec census and roads files, which are downloaded in the tar_read_files.R script.
