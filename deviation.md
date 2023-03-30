# Overview

The analytic plan of this study was preregistered on April 12, 2022 as the following DOI: [https://doi.org/10.17605/OSF.IO/SY6JX](https://doi.org/10.17605/OSF.IO/SY6JX). The data collection (creation of the corpus) started on May 7, 2022 and ended on July 1, 2022. The first round of human coding started on August 1, 2022 and ended on September 1, 2022.

The analysis of data (1st round) started on September 2, 2022 and ended on September 20, 2022.

The exploratory expert coding started on January 30, 2023 (after the analysis of data) and ended on March 21, 2023.

# Deviations from preregistration

As reported in the article, the exploratory expert coding was not preregistered and therefore not considered in the main analysis. The simulation study was not preregistered also.

The following are the deviations from the preregistration: [https://doi.org/10.17605/OSF.IO/SY6JX](https://doi.org/10.17605/OSF.IO/SY6JX).

## Hypotheses
1. The "manual deductive method" (Mattes & Kohring 2008) was not studied due to funding issues. Therefore, the preregistered H1a was not studied.
2. H4 was not studied because we don't know how to operationalize the main variable ("more likely to be affected"). If the likelihood to be affected is quantified by the variance of $CCR_{max}$, a visualization is presented in the Online Appendix. It seems that some automatic methods (e.g. ANTMN) produce a much higher variance of $CCR_{max}$.
3. H5 and the whole idea of cost effectiveness were not studied due to fact that it is difficult to estimate the time used by the programmer (PhD, 1st author) for programming.

Because of these changes, we reworded the original H1b, H2, and H3 in the paper:

*H1: Compared with manual methods, automatic inductive methods are less accurate in detecting frames.*

*H2: Compared with semi-supervised methods, automatic inductive methods are less accurate in detecting frames.*

*H3: Compared with manual methods, semi-supervised methods are less accurate in detecting frames.*

## Study design

1. The "manual deductive method" (Mattes & Kohring 2008) was not studied, as stated above.
2. For the manual coding, one item (*“Does the story contain visual information that might generate feelings of outrage, empathy-caring, sympathy, or compassion?”*) was omitted because no images are generated in our synthetic approach.

## Variables

### Measured variables

1. We decided not to use F1, precision, and recall because these values makes different assumptions about the distribution of correct and incorrect classifications. Instead we opted for $CCR_{max}$, an assumption-free measurement.

##  Analysis Plan

### Statistical models

1. We were not aware of some additional preprocessing steps for two methods at the time of preregistration. These steps were added to the multiverse analysis.
   1. For ANTMN, there is a need to extract a larger number of topics by LDA before the network analysis. In the original implementation, there is no indication of how large this number should be. We decided to introduce a variable called "K Factor" (*Kf*). When k = 5, the number of topics to be extracted by LDA is k * Kf. In the multiverse analysis, we considered *Kf* of 2, 3, and 4.
   2. For the manual deductive method, published papers using the coding scheme (Semetko & Valkenburg) use different methods to combine the frame elements. We considered also the variation in this aspect: averaging, factor analysis, and binary categorization.
2. We did not consider a preregistered decision:
   1. The "intersect" dictionary was not analyzed because there is no pattern that is the same between the two experts for "Attribution of responsibility".
