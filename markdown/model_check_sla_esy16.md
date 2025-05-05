Analysis of Bauer et al. (submitted) Functional traits of grasslands:
<br> Average community weighted mean of specific leaf area (SLA) per
block (esy16)
================
<b>Markus Bauer</b> <br>
<b>2025-05-05</b>

- [Preparation](#preparation)
- [Statistics](#statistics)
  - [Data exploration](#data-exploration)
    - [Means and deviations](#means-and-deviations)
    - [Graphs of raw data (Step 2, 6,
      7)](#graphs-of-raw-data-step-2-6-7)
    - [Outliers, zero-inflation, transformations? (Step 1, 3,
      4)](#outliers-zero-inflation-transformations-step-1-3-4)
    - [Check collinearity part 1 (Step
      5)](#check-collinearity-part-1-step-5)
  - [Models](#models)
  - [Model check](#model-check)
    - [DHARMa](#dharma)
    - [Check collinearity part 2 (Step
      5)](#check-collinearity-part-2-step-5)
  - [Model comparison](#model-comparison)
    - [<i>R</i><sup>2</sup> values](#r2-values)
    - [AICc](#aicc)
  - [Predicted values](#predicted-values)
    - [Summary table](#summary-table)
    - [Forest plot](#forest-plot)
    - [Effect sizes](#effect-sizes)
- [Session info](#session-info)

<br/> <br/> <b>Markus Bauer</b>

Technichal University of Munich, TUM School of Life Sciences, Chair of
Restoration Ecology, Emil-Ramann-Straße 6, 85354 Freising, Germany

<markus1.bauer@tum.de>

ORCiD ID: [0000-0001-5372-4174](https://orcid.org/0000-0001-5372-4174)
<br> [Google
Scholar](https://scholar.google.de/citations?user=oHhmOkkAAAAJ&hl=de&oi=ao)
<br> GitHub: [markus1bauer](https://github.com/markus1bauer)

> **NOTE:** To compare different models, you only have to change the
> models in the section ‘Load models’

# Preparation

Protocol of data exploration (Steps 1-8) used from Zuur et al. (2010)
Methods Ecol Evol [DOI:
10.1111/2041-210X.12577](https://doi.org/10.1111/2041-210X.12577)

#### Packages

``` r
library(here)
library(tidyverse)
library(ggbeeswarm)
library(patchwork)
library(DHARMa)
library(emmeans)
```

#### Load data

``` r
sites <- read_csv(
  here("data", "processed", "data_processed_sites_esy16.csv"),
  col_names = TRUE, na = c("na", "NA", ""), col_types = cols(
    .default = "?",
    eco.id = "f",
    region = col_factor(levels = c("north", "centre", "south"), ordered = TRUE),
    site.type = col_factor(
      levels = c("positive", "restored", "negative"), ordered = TRUE
      ),
    fertilized = "f",
    freq.mow = "f",
    obs.year = "f"
  )
) %>%
  filter(esy16 %in% c("R", "R22", "R1A") & !(eco.id == 647)) %>%
  mutate(
    esy16 = fct_relevel(esy16, "R", "R22", "R1A"),
    eco.id = factor(eco.id)
    ) %>%
  rename(y = cwm.abu.sla.mean)
```

# Statistics

## Data exploration

### Means and deviations

``` r
Rmisc::CI(sites$y, ci = .95)
```

    ##    upper     mean    lower 
    ## 241.8053 237.2950 232.7846

``` r
median(sites$y)
```

    ## [1] 237.375

``` r
sd(sites$y)
```

    ## [1] 30.05524

``` r
quantile(sites$y, probs = c(0.05, 0.95), na.rm = TRUE)
```

    ##       5%      95% 
    ## 183.8435 284.9535

``` r
sites %>% count(eco.id)
```

    ## # A tibble: 3 × 2
    ##   eco.id     n
    ##   <fct>  <int>
    ## 1 654       55
    ## 2 686       62
    ## 3 664       56

``` r
sites %>% count(site.type)
```

    ## # A tibble: 3 × 2
    ##   site.type     n
    ##   <ord>     <int>
    ## 1 positive     31
    ## 2 restored    109
    ## 3 negative     33

``` r
sites %>% count(esy16)
```

    ## # A tibble: 3 × 2
    ##   esy16     n
    ##   <fct> <int>
    ## 1 R        85
    ## 2 R22      64
    ## 3 R1A      24

``` r
sites %>% count(esy16, eco.id)
```

    ## # A tibble: 8 × 3
    ##   esy16 eco.id     n
    ##   <fct> <fct>  <int>
    ## 1 R     654       25
    ## 2 R     686       31
    ## 3 R     664       29
    ## 4 R22   654       15
    ## 5 R22   686       22
    ## 6 R22   664       27
    ## 7 R1A   654       15
    ## 8 R1A   686        9

``` r
sites %>% count(esy16, site.type)
```

    ## # A tibble: 9 × 3
    ##   esy16 site.type     n
    ##   <fct> <ord>     <int>
    ## 1 R     positive     17
    ## 2 R     restored     45
    ## 3 R     negative     23
    ## 4 R22   positive      8
    ## 5 R22   restored     49
    ## 6 R22   negative      7
    ## 7 R1A   positive      6
    ## 8 R1A   restored     15
    ## 9 R1A   negative      3

### Graphs of raw data (Step 2, 6, 7)

![](model_check_sla_esy16_files/figure-gfm/data-exploration-1.png)<!-- -->![](model_check_sla_esy16_files/figure-gfm/data-exploration-2.png)<!-- -->![](model_check_sla_esy16_files/figure-gfm/data-exploration-3.png)<!-- -->![](model_check_sla_esy16_files/figure-gfm/data-exploration-4.png)<!-- -->![](model_check_sla_esy16_files/figure-gfm/data-exploration-5.png)<!-- -->

### Outliers, zero-inflation, transformations? (Step 1, 3, 4)

![](model_check_sla_esy16_files/figure-gfm/outliers-1.png)<!-- -->

### Check collinearity part 1 (Step 5)

Exclude r \> 0.7 <br> Dormann et al. 2013 Ecography [DOI:
10.1111/j.1600-0587.2012.07348.x](https://doi.org/10.1111/j.1600-0587.2012.07348.x)

``` r
# sites %>%
#   select(where(is.numeric), -y, -starts_with("cwm.")) %>%
#   GGally::ggpairs(
#     lower = list(continuous = "smooth_loess")
#     ) +
#   theme(strip.text = element_text(size = 7))

# -> no continuous variables
```

## Models

> **NOTE:** Only here you have to modify the script to compare other
> models

``` r
load(file = here("outputs", "models", "model_sla_esy16_1.Rdata"))
load(file = here("outputs", "models", "model_sla_esy16_2.Rdata"))
m_1 <- m1
m_2 <- m2
```

``` r
m_1$call
## lm(formula = y ~ esy16 * (site.type + eco.id) + obs.year, data = sites)
m_2$call
## lm(formula = y ~ esy16 * site.type + eco.id + obs.year, data = sites)
```

## Model check

### DHARMa

``` r
simulation_output_1 <- simulateResiduals(m_1, plot = TRUE)
```

![](model_check_sla_esy16_files/figure-gfm/dharma_all-1.png)<!-- -->

``` r
simulation_output_2 <- simulateResiduals(m_2, plot = TRUE)
```

![](model_check_sla_esy16_files/figure-gfm/dharma_all-2.png)<!-- -->

``` r
plotResiduals(simulation_output_1$scaledResiduals, sites$eco.id)
```

![](model_check_sla_esy16_files/figure-gfm/dharma_single-1.png)<!-- -->

``` r
plotResiduals(simulation_output_2$scaledResiduals, sites$eco.id)
```

![](model_check_sla_esy16_files/figure-gfm/dharma_single-2.png)<!-- -->

``` r
plotResiduals(simulation_output_1$scaledResiduals, sites$site.type)
```

![](model_check_sla_esy16_files/figure-gfm/dharma_single-3.png)<!-- -->

``` r
plotResiduals(simulation_output_2$scaledResiduals, sites$site.type)
```

![](model_check_sla_esy16_files/figure-gfm/dharma_single-4.png)<!-- -->

``` r
plotResiduals(simulation_output_1$scaledResiduals, sites$obs.year)
```

![](model_check_sla_esy16_files/figure-gfm/dharma_single-5.png)<!-- -->

``` r
plotResiduals(simulation_output_2$scaledResiduals, sites$obs.year)
```

![](model_check_sla_esy16_files/figure-gfm/dharma_single-6.png)<!-- -->

``` r
plotResiduals(simulation_output_1$scaledResiduals, sites$history)
```

![](model_check_sla_esy16_files/figure-gfm/dharma_single-7.png)<!-- -->

``` r
plotResiduals(simulation_output_2$scaledResiduals, sites$history)
```

![](model_check_sla_esy16_files/figure-gfm/dharma_single-8.png)<!-- -->

``` r
plotResiduals(simulation_output_1$scaledResiduals, sites$hydrology)
## Warning in ensurePredictor(simulationOutput, form): DHARMa:::ensurePredictor:
## character string was provided as predictor. DHARMa has converted to factor
## automatically. To remove this warning, please convert to factor before
## attempting to plot with DHARMa.
```

![](model_check_sla_esy16_files/figure-gfm/dharma_single-9.png)<!-- -->

``` r
plotResiduals(simulation_output_2$scaledResiduals, sites$hydrology)
## Warning in ensurePredictor(simulationOutput, form): DHARMa:::ensurePredictor:
## character string was provided as predictor. DHARMa has converted to factor
## automatically. To remove this warning, please convert to factor before
## attempting to plot with DHARMa.
```

![](model_check_sla_esy16_files/figure-gfm/dharma_single-10.png)<!-- -->

``` r
plotResiduals(simulation_output_1$scaledResiduals, sites$land.use.hist)
## Warning in ensurePredictor(simulationOutput, form): DHARMa:::ensurePredictor:
## character string was provided as predictor. DHARMa has converted to factor
## automatically. To remove this warning, please convert to factor before
## attempting to plot with DHARMa.
```

![](model_check_sla_esy16_files/figure-gfm/dharma_single-11.png)<!-- -->

``` r
plotResiduals(simulation_output_2$scaledResiduals, sites$land.use.hist)
## Warning in ensurePredictor(simulationOutput, form): DHARMa:::ensurePredictor:
## character string was provided as predictor. DHARMa has converted to factor
## automatically. To remove this warning, please convert to factor before
## attempting to plot with DHARMa.
```

![](model_check_sla_esy16_files/figure-gfm/dharma_single-12.png)<!-- -->

``` r
plotResiduals(simulation_output_1$scaledResiduals, sites$fertilized)
```

![](model_check_sla_esy16_files/figure-gfm/dharma_single-13.png)<!-- -->

``` r
plotResiduals(simulation_output_2$scaledResiduals, sites$fertilized)
```

![](model_check_sla_esy16_files/figure-gfm/dharma_single-14.png)<!-- -->

``` r
plotResiduals(simulation_output_1$scaledResiduals, sites$freq.mow)
```

![](model_check_sla_esy16_files/figure-gfm/dharma_single-15.png)<!-- -->

``` r
plotResiduals(simulation_output_2$scaledResiduals, sites$freq.mow)
```

![](model_check_sla_esy16_files/figure-gfm/dharma_single-16.png)<!-- -->

### Check collinearity part 2 (Step 5)

Remove VIF \> 3 or \> 10 <br> Zuur et al. 2010 Methods Ecol Evol [DOI:
10.1111/j.2041-210X.2009.00001.x](https://doi.org/10.1111/j.2041-210X.2009.00001.x)

``` r
car::vif(m_1, type = "predictor")
```

    ## Warning in cor(X): Standardabweichung ist Null

    ## GVIFs computed for predictors

    ##           GVIF Df GVIF^(1/(2*Df))    Interacts With         Other Predictors
    ## esy16       NA 14              NA site.type, eco.id                 obs.year
    ## site.type   NA  8              NA             esy16         eco.id, obs.year
    ## eco.id      NA  8              NA             esy16      site.type, obs.year
    ## obs.year    NA  1              NA              --   esy16, site.type, eco.id

``` r
car::vif(m_2)
```

    ## there are higher-order terms (interactions) in this model
    ## consider setting type = 'predictor'; see ?vif

    ##                     GVIF Df GVIF^(1/(2*Df))
    ## esy16           2.848953  2        1.299186
    ## site.type       3.169306  2        1.334262
    ## eco.id          1.169420  2        1.039902
    ## obs.year        1.047374  1        1.023413
    ## esy16:site.type 7.205665  4        1.279998

## Model comparison

### <i>R</i><sup>2</sup> values

``` r
MuMIn::r.squaredGLMM(m_1)
##            R2m       R2c
## [1,] 0.4100263 0.4100263
MuMIn::r.squaredGLMM(m_2)
##            R2m       R2c
## [1,] 0.3937617 0.3937617
```

### AICc

Use AICc and not AIC since ratio n/K \< 40 <br> Burnahm & Anderson 2002
p. 66 ISBN: 978-0-387-95364-9

``` r
MuMIn::AICc(m_1, m_2) %>%
  arrange(AICc)
##     df     AICc
## m_2 13 1604.514
## m_1 16 1605.426
```

## Predicted values

### Summary table

``` r
car::Anova(m_1)
```

    ## Note: model has aliased coefficients
    ##       sums of squares computed by model comparison

    ## Anova Table (Type II tests)
    ## 
    ## Response: y
    ##                 Sum Sq  Df F value    Pr(>F)    
    ## esy16            21470   2 19.1755 3.502e-08 ***
    ## site.type        12149   2 10.8511 3.839e-05 ***
    ## eco.id           11909   2 10.6365 4.637e-05 ***
    ## obs.year           409   1  0.7315    0.3937    
    ## esy16:site.type   1174   4  0.5242    0.7181    
    ## esy16:eco.id      3273   3  1.9488    0.1239    
    ## Residuals        88451 158                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(m_1)
```

    ## 
    ## Call:
    ## lm(formula = y ~ esy16 * (site.type + eco.id) + obs.year, data = sites)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -68.894 -11.428  -0.253  13.203  70.616 
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           227.356      5.226  43.501  < 2e-16 ***
    ## esy16R22               10.318      8.678   1.189 0.236227    
    ## esy16R1A              -29.567      8.289  -3.567 0.000478 ***
    ## site.type.L            20.669      5.389   3.835 0.000181 ***
    ## site.type.Q             7.062      4.238   1.666 0.097642 .  
    ## eco.id686              25.700      6.376   4.031 8.62e-05 ***
    ## eco.id664              29.724      6.474   4.591 8.94e-06 ***
    ## obs.year2023           -3.159      3.694  -0.855 0.393699    
    ## esy16R22:site.type.L   -4.044     10.344  -0.391 0.696367    
    ## esy16R1A:site.type.L  -10.290     13.509  -0.762 0.447348    
    ## esy16R22:site.type.Q   -7.513      7.159  -1.049 0.295605    
    ## esy16R1A:site.type.Q   -9.222      9.550  -0.966 0.335687    
    ## esy16R22:eco.id686    -22.252     10.252  -2.170 0.031468 *  
    ## esy16R1A:eco.id686    -17.488     12.299  -1.422 0.157038    
    ## esy16R22:eco.id664    -19.160     10.147  -1.888 0.060827 .  
    ## esy16R1A:eco.id664         NA         NA      NA       NA    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 23.66 on 158 degrees of freedom
    ## Multiple R-squared:  0.4307, Adjusted R-squared:  0.3803 
    ## F-statistic: 8.538 on 14 and 158 DF,  p-value: 1.451e-13

### Forest plot

``` r
dotwhisker::dwplot(
  list(m_1, m_2),
  ci = 0.95,
  show_intercept = FALSE,
  vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) +
  xlim(-55, 50) +
  theme_classic()
```

    ## Model matrix is rank deficient. Parameters `esy16R1A:eco.id664` were not
    ##   estimable.

![](model_check_sla_esy16_files/figure-gfm/predicted_values-1.png)<!-- -->

### Effect sizes

Effect sizes of chosen model just to get exact values of means etc. if
necessary.

#### ESY EUNIS Habitat type

``` r
(emm <- emmeans(
  m_1,
  revpairwise ~ esy16,
  type = "response"
  ))
```

    ## $emmeans
    ##  esy16 emmean  SE  df lower.CL upper.CL
    ##  R        244 2.8 158      239      250
    ##  R22      241 4.4 158      232      249
    ##  R1A   nonEst  NA  NA       NA       NA
    ## 
    ## Results are averaged over the levels of: site.type, eco.id, obs.year 
    ## Confidence level used: 0.95 
    ## 
    ## $contrasts
    ##  contrast  estimate  SE  df t.ratio p.value
    ##  R22 - R      -3.49 5.2 158  -0.670  0.5037
    ##  R1A - R     nonEst  NA  NA      NA      NA
    ##  R1A - R22   nonEst  NA  NA      NA      NA
    ## 
    ## Results are averaged over the levels of: site.type, eco.id, obs.year 
    ## P value adjustment: tukey method for comparing a family of 2 estimates

``` r
plot(emm, comparison = TRUE)
```

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_segment()`).

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_point()`).

![](model_check_sla_esy16_files/figure-gfm/effect-sizes-1-1.png)<!-- -->

#### Habiat type x Region

``` r
(emm <- emmeans(
  m_1,
  revpairwise ~ esy16 + eco.id,
  type = "response"
  ))
```

    ## $emmeans
    ##  esy16 eco.id emmean   SE  df lower.CL upper.CL
    ##  R     654       226 4.82 158      216      235
    ##  R22   654       236 7.22 158      222      250
    ##  R1A   654       196 6.73 158      183      210
    ##  R     686       251 4.42 158      243      260
    ##  R22   686       240 5.94 158      228      251
    ##  R1A   686       204 9.50 158      186      223
    ##  R     664       256 4.54 158      247      264
    ##  R22   664       247 5.28 158      236      257
    ##  R1A   664    nonEst   NA  NA       NA       NA
    ## 
    ## Results are averaged over the levels of: site.type, obs.year 
    ## Confidence level used: 0.95 
    ## 
    ## $contrasts
    ##  contrast                      estimate    SE  df t.ratio p.value
    ##  R22 eco.id654 - R eco.id654      10.32  8.68 158   1.189  0.9340
    ##  R1A eco.id654 - R eco.id654     -29.57  8.29 158  -3.567  0.0110
    ##  R1A eco.id654 - R22 eco.id654   -39.88  9.89 158  -4.034  0.0021
    ##  R eco.id686 - R eco.id654        25.70  6.38 158   4.031  0.0022
    ##  R eco.id686 - R22 eco.id654      15.38  8.45 158   1.820  0.6072
    ##  R eco.id686 - R1A eco.id654      55.27  8.07 158   6.846  <.0001
    ##  R22 eco.id686 - R eco.id654      13.77  7.65 158   1.800  0.6211
    ##  R22 eco.id686 - R22 eco.id654     3.45  8.03 158   0.430  0.9999
    ##  R22 eco.id686 - R1A eco.id654    43.33  8.99 158   4.820  0.0001
    ##  R22 eco.id686 - R eco.id686     -11.93  7.39 158  -1.614  0.7411
    ##  R1A eco.id686 - R eco.id654     -21.35 10.70 158  -2.000  0.4849
    ##  R1A eco.id686 - R22 eco.id654   -31.67 12.00 158  -2.645  0.1480
    ##  R1A eco.id686 - R1A eco.id654     8.21 10.50 158   0.782  0.9938
    ##  R1A eco.id686 - R eco.id686     -47.05 10.50 158  -4.471  0.0004
    ##  R1A eco.id686 - R22 eco.id686   -35.12 11.20 158  -3.125  0.0430
    ##  R eco.id664 - R eco.id654        29.72  6.47 158   4.591  0.0002
    ##  R eco.id664 - R22 eco.id654      19.41  8.53 158   2.276  0.3134
    ##  R eco.id664 - R1A eco.id654      59.29  8.13 158   7.295  <.0001
    ##  R eco.id664 - R eco.id686         4.02  6.12 158   0.658  0.9979
    ##  R eco.id664 - R22 eco.id686      15.96  7.48 158   2.135  0.3975
    ##  R eco.id664 - R1A eco.id686      51.08 10.50 158   4.842  0.0001
    ##  R22 eco.id664 - R eco.id654      20.88  7.15 158   2.920  0.0754
    ##  R22 eco.id664 - R22 eco.id654    10.56  7.81 158   1.352  0.8771
    ##  R22 eco.id664 - R1A eco.id654    50.45  8.56 158   5.896  <.0001
    ##  R22 eco.id664 - R eco.id686      -4.82  6.89 158  -0.700  0.9969
    ##  R22 eco.id664 - R22 eco.id686     7.12  6.82 158   1.043  0.9669
    ##  R22 eco.id664 - R1A eco.id686    42.24 10.90 158   3.883  0.0037
    ##  R22 eco.id664 - R eco.id664      -8.84  6.97 158  -1.269  0.9087
    ##  R1A eco.id664 - R eco.id654     nonEst    NA  NA      NA      NA
    ##  R1A eco.id664 - R22 eco.id654   nonEst    NA  NA      NA      NA
    ##  R1A eco.id664 - R1A eco.id654   nonEst    NA  NA      NA      NA
    ##  R1A eco.id664 - R eco.id686     nonEst    NA  NA      NA      NA
    ##  R1A eco.id664 - R22 eco.id686   nonEst    NA  NA      NA      NA
    ##  R1A eco.id664 - R1A eco.id686   nonEst    NA  NA      NA      NA
    ##  R1A eco.id664 - R eco.id664     nonEst    NA  NA      NA      NA
    ##  R1A eco.id664 - R22 eco.id664   nonEst    NA  NA      NA      NA
    ## 
    ## Results are averaged over the levels of: site.type, obs.year 
    ## P value adjustment: tukey method for comparing a family of 8 estimates

``` r
plot(emm, comparison = TRUE)
```

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_segment()`).

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_point()`).

![](model_check_sla_esy16_files/figure-gfm/effect-sizes-2-1.png)<!-- -->

#### Habiat type x Site type

``` r
(emm <- emmeans(
  m_1,
  revpairwise ~ esy16 + site.type,
  type = "response"
  ))
```

    ## $emmeans
    ##  esy16 site.type emmean   SE  df lower.CL upper.CL
    ##  R     positive     233 5.79 158      221      244
    ##  R22   positive     229 8.73 158      212      246
    ##  R1A   positive  nonEst   NA  NA       NA       NA
    ##  R     restored     238 3.55 158      231      245
    ##  R22   restored     241 3.42 158      234      248
    ##  R1A   restored  nonEst   NA  NA       NA       NA
    ##  R     negative     262 4.94 158      252      271
    ##  R22   negative     252 9.00 158      235      270
    ##  R1A   negative  nonEst   NA  NA       NA       NA
    ## 
    ## Results are averaged over the levels of: eco.id, obs.year 
    ## Confidence level used: 0.95 
    ## 
    ## $contrasts
    ##  contrast                    estimate    SE  df t.ratio p.value
    ##  R22 positive - R positive      -3.69 10.50 158  -0.353  0.9997
    ##  R1A positive - R positive     nonEst    NA  NA      NA      NA
    ##  R1A positive - R22 positive   nonEst    NA  NA      NA      NA
    ##  R restored - R positive         5.97  6.78 158   0.880  0.9654
    ##  R restored - R22 positive       9.66  9.42 158   1.025  0.9316
    ##  R restored - R1A positive     nonEst    NA  NA      NA      NA
    ##  R22 restored - R positive       8.61  6.75 158   1.277  0.8336
    ##  R22 restored - R22 positive    12.31  9.25 158   1.331  0.8061
    ##  R22 restored - R1A positive   nonEst    NA  NA      NA      NA
    ##  R22 restored - R restored       2.65  4.93 158   0.537  0.9970
    ##  R1A restored - R positive     nonEst    NA  NA      NA      NA
    ##  R1A restored - R22 positive   nonEst    NA  NA      NA      NA
    ##  R1A restored - R1A positive     9.98 11.50 158   0.870  0.9672
    ##  R1A restored - R restored     nonEst    NA  NA      NA      NA
    ##  R1A restored - R22 restored   nonEst    NA  NA      NA      NA
    ##  R negative - R positive        29.23  7.62 158   3.835  0.0029
    ##  R negative - R22 positive      32.92 10.00 158   3.282  0.0185
    ##  R negative - R1A positive     nonEst    NA  NA      NA      NA
    ##  R negative - R restored        23.26  6.08 158   3.824  0.0030
    ##  R negative - R22 restored      20.62  6.00 158   3.434  0.0115
    ##  R negative - R1A restored     nonEst    NA  NA      NA      NA
    ##  R22 negative - R positive      19.82 10.60 158   1.862  0.4719
    ##  R22 negative - R22 positive    23.51 12.40 158   1.890  0.4535
    ##  R22 negative - R1A positive   nonEst    NA  NA      NA      NA
    ##  R22 negative - R restored      13.85  9.67 158   1.432  0.7496
    ##  R22 negative - R22 restored    11.20  9.62 158   1.165  0.8835
    ##  R22 negative - R1A restored   nonEst    NA  NA      NA      NA
    ##  R22 negative - R negative      -9.41 10.30 158  -0.916  0.9584
    ##  R1A negative - R positive     nonEst    NA  NA      NA      NA
    ##  R1A negative - R22 positive   nonEst    NA  NA      NA      NA
    ##  R1A negative - R1A positive    14.68 17.50 158   0.837  0.9726
    ##  R1A negative - R restored     nonEst    NA  NA      NA      NA
    ##  R1A negative - R22 restored   nonEst    NA  NA      NA      NA
    ##  R1A negative - R1A restored     4.69 15.50 158   0.302  0.9999
    ##  R1A negative - R negative     nonEst    NA  NA      NA      NA
    ##  R1A negative - R22 negative   nonEst    NA  NA      NA      NA
    ## 
    ## Results are averaged over the levels of: eco.id, obs.year 
    ## P value adjustment: tukey method for comparing a family of 6.52079728939615 estimates

``` r
plot(emm, comparison = TRUE)
```

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_segment()`).

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](model_check_sla_esy16_files/figure-gfm/effect-sizes-3-1.png)<!-- -->

# Session info

    ## R version 4.4.2 (2024-10-31 ucrt)
    ## Platform: x86_64-w64-mingw32/x64
    ## Running under: Windows 11 x64 (build 26100)
    ## 
    ## Matrix products: default
    ## 
    ## 
    ## locale:
    ## [1] LC_COLLATE=German_Germany.utf8  LC_CTYPE=German_Germany.utf8   
    ## [3] LC_MONETARY=German_Germany.utf8 LC_NUMERIC=C                   
    ## [5] LC_TIME=German_Germany.utf8    
    ## 
    ## time zone: Europe/Berlin
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] emmeans_1.11.0   DHARMa_0.4.7     patchwork_1.2.0  ggbeeswarm_0.7.2
    ##  [5] lubridate_1.9.3  forcats_1.0.0    stringr_1.5.1    dplyr_1.1.4     
    ##  [9] purrr_1.0.2      readr_2.1.5      tidyr_1.3.1      tibble_3.2.1    
    ## [13] ggplot2_3.5.1    tidyverse_2.0.0  here_1.0.1      
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyselect_1.2.1   vipor_0.4.7        farver_2.1.2       fastmap_1.2.0     
    ##  [5] bayestestR_0.15.2  promises_1.3.2     digest_0.6.36      estimability_1.5.1
    ##  [9] timechange_0.3.0   mime_0.12          lifecycle_1.0.4    magrittr_2.0.3    
    ## [13] compiler_4.4.2     rlang_1.1.4        tools_4.4.2        utf8_1.2.4        
    ## [17] yaml_2.3.9         knitr_1.48         labeling_0.4.3     bit_4.0.5         
    ## [21] ggstance_0.3.7     plyr_1.8.9         gap.datasets_0.0.6 abind_1.4-8       
    ## [25] withr_3.0.0        datawizard_1.0.2   stats4_4.4.2       grid_4.4.2        
    ## [29] fansi_1.0.6        xtable_1.8-4       colorspace_2.1-0   scales_1.3.0      
    ## [33] iterators_1.0.14   MASS_7.3-60.2      insight_1.1.0      cli_3.6.3         
    ## [37] mvtnorm_1.3-3      dotwhisker_0.8.3   rmarkdown_2.27     crayon_1.5.3      
    ## [41] reformulas_0.4.0   generics_0.1.3     performance_0.13.0 rstudioapi_0.16.0 
    ## [45] tzdb_0.4.0         parameters_0.24.2  minqa_1.2.8        splines_4.4.2     
    ## [49] parallel_4.4.2     vctrs_0.6.5        boot_1.3-31        Matrix_1.7-0      
    ## [53] carData_3.0-5      car_3.1-3          hms_1.1.3          bit64_4.0.5       
    ## [57] Formula_1.2-5      qgam_1.3.4         beeswarm_0.4.0     Rmisc_1.5.1       
    ## [61] foreach_1.5.2      gap_1.6            glue_1.7.0         nloptr_2.2.1      
    ## [65] codetools_0.2-20   stringi_1.8.4      gtable_0.3.5       later_1.4.1       
    ## [69] lme4_1.1-37        munsell_0.5.1      pillar_1.9.0       htmltools_0.5.8.1 
    ## [73] R6_2.5.1           Rdpack_2.6.3       doParallel_1.0.17  rprojroot_2.0.4   
    ## [77] vroom_1.6.5        evaluate_0.24.0    shiny_1.10.0       lattice_0.22-6    
    ## [81] highr_0.11         rbibutils_2.3      httpuv_1.6.15      Rcpp_1.0.14       
    ## [85] gridExtra_2.3      coda_0.19-4.1      nlme_3.1-164       MuMIn_1.48.11     
    ## [89] mgcv_1.9-1         xfun_0.45          pkgconfig_2.0.3
