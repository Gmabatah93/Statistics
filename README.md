# Descriptive
### Numerical: Interval | Ratio

Center | Dispersion | Shape | Bivariate
--- | --- | --- | ---
Mean | Range | Kurtosis | Covariance
Median | Variance | Skewness | Correlation
Mode | Std | |

### Categorical: Nominal | Ordinal

---

# Experimental Design
### Principals
* **Control**: compare treatment to control group
* **Replicate**: large Sample or Replicate
* **Randomize**: Randomly assign subjects to treatments
* **Block**: Covariance Analysis
  + Randomized Complete Block Design:
    + Each Block sees each treatment once.
    + Random within each block
  + Balanced Incomplete Block Design
    + not every treatment will appear in every block
  + Latin Squares
    + 2 blocking variables
    + same levels
    + No Interactions
  + Factorial Designs
    + Interactions Considered

### Sampling
> If Sampling of sample size **n** is RANDOM, then the sample is UNBIASED & REPRESENTATIVE of the population. \
Thus, any results based on the sample can GENERALIZE to the population parameter

**Simple Random Sample**:\
**Stratified Sample**:\
**Clustered Sample**:\
**Multistage Sample**:

**Bootstrap**: Sampling with Replacement. *mimics Sampling Distribution using a single sample*\
**Permutation**: Sampling without Replacement. _shuffled dataset assuming Null is true_


### Testing Procedure

> 1. Well Developed, Clear Research Problem or Question
> 2. Establish Null and Alternative
> 3. Determine Appropriate Statistical Test & Sampling Distribution
> 4. Choose Type I Error rate "alpha"
> 5. State Decision Rule
> 6. Gather Sample Data
> 7. Calculate Test Statistic
> 8. State Statistical Conclusion
> 9. Make Decision or Inference based on Conclusion

---

# Hypothesis Testing
> "Does our conclusion match the overall state of Reality"\
"Did our sample come from the same population if we assume the Null is True"

**Null**: Status Quo - Assumed - Given\
**Alternative**: Claim - Assertion - Unknown

Null = True | Alt = True
 --- | ---
x correctly inside non-reject region | x inside non-reject region due to chance. **Type II - B**
x outside non-rejection region due to chance. **Type I - a** | x correctly outside nonrejection Region. **Power: 1 - B**

<p float="left">
  <img src="Images/TYPE1.PNG" width="400" />
  <img src="Images/TYPE2.PNG" width="400" />
</p>

---

# Inference

Sampling Distribution: Central Limit Theorem, take multiple samples _(n > 30)_ & their means and it will approximate to a Normal Distribution  

## Confidence Interval
* When estimating a population parameter using a sample statistic it's never going to be perfect. _Express that error using Interval Estimate_.
* Randomness lies in the elements chosen for the sample **NOT** the population mean.
* Proportion of samples _size n_ for which our estimate the _sample mean_ is within a certain distance +_ of the true population mean. **NOT** the probability that the population mean lies within the interval
* _"95% of all intervals made using <img src="Images/CI.PNG" width="100"> will contain the population parameter"_

## Single Sample

z-test (x) | z-test (p)
--- | ---
<img src="Images/ztestx.PNG" width="100"> | <img src="Images/ztestp.PNG" width="100">

## Two Population

z-test (Unpaired) | z-test (Paired)
--- | ---
<img src="Images/ztestunparied.PNG" width="100"> |

## Chi-Squared

> * Helps understand the relationship between two categorical variables
> * Involves the Frequency of events (counts)
> * Helps compare what we actually observe with what we expected often times using population data or Theoretical data
> * Assist in determining the role of random chance variation btw categorical variables

Goodness of Fit | Independence
--- | ---
 | <img src="Images/chiind.PNG" width="200">

## ANOVA
