# Modeling Yearly Maximums and Future Cyber Risk

A statistical analysis of extreme cyber security breaches using **Extreme Value Theory (EVT)**.

This project investigates how large future cyber security breaches could become by modeling annual maximum data breaches with **Generalized Extreme Value (GEV)** distributions. The analysis compares stationary and non-stationary extreme value models, estimates long-term return levels, and evaluates model uncertainty through bootstrap resampling and Monte Carlo simulations.

---

## Project Overview

Large cyber security breaches are rare but can have devastating consequences. Traditional statistical methods focus on average behavior and often underestimate the probability of extreme events.

This project applies **Extreme Value Theory (EVT)** to annual maximum data breaches in order to:

- Estimate the potential size of future extreme cyber attacks.
- Investigate whether the magnitude of the largest annual breaches has changed over time.
- Compare stationary and non-stationary GEV models.
- Quantify uncertainty in long-term risk predictions.

---

## Dataset

The analysis uses the **"World's Biggest Data Breaches and Hacks"** dataset from Kaggle.

The original dataset contains **419 reported cyber security breaches (2004–2022)**. Using the Block Maxima approach, only the largest breach from each year is retained for the extreme value analysis.

Variables used:

- `year`
- `records_lost`

---

## Methods

The analysis includes:

- Data cleaning and preprocessing
- Exploratory data analysis
- Annual Block Maxima extraction
- Stationary Generalized Extreme Value (GEV) model
- Non-stationary GEV model with a time-dependent location parameter
- Model comparison using
  - AIC
  - Likelihood Ratio Test
- Return level estimation
  - 50-year event
  - 100-year event
- Bootstrap uncertainty analysis
- Monte Carlo simulation study comparing stationary and non-stationary models

The implementation is written entirely in **R** using the `extRemes` package.

---

## Main Results

The analysis indicates:

- Heavy-tailed behavior in extreme cyber security breaches.
- Future breaches involving **billions of compromised records** are statistically plausible.
- No statistically significant temporal trend was found in annual maximum breach sizes.
- The stationary GEV model was preferred over the non-stationary alternative according to AIC and likelihood ratio testing.
- Long-term return level estimates are associated with considerable uncertainty due to the limited sample size.

---

## How to Run

1. Clone the repository.

```bash
git clone https://github.com/yourusername/Modeling-Yearly-Maximums-and-Future-Cyber-Risk.git
```

2. Place the dataset (`data_breaches.csv`) in the project directory.

3. Open `Project_work_cyber_risk.R` in RStudio.

4. Install required packages if necessary.

5. Run the script from top to bottom.

---

## Authors

**Måns Conradson**

**Jacob Telander**
