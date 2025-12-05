# Statistical Validation of Valorant Tier Lists 🎯

![R](https://img.shields.io/badge/Made%20with-R-blue?logo=R&style=flat-square)
![PDF](https://img.shields.io/badge/Report-Available-red?logo=adobe-acrobat-reader&style=flat-square)

## 📄 Abstract

This project analyzes player performance data from an official 2025 **Valorant** tournament to determine the statistical validity of community-created **Tier Lists**.

Using **R**, we applied inference models to test if a character's "Tier" (S, A, B, C) has a significant effect on the player's **Rating**, controlling for variables like Kills Per Round (KPR), Deaths Per Round (DPR), and Headshot percentage.

**Key Methods Applied:**
*   **Multiple Linear Regression:** Model fitting and diagnostic testing (Normality, Homoscedasticity, Linearity).
*   **ANOVA:** Testing for significant differences in performance means between Tiers.
*   **ANCOVA:** Analyzing the Tier effect while adjusting for continuous covariates.

**Main Conclusion:**
The analysis suggests that the fan-made Tier List **does not accurately reflect actual performance**. While "Tier C" agents show a distinct high-risk/high-reward playstyle (higher KPR and DPR), there is no statistical evidence to support the superiority of higher Tiers in terms of overall Rating.

---

## 📂 Repository Contents

*   **`Report_Valorant_Analysis.pdf`** 📕
    *   The full technical report containing the theoretical framework, mathematical formulation, and detailed interpretation of results.
    *   *Language: Galician (Abstract available in English).*
*   **`analysis_script.R`** 💻
    *   The source code used for data cleaning, model fitting, and plot generation.
*   **`/plots`** 📊
    *   Diagnostic plots generated during the study (Regression fit, QQ-plots, etc.).

## 📬 Contact
**Antón Seijo Barrio**
*Master's Degree Applicant*
