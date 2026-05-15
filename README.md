# EZMSA

**EZMSA** is an R Shiny application for performing method of standard addition (MSA)
calculations from raw analytical data. It automates weighted least squares regression,
model order selection, back-calculation of the unknown analyte concentration,
and measurement uncertainty estimation. An optional formatted report can be
exported in HTML, PDF, or Word format.

A live version of the application is available at: https://toxbrigitte.shinyapps.io/EZMSA/

This tool is intended to accompany the associated publication in the *Journal
of Analytical Toxicology* and to serve as a practical template for analysts
applying the method of standard addition in their own workflows.

\---

## Features

* Accepts plain text input files with comma, semicolon, or tab delimiters
* Automatic or user-specified selection of regression model order (linear or quadratic)
* Automatic or user-specified selection of regression weighting (1/x², 1/variance, 1/x, or unweighted)
* Back-calculation of the unknown analyte concentration from the y-intercept
* Measurement uncertainty estimation incorporating:

  * Uncertainty of the y-intercept (u\_b0)
  * Certified Reference Material uncertainty (u\_CRM)
  * Combined and expanded measurement uncertainty at a user-specified confidence level
* Regular and inverted calibration curve plots
* Optional, customizable report exportable in HTML, PDF, or Word format

\---

## Requirements

* R ≥ 4.4.0
* The following R packages:

```r
install.packages(c("shiny", "bslib", "rmarkdown", "ggplot2", "readxl", "hms"))
```

Generating PDF reports additionally requires a LaTeX installation. [TinyTeX](https://yihui.org/tinytex/) is recommended:

```r
install.packages("tinytex")
tinytex::install\_tinytex()
```

\---

## Running the Application

Clone or download the repository, then launch the app from R:

```r
shiny::runApp("path/to/ezmsa")
```

Alternatively, access the hosted version directly at: https://toxbrigitte.shinyapps.io/EZMSA/

\---

## Input Data Format

Input data must be provided as a **plain text file** with values separated by a
comma, semicolon, or tab. The file must be structured as follows:

* **Columns 1 through n−1:** Replicate instrument measurements
* **Column n (last column):** Spiked analyte concentrations

|Replicate 1|Replicate 2|Spiked Concentration|
|-|-|-|
|...|...|...|

Additional options when importing data:

* **Column separator:** Comma, semicolon, or tab
* **Decimal separator:** Dot or comma
* **Header:** Indicate whether a header row is present

An example input file (`example\_data.csv`) is provided in the repository.

> \*\*Important:\*\* Input values must be \*\*1-100\*\*. Very large 
numeric values produce extremely small variances, which can cause numerical 
instability in the weighted regression and lead to unreliable results. 
If your data are in large units (e.g., values in the millions), consider 
rescaling prior to analysis.

\---

## Usage

1. Launch the application and navigate to the sidebar.
2. Upload your data file and configure the import settings (separator, header, decimal format).
3. Select the desired **weight** and **regression order**, or leave both set to
*Use Best* to allow the algorithm to select automatically (recommended).
4. Enter the **concentration units**, **significance threshold** (default: 0.05),
and **u\_CRM** (set to 0 if unknown or not applicable).
5. Click **Start Analysis**.
6. Review results in the **Plots**, **Raw Data**, and **Results** tabs.
7. Optionally, navigate to the **Report** tab to customize and export a formatted report.

\---

## Model Selection Logic

When *Use Best* is selected, EZMSA automatically determines the appropriate regression model:

**Order selection:** Both linear and quadratic models are fitted. The quadratic
term (b₂) is retained only if its confidence interval does not include zero.
If the confidence interval includes zero, a linear model is selected.

**Weight selection:** If fewer than 3 replicates are present, 1/x² weighting is
applied. Otherwise, 1/variance weighting is used.

The user may also override these defaults and manually specify any combination
of order and weighting.

\---

## Measurement Uncertainty

Measurement uncertainty is estimated by combining two components in quadrature:

* **u\_b0:** Uncertainty of the y-intercept derived from the weighted least squares regression
* **u\_CRM:** Uncertainty associated with the certified reference material, provided by the user

Combined standard uncertainty:

```
u\_c = sqrt(u\_b0² + u\_CRM²)
```

Expanded uncertainty is calculated as:

```
U = t(α, df) × u\_c
```

where t is the critical t-value at the user-specified significance threshold
and degrees of freedom corresponding to the selected model.

\---

## Output

**Plots tab:** Displays both a regular calibration curve (measurements on
y-axis, concentrations on x-axis) and an inverted calibration curve
(concentrations on y-axis, measurements on x-axis), with the fitted regression
overlaid.

**Raw Data tab:** Displays the imported data for verification.

**Results tab:** Reports the weight and regression order selected, confidence
interval on the quadratic term, regression equation, u\_CRM, u\_b0, combined
standard uncertainty, expanded uncertainty, calculated unknown concentration,
and confidence interval bounds.

**Report tab:** Allows the user to specify a report title, additional comments,
and the content to be included. Reports can be downloaded in HTML, PDF, or Word format.

\---

## Authors

EZMSA was conceptualized by Brigitte Desharnais and coded by Étienne Lebrun,
based on work from the EZMSA team: Jocelyn V. Abonamah, Brigitte Desharnais,
and Szabolcs Sofalvi.

Laboratoire de sciences judiciaires et de médecine légale  
Ministère de la Sécurité Publique, Gouvernement du Québec, Canada

\---

## Citation

If you use EZMSA in your work, please cite the associated publication:

> Jocelyn V. Abonamah, Brigitte Desharnais, Étienne Lebrun, Szabolcs Sofalvi, Pascal Mireault. \*Implementing an accurate method of standard addition (MSA) using the EZMSA Excel or R tool\*. 
Journal of Analytical Toxicology. \[2026]. DOI: \[DOI]

\---

## License

Copyright © 2026, Laboratoire de sciences judiciaires et de médecine légale, Ministère de la Sécurité Publique, Gouvernement du Québec.

CC-BY-NC-SA

\---

## Contact

To report bugs or request features, please contact Brigitte Desharnais at brigitte.desharnais@msp.gouv.qc.ca, or open an issue on the [GitHub repository](https://github.com/ToxBrigitte/ezmsa).

