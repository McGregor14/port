# PORT

This repository contains materials used to clean and analyse data generated from the Predicting Outcomes in Response to Therapy (PORT) study.

# Requirements

# Structure

Requires updating

```         
├── data
│   ├── interim
│   │   ├── flare
│   │   │   ├── step-01
│   │   │   └── step-02
│   │   ├── ieso
│   │   │   ├── step-01
│   │   │   └── step-02
│   │   ├── midapp
│   │   │   ├── step-01
│   │   │   └── step-02
│   │   └── redcap
│   │       ├── step-01
│   │       └── step-02
│   ├── processed
│   └── raw
│       ├── flare
│       │   └── PORT-20220505T154512Z
│       ├── ieso
│       ├── midapp
│       └── redcap
├── notebooks
│   └── images
├── r
│   └── data_cleaning
│       ├── flare
│       ├── ieso
│       ├── midapp
│       └── redcap
├── references
├── renv
│   ├── library
│   │   └── R-4.2
│   └── staging
└── reports
    ├── figures
    └── tables
```

File tree created using: <https://tree.nathanfriend.io/>

# Timings

-   screening: data collected from participants at intake via survey and phone call
-   baseline: data from the first survey administered via redcap
-   flare/aff/exp: data from the flare app completed shortly after the baseline survey 
-   mid-app: data from the survey administered during the flare app fear conditioning task break
-   ieso: data provided by ieso (the CBT provider)
-   assessment: data from participants' assessment session administered by ieso
-   treatment: data from participants' treatment sessions administered by ieso
-   follow-up: data from the final survey administered via redcap and sent to participants one month after treatment finished

# Style decisions

-   Naming convention for cleaning scripts: \<step\>\_\<origin\>\_\<explanation\>\_\<measure\>
    -   example: 01_flare_clean_gad
-   Use capitalisation in commented code
-   "exp" at the start of all, and only, expectancy rating variables

# Notes

-   All participants in the full dataset (i.e. before exclusions have been applied) fully consented to taking part, though some did not meet inclusion/exclusion criteria
-   1 participant is missing treatment assessment data

# To do

-   Clean scrambled sentences data

# License

This project is licensed under the terms of the [MIT License](https://github.com/McGregor14/port/blob/main/LICENSE).