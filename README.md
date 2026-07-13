# ShowMyData: Repeated Measures – Multiple Measures

![ShowMyData Repeated Measures – Multiple Measures](images/view.png)

**ShowMyData** is a collection of free, open-source Shiny applications for creating publication-quality data visualizations.

This application creates graphs for repeated-measures designs involving three or more measurements per participant. Individual trajectories, summary statistics, and extensive customization options make it well suited for longitudinal and within-subject data.

---

## Launch the app

**https://showmydata.org**

---

## Run locally

```r
install.packages(c(
  "shiny",
  "psychometric",
  "stringr",
  "tidyr",
  "readr",
  "gsheet",
  "colourpicker",
  "rclipboard"
))
```

```r
shiny::runGitHub(
  repo = "smd_multiplerepeats",
  username = "ShowMyData",
  subdir = "multiplerepeats"
)
```

---

## Download the source code

To download the source code from GitHub:

1. Click the green **Code** button near the top of this repository.
2. Choose **Download ZIP**.
3. Unzip the downloaded folder.

---

## About ShowMyData

ShowMyData is an open-source collection of interactive Shiny applications that make it easy to create elegant, data-rich visualizations for research, teaching, and publication. Our guiding principle is simple: **show the data**. By making individual observations visible whenever practical, the apps help viewers see what is really present in the data.

Learn more at:

**https://showmydata.org**

---

## Citation

If you use this software in research or teaching, please cite:

> Wilmer, J. B. (2022). *Data Visualization Web Apps* (Version 2.0) [Web Apps]. ShowMyData. https://showmydata.org

---

## License

This software is licensed under the GNU Affero General Public License v3.0 (AGPL-3.0).

---

## Feedback

Bug reports, feature requests, and contributions are welcome through the GitHub Issues page.
