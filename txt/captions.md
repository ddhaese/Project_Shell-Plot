# Captions

_Figure 1_: Shell plot visualizing the binned count of cancer cells in the blood of 164 anonymized subjects during treatment. Total observation period ranged from a few days to more than 8 years. Hover of the episode sectors to reveal additional information of the episode and linked observation and subject. Click on an episode to copy the data row to the clipboard. Scroll to zoom in and out.
_Figure 2_: Terminology used for the shell plot.
_Figure 3_: Parameterization of the layout for the shell plot. The circled numbers indicate the order in which to calculate the (derived) parameters. See text for more details.
_Figure 4_: Process diagram for the shell plot. Only one input is truly compulsory and that is the observational data with the features `Subject`, `Start`, `End`, and `Group`. Although `Width` and `Height` are strictly layout parameters, they are treated as true input values because of their importance. `id` represents an unique HTML identifier to avoid conflict when working with more than one shell plot per HTML page. The input `gridlines` corresponds to the `at` argument of the `axis()` function. To allow extra information to be shown upon hover, an `infobox` template must be provided. Layout and styling information can be provided as a YAML configuration file. Additional and dynamic styling (e.g. highlighting of sector upon hover) is provided as CSS style sheets.
_Figure 5_: Distribution of the total treatment time (expressed in days since admission) among all subjects. The layout in this plot is hindered by the longer observation period of _Zakariya Tucker_ which makes other observation scale along. In many realistic cases, the distribution of observation periods is even more skewed and leptocurtic then in this dataset.
_Figure 6_: Plotting the subject identifiers of the top 200 or 500 episodes on a canvas containing a number of guidelines.
_Figure 7_: Because the plot is constructed as a SVG element, it consist purely of XML code allowing for all kinds of interactivity through CSS, Javascript and derivatives thereof. If you are viewing this in your browser, you can try and select some text in _Figure 6_.
_Figure 8_: Drawing the upper arc of the highlighted observation in _Figure 2_ using an SVG `<path>` element.
_Figure 9_: Drawing our first annulus sector. 
_Figure 10_: Hover over the sector to reveal the data attached. Mind that at this point no styling has been performed.
_Figure 11_: Example legend.
_Figure 12_: Test of drawing the gridlines (pink).
