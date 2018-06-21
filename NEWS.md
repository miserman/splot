# splot 0.3.3

## Additions
* place the legend interactively.
* adjust color opacity, and assign colors in gradients.
* split by specified values.
* display predicted probability lines in scatter plots with 2-level ys.
* allows for non-numeric y variables.

## Improvements
* general improvements in automatic settings
* more reliable connection between data and line colors, types, and widths.
* provides more control over the legend and colorby.
* improved reading of the y and su arguments.
* improved bar plot y-axis labels.

## Bug fixes
* now appropriately recycles short variables.
* avoids an error when levels of by are missing within between splits.

# splot 0.3.2

## Additions
* sort text x variables by their y values.
* split by a specified number of segments.

## Improvements
* cleared up sub/sud related mixups.
* level ordering now applies to between variables.
* improved formula parsing; expressions should be better recognized
  as arguments.
* rearranged arguments to be more like other functions; variables other
  that y must be named, or entered as part of the y formula.
* multiple variables entered as y can now be renamed/ordered with an mv entry
  in levels.

## Bug fixes
* corrected y-axis labeling for bar plots when autori is true.
* custom notes now overwrite all other note elements,
* specified x levels are now appropriately applied when mv.as.x is true.
* standardized lty behavior across types, and prevented it from causing errors.
* fixed an issue with longer vectors entered directly as arguments.
* multiple y variables with the same name are now treated as different.

# splot 0.3.1

## Additions
* added colorby argument, offering a different way to assign colors.
* added utility functions: splot.color to assign colors by group,
  and splot.bench to plot the run-time of expressions.
* allows for text as x-axis labels in scatter plots.

## Improvements
* changed the way variables are evaluated to accommodated calls to splot
  from within functions.
* improved handling of between variables with uneven levels.
* improved variable/level length handling.
* improved handling of prediction line errors and some fatal errors.
* bar and line plots no longer skip levels of x with only one level of by.

## Bug fixes
* corrected occasional mismatch between x labels and data.
