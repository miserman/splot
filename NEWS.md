# splot 0.3.2

## Additions
* split by a specified number of segments.

## Bug fixes
* specified x levels are now appropriately applied when mv.as.x is true
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
