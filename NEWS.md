# splot 0.3.2

## Additions
* split by a specified number of segments.

## Bug fixes
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
