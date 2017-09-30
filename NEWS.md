# splot 0.3.1

## Additions
* added utility function: splot.color to assign colors by group,
  and splot.bench to plot the run-time of expressions.
* allows for text as x-axis labels in scatter plots
* additional documentation

## Improvements
* changed the way variables are evaluated to accomodated calls to splot
  from within functions.
* improved handling of between variables with uneven levels
* improved variable/level length handling
* improved handling of prediction line errors and some fatal errors
* bar and line plots no longer skip levels of x with only one level of by

## Bug fixes
* now appropriately sorts x-axis labels