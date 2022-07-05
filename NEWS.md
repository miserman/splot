# splot 0.5.3

## Additions
* adds splot.colorcontrast.

## Improvements
* allows for more control over graphical parameters.

## Bug fixes
* appropriately drops levels when they are based on a factor

# splot 0.5.2

## Additions
* dark option to more easily set text colors for dark backgrounds.

## Improvements
* standardizes color codes before passing them to par.
* avoids overwriting par color settings.
* better handles arguments outside of options when options is specified.
* treats logical y variables as binary rather than factors.

# splot 0.5.1

## Additions
* check equivalency of expression outputs within splot.bench.

## Improvements
* handles input with as.matrix but not as.data.frame methods.
* adds a cutoff option and relative timings to splot.bench for better
  display, and includes more in its return.
* splot.bench now evaluates within the global environment so loaded
  packages can be used.

# splot 0.5.0

## Additions
* specify how colors are interpolated with color.summary.
* calculate average colors with the splot.colormean function.
* fill the area under each density lines with its color with
  density.fill = TRUE (default), adjust the fill's opacity
  with density.opacity, and further control the densities with
  arguments in density.args (which replaces the old bw and adj
  arguments).

## Improvements
* broadens colorby applicability and flexibility.
* tweaks default spacing rules to maximize plot areas.
* aligns axis label specification for density with other types.
* better retains or infers names for multiple variables.
* avoids an error when colorby's by has missing values.

## Bug fixes
* corrects several colorby assignment issues.
* handles mixed-type multiple ys by converting non-numeric variables
  to numeric when other numeric variables are included.
* maintains variable ordering when displaying counts of categorical ys.
* includes colorby variable name in split note when split.

# splot 0.4.2

## Additions
* assign colors in the scale of numeric values with splot.color's
  grade argument.

## Improvements
* makes a legend for colorby in more cases.
* allows renaming and reordering of character or factor y levels.
* more broadly retains character and factor level orders.
* more broadly accounts for missing values.
* better handles colorby x aggregation.
* splot.color returns shorter color codes when opacity is 1.

## Bug fixes
* corrects application of adjustments in splot.color when no sampling
  method is used.
* corrects connected line paths when sparse.
* calculates frequencies of y levels within by and between splits.
* corrects some colorby x and by assignments.
* preserves color assignments when colorby has multiple arguments,
  and some levels of by are empty.
* avoids error when a level of split is empty.

# splot 0.4.1

## Improvements
* allows splot.color sampling to be turned off.
* allows splot.bench to handle single runs.
* enables more sorting options.

## Bug fixes
* correctly does not display error bars when error is set to FALSE.
* corrects the x-axis range of scatter plots when x is logical.
* corrects bet reassignment given multiple ys.

# splot 0.4.0

## Additions
* place the legend interactively.
* adjust color opacity, and assign colors in gradients.
* split by specified values.
* display predicted probability lines in scatter plots with 2-level ys.
* allows for non-numeric y variables.

## Improvements
* general improvements in automatic settings.
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
* custom notes now overwrite all other note elements.
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
