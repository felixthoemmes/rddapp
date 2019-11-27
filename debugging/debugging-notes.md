# debugging notes

## bug report 1

**bug 1)** Felix: "In terms of the power analysis, I don't have a reproducible example. In the beta version, it appears to crash every time I try to use the
power analysis that searches over a grid (the lower panel in the shiny
app)."

**bug 2)** dataset causes shiny GUI to crash

_file_
multivarRD.csv

_assignment/cutoffs**_
outcome: mt
assign1: vp >= 20
assign2: mp <= 5
treatm: trt

Felix: "The issue is with error reporting. In your example, the crashes are a result of your design probably not having any observations in some of the quadrants. The software should give you an error message in the design table, and then refuse to estimate anything. I guess it refuses, but doesn't really tell you why."

## bug report 2
*annotated list by Felix*

1)
Model tab:
+ It is not entirely clear what “IF” (with blue background) in the
Treatment Design box means. What’s required here is the assignment
variable and the cutoff score. “IF” probably stands for the logical
condition?

Maybe this was not entirely clear. The IF indeed stands for "Treatment
is assigned IF", and then the user puts in the assignment variable,
the smaller or larger operator, and then the cut-off score.
On the other hand, our description in the help file seems pretty clear
(I think you wrote this). Here is what we write:

The mechanism is described by (a series) of “if” statements, in the
Treatment design panel, under the heading IF. The user selects the
assignment variable in the drop-down menu labeled primary assignment.
The box underneath is used to input the threshold for being assigned
to the treatment. By default, every value of the assignment variable
that is greater than or equal to the threshold leads to an assignment
to the treatment condition. Clicking on the ≥ sign switches the
condition to less than or equal to (≤), meaning that values of the
assignment variable less than or equal to the threshold lead to
assignment.

This seems pretty clear to me (but of course I used this so long that
everything seems clear and obvious to me).

I see two options: a) leave as is, because manual is clear enough, or
b) change the blue "IF" to "Receive treatment if".
Interested in your thoughts here. I don't consider this a bug, but a
matter of taste.



2)
Sensitivity tab:
+ I don’t get any plots unless I explicitly click on each
(re)computation button.

This is the programmed and intended behavior. We decided against
automatic computation, because sometimes it can be very
time-consuming. I still believe this is the best thing to do, but
maybe the button (the little circular arrows) is not the best icon
choice. Another choice would be the "Play" icon (triangle on the
side), but then it's maybe not obvious that you can recompute once you
change values. Again, we do mention this in the handbook. We write:

Note that neither of Figures 4.1 or 4.2 will be generated
automatically; they will only appear after the corresponding blue
circular arrow button is pressed (found in the lower right corner of
each figure panel).

Again, not a bug, but taste. Would be interested in your opinion on
either "circular arrow" icon, or "play" icon.


3)
+ Figure 4.1 & 4.2: The percentage symbol (%) is quite far to the
right of the CI number.

OK, maybe we can change the size of the box where the user inputs the
value of the CI. Naturally this number will almost always be 95, and
sometimes be changed to 99. Currently the box is a bit long. If you
can make is smaller, feel free to do that. If this messes up the
overall design, ignore this point.


4)
+ Figure 4.2: Would be great if one could change the range of the
y-axis because one cannot always see whether the confidence region
actually includes zero or not.

Changing the range of the Y axis is a sensible request. In terms of
the code it should be relatively easy to implement. The code already
has an xlim argument (which is linked to the inputs in the shiny app).
If we were to add a ylim argument, we would need to make it clear to
the user which arguments are for which. We could add a subheading that
reads X-axis, and Y-axis.

5)
Model with Auxiliary Variables:
+ When I enter a non-numeric variable, I get a warning triangle and
the message “non-numeric”. Moreover, instead of Table 1.1, Figure 3.1,
and Figure 4.2 I get error messages.

OK, this is an interesting case, of a user wanting to control for a
factor (or some other non-numeric variable). One way to handle this is
to simply recode this to a (set of) dummy-coded indicator variable(s).
However, this only works if we assume that the non-numeric variables
are always strictly nominal. If a user had a non-numeric variable that
was ordinal, it would still be treated as nominal, and dummy-coded. I
think the best solution for now is to add a sentence to the manual
that auxiliary variables must be numeric, and that if users want to
add non-numeric variables, these should be recoded manually (outside
of the shiny app).

Here is a template that could be included in the help manual.

All auxiliary variables must be numeric, including ordinal and nominal
variables (e.g., factors). Non-numeric variables should be manually
recoded into dummy-indicator variables prior to performing the
analysis.

6)
+ Table 1.2. Choosing cutoff values that are different from the
actually true cutoff values may results in treatment probabilities
greater than 1 or less than 0 (or, I don’t understand what pi
measures).

This appears to be a genuine bug. pi was added as a recommendation by
another consultant. It should just be a conditional probability, and
should not exceed 0 or 1. Can you pull out the relevant line of code
(should be a single statement), examine it, and share with me? This
should hopefully be a simple bug fix.


7)
Model with cluster ID:
+ Next to the cluster ID entry field I got a warning triangle and the
message “singleton.” And, I got an error message for Figure 3.1 (but
no Figure).

This seems to be a warning that a cluster ID has a single (or zero)
observations. I don't think this is a bug, but at the same time I am
not sure what this error message means. Would you be able to
investigate this a bit further. Again, my hunch would be a cluster
with too few observations (likely 1 or zero).

8)
+ NOTE: once I reran everything from scratch (loading the data again)
it just worked fine!

Of course, this is strange. This might be too difficult to debug.
Unless you have a smart idea, ignore for now.


9)
Multivariate RDD:
+ I have still the same issue with the data set I used in April:
ShinyRDD crashes after entering the required cutoff information.

This bug should be squashed now that you have fixed the issue with
"trt". Can you try with the dataset that Peter our consultant attached
to the last email? See if you also get this crash.

10)
+ When I hit the ITT button I no longer get any results, only an error
message (as you are aware).

Are you able to reproduce this error? With the CARE dataset it works.
If you can reproduce this error, that would be a good start.

11)
+ I used your test data but with different assignment rules, i.e., I
used x2 >= 100 instead of x2 <= 75. I think everything should still
work just fine but it doesn’t (there are treatment and control cases
in each of the four cells).
(i) Something is wrong in Table 1.2. I get incorrect and negative
treatment probabilities.
(ii) I don’t understand Figure 3.1 for the univariate approach with
assignment variable x2. Maybe because there are only 4 complier cases;
But also the linear, quadratic and cubic curves on the right side (x2
>= 100) don’t make sense to me.

Let's put this last error on the backburner as well. Sometimes with
crazy wrong assignment rules (and only handful of observations) some
really unusual things can happen.

----------------------

## debugging shiny with r studio

* can only use breakpoints in `shinyServer()`, but `browser()` works everywhere
  * not always helpful as shiny code doesn't execute linearly
  * need to rememeber to take `browser()` statements out after
* use **showcase mode** to ID which parts of code are executing after various actions (to help find where bugs trigger in code)
  * `shiny::runApp(display.mode="showcase")`
  * can also enable by default by setting `DisplayMode: Showcase` in DESCRIPTION file
* can also use the **reactive log**
  * more detailed than showcase mode
    * shows not only which reactives are executing in real time, but the dependencise between them
  * `options(shiny.reactlog=TRUE)`
  * more info [here](https://rstudio.github.io/reactlog
* enter debugger on error
  * `options(shiny.error = browser)`
