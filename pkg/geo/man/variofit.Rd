\name{variofit}
\alias{variofit}
\title{
  Function that fits a model to a variogram.
}
\description{
Function that fits a model to a  variogram.   The  fitting
       occurs   either   automatically   or  interactively.   The
       function is called  after  the  function  variogram  which
       calculates  the  variogram  to  which the model is fitted.
       Currently only spherical model is supported.  Later  other
       models will be added.
}
\usage{
variofit(vagram, model=1, option=2, interactivt=F, sill=0)
}
\arguments{
\item{vagram}{
       List with the calculated variogram. Components  of  the
       list  are:          \$dist   mean distance of the interval.
       <s-example>
              \$vario  calculated   value   of   the    variogram.
              \$number number of datapoints in the interval.
       </s-example> 
       In nearly all cases vagram will be  the  output  from  the
       program variogram.
}
\item{model}{
Type of model.  Default is spherical.  It is  currently
       the only model supported.
}
\item{option}{
Method to use in automatic fitting.  Allowed values are
       1,2,3 and 4.  Default value is 2.  For further information
       see below.
}
\item{interactivt}{
If T the fitting is done interactively by plotting
       the variogram on the screen and asking the  user to select
       sill, range and nugget by the locator function.
}
}
\section{Value}{
<s-example>
       A list with the following  components.     
        \$nugget   :   Estimated  nugget effect
        \$sill     :   Estimated sill
        \$range    :   Estimated  range        
        \$dist     :   mean distance  of the interval.
        \$vario    :   calculated value of the variogram.
        \$number   :   number of  datapoints in the interval.
</s-example>
}
\section{Side Effects}{
 No side effects.  X11 window  has  to  be  open  when  the
       program is called.
}
\section{Details}{
<s-example>
       With  automatic  fitting  the  program  fits  a  spherical
       variogram  to  the data by one of 4 different method.  All
       of the methods are based on fitting the variogram  by  the
       "supsmu"  of  Splus  that  is  a robust smoother. When the
       "supsmu" function is used  it  should  not   matter  which
       method  is  used  to calculate the variogram, the "supsmu"
       function handles outliers.  The 4 method for fitting are:

              1: Sill = maximum of smoothed variogram. Range the
                        corresponding distance.         
              2: Sill = first maximum of smoothed  variogram.
                        Range the corresponding  distance.
              3: Sill = Second maximum of smoothed variogram. Range
                        the corresponding distance.
              4: Sill  given. Range is the distance when the
                       smoothed variogram  reaches  the  sill.
              Good choice of sill can be var(z)

       2 is the default value but of these method 2 and 4 seem
       the most useful. The user can plot the variogram and the
       fitted curve by the function plvar. Then the fitted curve
       can be changed by changing the components of the list
       returned by the program.

       When using the program the user should keep in mind that
       only two parameters determine the interpolated values,
       i.e. range and nugget/sill. The absolute value of the
       sill is only important if the estimation variance is to be
       found.
</s-example>
}
\seealso{
\code{\link{variogram}},
\code{\link{pointkriging}}.
}
% docclass is function
% Converted by Sd2Rd version 43268.
