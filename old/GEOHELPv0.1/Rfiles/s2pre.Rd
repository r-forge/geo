\name{s2pre}
\alias{s2pre}
\title{
  Writes out data.frame or matrix to a prelude-file.
}
\description{
Data.frame or matrix object is written to a  prelude-file,
       that inherits names/dimnames attributes from the object.
}
\usage{
s2pre(data, file="splus.pre", na.replace="")
}
\arguments{

\item{data}{
Data.frame or matrix object.
}
\item{file}{
Name of the output file ("Splus.pre" by default).
}
\item{na.replace}{
A character to replace NA with in the output  file
       ("" by default).
}
}
\value{
A  prelude-file  representation  of  the  data-object   is
       written to a file.
}
\section{Side Effects}{
No warning is given if the filename "file" already  exists
       -- it is simply over-written.
}
\seealso{
\code{\link{cat}},
\code{\link{write}}.
}
\examples{
Within Splus:
       > tmp.test.frame
          tolur1     tolur2 stafir1
        1     11 0.04625551       a
        2     12 0.04845815       a
        3     13 0.05066079      NA
        4     14 0.05286344       a
        5     15 0.05506608       a
        6     16 0.05726872       b
        7     17 0.05947137       b
        8     18         NA       b
        9     19 0.06387665       b
       10     20 0.06607930       b
       > s2pre(tmp.test.frame,file="prufa.pre",na.replace="-1")
       >

       From UNIX:

       hafbitur/home/reikn/gardar/Papers/Methods95 [435] cat prufa.pre
       linu_nofn       tolur1  tolur2  stafir1
       ---------       ------  ------  -------
       1       11      0.04625551      a
       2       12      0.04845815      a
       3       13      0.05066079      -1
       4       14      0.05286344      a
       5       15      0.05506608      a
       6       16      0.05726872      b
       7       17      0.05947137      b
       8       18      -1      b
       9       19      0.06387665      b
       10      20      0.06607930      b
       hafbitur/home/reikn/gardar/Papers/Methods95 [436]
}
% docclass is function
% Converted by Sd2Rd version 43268.
