pltgrid <-
function(xgrid=NULL, ygrid=NULL, xpos, ypos, ...)
{
        if(!is.null(xgrid)) {
                if(missing(xpos))
                        xpos <- seq(par()$xaxp[1.], par()$xaxp[2.], length =
                                par()$xaxp[3.] + 1.)
                ypos1 <- xpos
                xpos <- matrix(xpos, length(xpos), 3.)
                xpos[, 3.] <- rep(NA, nrow(xpos))
                ypos1 <- xpos
                ypos1[, 1.] <- par()$usr[3.]
                ypos1[, 2.] <- par()$usr[4.]
                lines(t(xpos), t(ypos1), ...)
        }
        if(!is.null(ygrid)) {
                if(missing(ypos))
                        ypos <- seq(par()$yaxp[1.], par()$yaxp[2.], length =
                                par()$yaxp[3.] + 1.)
                print(ypos)
                ypos <- matrix(ypos, length(ypos), 3.)
                ypos[, 3.] <- rep(NA, nrow(ypos))
                xpos <- ypos
                xpos[, 1.] <- par()$usr[2.]
                xpos[, 2.] <- par()$usr[1.]
                lines(t(xpos), t(ypos), ...)
        }
}

