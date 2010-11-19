SMB.std.background <-
function(depth, depthcol = 1, depthlty = 1, depthlwd = 1, eyjar, depthlab,
        depthlabcsi = 0.12, ...)
{
        SMB.limits <- list(lat = c(62.85, 67.5), lon = c(-27.8, -9.8))
        geoplot(xlim = SMB.limits, ...)
        if(!missing(depth))
                gbplot(depth, depthcol, depthlty, depthlwd, depthlab,
                        depthlabcsi)
        if(!missing(eyjar))
                geolines(eyjar, ...)
}

