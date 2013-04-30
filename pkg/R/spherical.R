"spherical" <-
function(rang1, sill, nugget, x)
{
        x <- x/rang1
        return((((sill - nugget) * (1.5 * x - 0.5 * x^3) + nugget) * (1 - sign(
                x - 1)))/2 + (sill * (1 + sign(x - 1)))/2)
}

