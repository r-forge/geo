rm -r -f geo
R < skeleton.r --no-save
cd geo
cp ../DESCRIPTION . 
mkdir src
cd src
cp ../../geoR.c geo.c
cd ../R 
cp ../../zzz.R .
cd ../man
cp ../../Dummyhelp/* .
cp ../../GEOHELPv0.1/Rfiles/*.Rd .

cd ../..
R CMD build --binary geo
