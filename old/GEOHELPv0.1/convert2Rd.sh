for i in *.sgml
 do
  echo $i
  j=`echo $i |sed -e "s/.sgml//g"`
  R CMD Sd2Rd $i > Rfiles/$j.Rd
done
