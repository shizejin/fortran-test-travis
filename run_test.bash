for i in *.f90
do
	gfortran "$i" "test/test_$i" -o "test_${i%.f90}"
	"./test_${i%.f90}" < "test/test_${i%.f90}.dat"
done
