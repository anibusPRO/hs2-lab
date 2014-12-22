import Students


first' (x:xs) = x

smaller x y
	| x < y = True
	| x > y = False
	| otherwise = False

sort [] studentAttr = []
sort (x:xs) studentAttr = sort [y | y <- xs, (smaller (studentAttr y) (studentAttr x)) == True ] studentAttr ++ [x] ++ sort [y | y <- xs, (smaller (studentAttr y) (studentAttr x)) == False ] studentAttr

studentsSortYear = sort students year

oneYearStudents [] = []
oneYearStudents (x:xs) = oneYearStudents[y | y <- xs, ((year y) == (year x)) == True] ++ [x]

yearArrays arr = 
	yearArrays' arr [(oneYearStudents arr)]

yearArrays' [] finalArray = finalArray
yearArrays' (x:xs) finalArray = 
	if (length xs > 0)
		then
			if (((year (first' xs) ) == (year x)) == False)
				then yearArrays' xs (finalArray ++ [oneYearStudents xs])
				else yearArrays' xs finalArray
		else finalArray

sortArrays arr = qsort arr

qsort [] = [] 
qsort (x:xs) = qsort [y | y <- xs, (length y) < (length x) ] ++ [x] ++ qsort [y | y <- xs, (length y) >= (length x)]




start =  do
	mapM_ putStrLn   ( map (\( arrst) -> show (length arrst) ++ " студент " ++ show (year (first' arrst)  ) ++ " " ++ mainHelp arrst " " ) (sortArrays ( yearArrays studentsSortYear)  ) )

mainHelp (x:xs) finalStr = 
	if (length xs > 0)
		then mainHelp xs ( finalStr ++ " " ++ name x)
		else finalStr  ++ " " ++ name x