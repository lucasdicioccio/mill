# Mill

A minimal command line tool to filter tabular datasets.

This is more a pet project which might be useful that an industrial-strength
project.  The goal was to match performance close to awk for a very limited use
case (that is filter columnar data on some values).  After doing an extensive
number of data analyses using Laborantin (see Laborantin:
https://github.com/lucasdicioccio/laborantin ), I realized that
Laborantin::Table are simple and useful, but often my goal is to heavily subset
the data, for this, Ruby is a bit slow.  I used to place some awk scripts to
handle this case, awk is great and fast, but I hat to use column numbers when
all I want to give is a column name.  Hence, 'voilà' Mill, to 
save me precious seconds in the future and to practice my Haskell.


# Installation

Currently, you have to build Mill manually.

	git clone https://github.com/lucasdicioccio/mill.git
	cd mill
	cabal install

# Examples

	mill --infos < input-file.txt 				# will give you the list of recognized column names for the input (e.g., gender age company)
	mill --filter "age > 25" "gender = M" < input-file.txt 	# will write matching lines of input to STDOUT

