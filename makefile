# this builds both the proposal and the project
all:
	make proposal
	make outline
	make report
	make project

# this converts the proposal markdown document into a pdf
# it uses a tool called pandoc to do the conversions but I doubt you have it installed
proposal: proposal/proposal.md
	pandoc -f markdown --pdf-engine=lualatex ./proposal/proposal.md -o ./proposal/proposal-preview.pdf

# this converts the proposal markdown document into a pdf
# it uses a tool called pandoc to do the conversions but I doubt you have it installed
outline: outline/outline.md
	pandoc -f markdown --pdf-engine=lualatex ./outline/outline.md -o ./outline/outline-preview.pdf

# this converts the proposal markdown document into a pdf
# it uses a tool called pandoc to do the conversions but I doubt you have it installed
report: report/final-report.md
	pandoc -f markdown --pdf-engine=lualatex ./report/final-report.md -o ./report/final-report-preview.pdf

# this compiles Haskell code into a binary and puts it in the build directory
project: project/converter.hs
	# run the haskell compiler
	ghc ./project/converter.hs -o ./project/converter
	# make a build directory
	mkdir -p build
	# put binary in build directory
	cp project/converter build/

clean:
	# the proposal pdf
	rm -f proposal/proposal-preview.pdf
	# the outline pdf
	rm -f outline/outline-preview.pdf
	# the final report pdf
	rm -f report/final-report-preview.pdf
	# left over compilation files
	rm -f project/converter.hi project/converter.o
	# the project binary
	rm -f project/converter
	# the project build directory
	rm -rf build
