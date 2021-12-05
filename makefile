# this builds both the proposal and the project
all:
	make proposal
	make outline
	make report
	make project
	make submission

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

submission:
	# make the build directory
	mkdir -p build
	# make temp directory for submission files
	mkdir -p ./submission
	# copy needed files
	cp ./report/final-report-final.pdf ./submission/report.pdf
	cp ./project/converter.hs ./submission/converter.hs
	cp ./project/doc1.md ./submission/doc1.md
	cp ./project/doc2.md ./submission/doc2.md
	cp ./project/doc3.md ./submission/doc3.md
	# add files to zip
	zip -j ./submission/submission.zip ./submission/doc1.md
	zip -j ./submission/submission.zip ./submission/doc2.md
	zip -j ./submission/submission.zip ./submission/doc3.md
	zip -j ./submission/submission.zip ./submission/converter.hs
	zip -j ./submission/submission.zip ./submission/report.pdf
	# copy zip to build directory
	cp ./submission/submission.zip ./build/submission.zip

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
	# the submission directory
	rm -rf submission
