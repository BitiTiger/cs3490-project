# this converts the proposal markdown document into a pdf
# it uses a tool called pandoc to do the conversions but I doubt you have it installed
proposal:
	pandoc -f markdown --pdf-engine=lualatex proposal.md -o proposal.pdf
