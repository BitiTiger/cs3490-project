-- This file is the group effort of Cameron Himes and Chase Faine

{-
  Sources (note: lines from these sources are marked with "from source #")

  [1]: https://wiki.haskell.org/Tutorials/Programming_Haskell/Argument_handling
  [2]: https://stackoverflow.com/a/20482547Submit
  [3]: https://stackoverflow.com/a/7867786
-}

{-
	Import Statements
-}

import System.Environment (getArgs) -- [from source 1]
-- [from source 1]
import System.IO (IOMode (ReadMode, WriteMode), hGetContents, openFile, hClose, hPutStrLn) -- [from source 3]

{-
	Data Type Definitions
-}
type Text = String

data Inline
  = Normal Text -- normal text
  | Bold Text -- bold text
  | Italic Text -- italic text
  | BoldItalic Text -- bold and italic text
  | Strikethrough Text -- strikethrough text
  | Preformatted Text -- inline code
  deriving (Show, Eq)

type ParagraphText = [Inline] -- a paragraph is made of many inline elements

data ListType
  = OrderedList -- used for unordered lists
  | UnorderedList -- used for ordered lists
  deriving (Show, Eq)

data Block
  = LI [Block] -- List Items
  | List ListType Integer [Block] -- Lists (type, indentation, items)
  | Heading1 ParagraphText -- Heading 1
  | Heading2 ParagraphText -- Heading 2
  | Heading3 ParagraphText -- Heading 3
  | Heading4 ParagraphText -- Heading 4
  | Heading5 ParagraphText -- Heading 5
  | Heading6 ParagraphText -- Heading 6
  | Paragraph ParagraphText -- a paragraph is made of text
  | Code ParagraphText -- Code block
  deriving (Show, Eq)

type Document = [Block]

data Token
  = BoldOp -- used for bold
  | ItalicOp -- used for italic
  | BoldItalicOp -- used for bold and italic
  | PreformattedOp -- used for preformatted
  | StrikethroughOp -- used for strikethrough
  | CodeOp -- used for code
  | Dash -- used for unordered lists
  | OLOp -- used for ordered lists
  | Tab -- used for indentation
  | NewLine -- used to shorten lines
  | EndBlock -- used to end a block
  | H1Op -- used for level 1 headings
  | H2Op -- used for level 2 headings
  | H3Op -- used for level 3 headings
  | H4Op -- used for level 4 headings
  | H5Op -- used for level 5 headings
  | H6Op -- used for level 6 headings
  | PB Block -- preparsed Block type
  | PI Inline -- preparsed Inline type
  | PT [Inline] -- preparsed ParagraphText
  | GenericText Text -- generic Text type
  | Err String -- error token
  deriving (Show, Eq)

{-
	Examples
-}

-- a demo for nested lists
doc1 :: Document
doc1 =
  [ Heading1 [Normal "This is my first header"],
    Heading2 [Normal "This tests", Bold "bold", Normal "text."],
    Heading2 [Normal "This tests", Italic "italic", Normal "text."],
    Heading2 [Normal "This tests", BoldItalic "bold and italic", Normal "text."],
    Heading2 [Normal "This tests", Preformatted "preformatted", Normal "text."],
    Paragraph
      [ Normal "This is",
        Strikethrough "not",
        Normal "a normal paragraph. I can make things",
        Bold "bold",
        Normal "or",
        Italic "italic",
        Normal "or",
        BoldItalic "both",
        Normal ". I can even have",
        Preformatted "preformatted",
        Normal "text. Isn't markdown cool?!"
      ]
  ]

-- a demo for ordered lists
doc2 :: Document
doc2 =
  [ Heading1 [Normal "The Top 3 Programming Languages"],
    List
      OrderedList
      0
      [ LI [Paragraph [Normal "Python"]],
        LI [Paragraph [Normal "JavaScript"]],
        LI [Paragraph [Normal "GoLang"]]
      ]
  ]

-- a demo for nested lists
doc3 :: Document
doc3 =
  [ Heading1 [Normal "The families of programming languages"],
    List
      UnorderedList
      0
      [ LI
          [ Paragraph [Normal "Machine Code"],
            List
              OrderedList
              1
              [ LI [Paragraph [Normal "CISC"]],
                LI [Paragraph [Normal "RISC"]]
              ]
          ],
        LI
          [ Paragraph [Normal "Assembly"],
            List
              OrderedList
              1
              [ LI [Paragraph [Normal "ARM"]],
                LI [Paragraph [Normal "X86"]],
                LI [Paragraph [Normal "6502"]]
              ]
          ],
        LI
          [ Paragraph [Normal "High Level"],
            List
              OrderedList
              1
              [ LI [Paragraph [Normal "C"]],
                LI [Paragraph [Normal "Haskell"]],
                LI [Paragraph [Normal "JavaScript"]]
              ]
          ]
      ]
  ]

{-
	Function Definitions
-}

-- this gets the file names from the user
getFiles :: [String] -> (String, String)
getFiles [] = error "Error: No files provided." -- no files
getFiles [i] = error "Error: No output file provided." -- one file
getFiles [i, o] = (i, o) -- two files
getFiles (i : o : xs) = error "Error: Too many files provided." -- N files

-- helper to check for ordered lists
isValidOrderedList :: String -> Bool
isValidOrderedList s =
  let q0 "" = False
      q0 (x : xs)
        | x `elem` ['0' .. '9'] = q1 xs
        | otherwise = False
      q1 "" = False
      q1 (x : xs)
        | x `elem` ['0' .. '9'] = q1 xs
        | x == '.' = q2 xs
        | otherwise = False
      q2 [] = True
      q2 xs = False
   in q0 s

-- helper to convert four spaces to tabs
convertSpacesToTabs :: String -> String
convertSpacesToTabs "" = ""
convertSpacesToTabs (' ' : ' ' : ' ' : ' ' : xs) = "\t" ++ convertSpacesToTabs xs
convertSpacesToTabs (x : xs) = x : convertSpacesToTabs xs

-- helper to add spaces between symbols
preproc :: String -> String
preproc "" = "" -- Base Case
preproc ('#' : '#' : '#' : '#' : '#' : '#' : xs) = ' ' : '#' : '#' : '#' : '#' : '#' : '#' : ' ' : preproc xs -- Header6 Case
preproc ('#' : '#' : '#' : '#' : '#' : xs) = ' ' : '#' : '#' : '#' : '#' : '#' : ' ' : preproc xs -- Header5 Case
preproc ('#' : '#' : '#' : '#' : xs) = ' ' : '#' : '#' : '#' : '#' : ' ' : preproc xs -- Header4 Case
preproc ('#' : '#' : '#' : xs) = ' ' : '#' : '#' : '#' : ' ' : preproc xs -- Header3 Case
preproc ('#' : '#' : xs) = ' ' : '#' : '#' : ' ' : preproc xs -- Header2 Case
preproc ('#' : xs) = ' ' : '#' : ' ' : preproc xs -- Header1 Case
preproc ('*' : '*' : '*' : xs) = ' ' : '*' : '*' : '*' : ' ' : preproc xs -- BoldItalic Case
preproc ('*' : '*' : xs) = ' ' : '*' : '*' : ' ' : preproc xs -- Bold Case
preproc ('*' : xs) = ' ' : '*' : ' ' : preproc xs -- Italic Case
preproc ('_' : '_' : '_' : xs) = ' ' : '_' : '_' : '_' : ' ' : preproc xs -- BoldItalic Case
preproc ('_' : '_' : xs) = ' ' : '_' : '_' : ' ' : preproc xs -- Bold Case
preproc ('_' : xs) = ' ' : '_' : ' ' : preproc xs -- Italic Case
preproc ('`' : '`' : '`' : xs) = ' ' : '`' : '`' : '`' : ' ' : preproc xs -- Code Case
preproc ('`' : xs) = ' ' : '`' : ' ' : preproc xs -- Preformatted Case
preproc ('-' : xs) = ' ' : '-' : ' ' : preproc xs -- Dash
preproc ('~' : '~' : xs) = ' ' : '~' : '~' : ' ' : preproc xs -- Strikethrough Case
preproc ('\n' : '\n' : xs) = ' ' : '\n' : '\n' : ' ' : preproc xs -- Endline
preproc ('\n' : xs) = ' ' : '\n' : ' ' : preproc xs -- Newline
preproc ('\t' : xs) = ' ' : '\t' : ' ' : preproc xs -- Tab
preproc (x : xs) = x : preproc xs -- GenericText

-- this converts a single string to the correct token
classify :: String -> Token
classify [] = error "Token error: empty string."
classify "#" = H1Op
classify "##" = H2Op
classify "###" = H3Op
classify "####" = H4Op
classify "#####" = H5Op
classify "######" = H6Op
classify "***" = BoldItalicOp
classify "**" = BoldOp
classify "*" = ItalicOp
classify "___" = BoldItalicOp
classify "__" = BoldOp
classify "_" = ItalicOp
classify "```" = CodeOp
classify "`" = PreformattedOp
classify "-" = Dash
classify "~~" = StrikethroughOp
classify "\n\n" = EndBlock
classify "\n" = NewLine
classify "\t" = Tab
classify x
  | isValidOrderedList x = OLOp
  | otherwise = GenericText x

-- this removes the preceding space from a string (ex: " foo" -> "foo")
removeSpaceFront :: String -> String
removeSpaceFront (' ' : x) = x
removeSpaceFront x = x

-- this is a custom words function that does not remove tabs or newlines
splitAtWords' :: String -> [String]
splitAtWords' "" = []
splitAtWords' ('\t' : xs) = "\t" : splitAtWords' xs -- tab
splitAtWords' ('\n' : xs) = "\n" : splitAtWords' xs -- newline
splitAtWords' xs = r1 : splitAtWords' r2
  where
    (r1, r2) = span (/= ' ') (removeSpaceFront xs) -- [from source 2]

-- this is a wrapper for splitAtWords that removes empty strings from the list before returning
splitAtWords :: String -> [String]
splitAtWords x = filter (/= "") (splitAtWords' x)

-- this is the actual lexer
lexer :: String -> [Token]
lexer s = map classify (splitAtWords (preproc (convertSpacesToTabs s)))

-- this checks if the next token is unparsed text
isUnparsedText :: Token -> Bool
isUnparsedText BoldOp = True
isUnparsedText ItalicOp = True
isUnparsedText BoldItalicOp = True
isUnparsedText StrikethroughOp = True
isUnparsedText PreformattedOp = True
isUnparsedText (GenericText t) = True
isUnparsedText _ = False

-- this checks if there might be another list in an array
containsList :: [Block] -> Bool
containsList [] = False
containsList (x : xs) = case x of
  (LI {}) -> True -- NOTE: Lists are INSIDE of list items, so I used LI instead of List
  _ -> containsList xs

-- this tries to add a new child list to an existing list (iteratively)
mapLists :: [Block] -> Block -> [Block]
mapLists [] (List t2 i2 o2) = [List t2 i2 o2]
mapLists ((List t1 i1 o1) : xs) (List t2 i2 o2) =
  if containsList xs
    then List t1 i1 o1 : mapLists xs (List t2 i2 o2)
    else mergeLists (List t1 i1 o1) (List t2 i2 o2) : xs
mapLists (x : xs) (List t2 i2 o2) = x : mapLists xs (List t2 i2 o2)

-- this tries to add a child list to an existing list (recursively)
mergeLists :: Block -> Block -> Block
mergeLists (List t1 i1 o1) (List t2 i2 o2)
  | i1 == i2 && t1 == t2 = List t1 i1 (o1 ++ o2) -- they match, so merge them
  | otherwise = List t1 i1 (mapLists o1 (List t2 i2 o2)) -- try to insert recursively
mergeLists _ _ = error "both arguments must be lists"

--                     p          os        pos
addParagraphToList :: Block -> [Block] -> [Block]
addParagraphToList (Paragraph p) [] = [LI [Paragraph p]]
addParagraphToList (Paragraph p) (o : os) =
  if containsList os
    then o : addParagraphToList (Paragraph p) os
    else case o of
      LI [Paragraph p2] -> LI [Paragraph (p2 ++ p)] : os
      LI [_] -> LI [o, Paragraph p] : os

-- the shift-reduce helper
sr :: [Token] -> [Token] -> Block
sr (Err s : input) _ = error ("Lexical error: " ++ s) -- error case
sr [] [PB b] = b -- promote the last block element
sr input (NewLine : rs) = sr input rs
--inline rules
sr input (GenericText t : rs) = sr input (PI (Normal t) : rs) -- promote text to normal text
sr input (PI (Normal t2) : PI (Normal t1) : rs) = sr input (PI (Normal (t1 ++ " " ++ t2)) : rs)
sr input (BoldOp : PI (Normal t) : BoldOp : rs) = sr input (PI (Bold t) : rs) -- promote text to bold text
sr input (ItalicOp : PI (Normal t) : ItalicOp : rs) = sr input (PI (Italic t) : rs) -- promote text to italic text
sr input (BoldItalicOp : PI (Normal t) : BoldItalicOp : rs) = sr input (PI (BoldItalic t) : rs) -- promote text to bold italic text
sr input (StrikethroughOp : PI (Normal t) : StrikethroughOp : rs) = sr input (PI (Strikethrough t) : rs) -- promote text to strikethrough text
sr input (PreformattedOp : PI (Normal t) : PreformattedOp : rs) = sr input (PI (Preformatted t) : rs) -- promote text to preformatted text
--block rules
sr input (PI i : x : rs) | not (isUnparsedText x) = sr input (PB (Paragraph [i]) : x : rs) -- check if it is SAFE to promote the inline element
sr input [PI i] = sr input [PB (Paragraph [i])] -- if the only thing left is a single text, promote it to a paragraph
sr input (PB (Paragraph p) : PI i : rs) = sr input (PB (Paragraph (i : p)) : rs) -- append text to a paragraph
sr input (PB (Paragraph p2) : PB (Paragraph p1) : rs) = sr input (PB (Paragraph (p1 ++ p2)) : rs) -- merge two paragraphs together
sr input (PB (Paragraph p) : H1Op : rs) = sr input (PB (Heading1 p) : rs) -- convert paragraph to heading 1
sr input (PB (Paragraph p) : PB (Heading1 h) : rs) = sr input (PB (Heading1 (h ++ p)) : rs) -- merge paragraph into heading 1
sr input (PB (Paragraph p) : H2Op : rs) = sr input (PB (Heading2 p) : rs) -- convert paragraph to heading 2
sr input (PB (Paragraph p) : PB (Heading2 h) : rs) = sr input (PB (Heading2 (h ++ p)) : rs) -- merge paragraph into heading 2
sr input (PB (Paragraph p) : H3Op : rs) = sr input (PB (Heading3 p) : rs) -- convert paragraph to heading 3
sr input (PB (Paragraph p) : PB (Heading3 h) : rs) = sr input (PB (Heading3 (h ++ p)) : rs) -- merge paragraph into heading 3
sr input (PB (Paragraph p) : H4Op : rs) = sr input (PB (Heading4 p) : rs) -- convert paragraph to heading 4
sr input (PB (Paragraph p) : PB (Heading4 h) : rs) = sr input (PB (Heading4 (h ++ p)) : rs) -- merge paragraph into heading 4
sr input (PB (Paragraph p) : H5Op : rs) = sr input (PB (Heading5 p) : rs) -- convert paragraph to heading 5
sr input (PB (Paragraph p) : PB (Heading5 h) : rs) = sr input (PB (Heading5 (h ++ p)) : rs) -- merge paragraph into heading 5
sr input (PB (Paragraph p) : H6Op : rs) = sr input (PB (Heading6 p) : rs) -- convert paragraph to heading 6
sr input (PB (Paragraph p) : PB (Heading6 h) : rs) = sr input (PB (Heading6 (h ++ p)) : rs) -- merge paragraph into heading 6
sr input (CodeOp : PB (Paragraph p) : CodeOp : rs) = sr input (PB (Code p) : rs) -- convert paragraph to code block
sr input (PB (Paragraph p) : OLOp : rs) = sr input (PB (List OrderedList 0 [LI [Paragraph p]]) : rs) -- promote paragraph to ordered list
sr input (PB (Paragraph p) : Dash : rs) = sr input (PB (List UnorderedList 0 [LI [Paragraph p]]) : rs) -- promote paragraph to unordered list
sr input (PB (Paragraph p) : PB (List t i o) : rs) = sr input (PB (List t i (addParagraphToList (Paragraph p) o)) : rs) -- merge paragraph into list
sr input (PB (List t i o) : Tab : rs) = sr input (PB (List t (i + 1) o) : rs) -- increase indentation on a list
sr input (PB (List t2 i2 o2) : PB (List t1 i1 o1) : rs) = sr input (PB (mergeLists (List t1 i1 o1) (List t2 i2 o2)) : rs) -- merge the lists
--shift-reduce rules
sr (i : input) stack = sr input (i : stack) -- shift stack
sr input stack = error (show input ++ show stack) -- ran out of pattern matches
sr [p] stack = error (show stack) -- ran out of options

-- this splits a token list into several lists for each block element
splitAtBlocks' :: [Token] -> [[Token]]
splitAtBlocks' [] = [[]]
splitAtBlocks' (x : xs) = r1 : splitAtBlocks' r2
  where
    (r1, r2) = span (/= EndBlock) (if x == EndBlock then xs else x : xs) -- [from source 2]

-- this is a wrapper for splitAtBlocks that removes empty blocks from the list before returning
splitAtBlocks :: [Token] -> [[Token]]
splitAtBlocks x = filter (/= []) (splitAtBlocks' x)

-- parses each block individually
parseEach :: [[Token]] -> [Block]
parseEach [[]] = []
parseEach [x] = [sr x []]
parseEach (x : xs) = sr x [] : parseEach xs

-- parser uses some code from lecture
parser :: [Token] -> [Block]
parser input = parseEach (splitAtBlocks input)

-- convert inline elements to an HTML string
inlineToHTML :: [Inline] -> String
inlineToHTML [] = ""
inlineToHTML (x : xs) = case x of
  (Normal t) -> t ++ " " ++ inlineToHTML xs
  (Bold t) -> " <strong>" ++ t ++ "</strong> " ++ inlineToHTML xs
  (Italic t) -> " <em>" ++ t ++ "</em> " ++ inlineToHTML xs
  (BoldItalic t) -> " <strong><em>" ++ t ++ "</em></strong> " ++ inlineToHTML xs
  (Strikethrough t) -> " <del>" ++ t ++ "</del> " ++ inlineToHTML xs
  (Preformatted t) -> " <span style='font-family:monospace'>" ++ t ++ "</span> " ++ inlineToHTML xs

-- convert inline elements back to their original form
codeParagraphToHTML :: [Inline] -> String
codeParagraphToHTML [] = ""
codeParagraphToHTML (x : xs) = case x of
  (Normal t) -> t ++ codeParagraphToHTML xs
  (Bold t) -> "**" ++ t ++ "**" ++ codeParagraphToHTML xs
  (Italic t) -> "*" ++ t ++ "*" ++ codeParagraphToHTML xs
  (BoldItalic t) -> "***" ++ t ++ "***" ++ codeParagraphToHTML xs
  (Strikethrough t) -> "~~" ++ t ++ "~~" ++ codeParagraphToHTML xs
  (Preformatted t) -> "`" ++ t ++ "`" ++ codeParagraphToHTML xs

-- convert blocks to an HTML string
blockToHTML :: [Block] -> String
blockToHTML [] = ""
blockToHTML (x : xs) = case x of
  (LI t) -> " <li>" ++ blockToHTML t ++ "</li> " ++ blockToHTML xs
  (List UnorderedList _ t) -> " <ul>" ++ blockToHTML t ++ "</ul> " ++ blockToHTML xs
  (List OrderedList _ t) -> " <ol>" ++ blockToHTML t ++ "</ol> " ++ blockToHTML xs
  (Heading1 t) -> " <h1>" ++ inlineToHTML t ++ "</h1> " ++ blockToHTML xs
  (Heading2 t) -> " <h2>" ++ inlineToHTML t ++ "</h2> " ++ blockToHTML xs
  (Paragraph t) -> " <p>" ++ inlineToHTML t ++ "</p> " ++ blockToHTML xs
  (Code t) -> " <p style='font-family:monospace'>" ++ codeParagraphToHTML t ++ "</h1> " ++ blockToHTML xs

-- this generates the entire HTML webpage
generateHTML :: [Block] -> String
generateHTML x =
  let top = "<!doctype html><html lang='en'><head><meta charset='utf-8'><title>My Webpage</title></head><body>"
      middle = blockToHTML x
      bottom = "</body></html>"
   in top ++ middle ++ bottom

-- this is the backbone holding up the other functions
main :: IO ()
main = do
  -- get file paths
  args <- getArgs -- get system args [from source 1]
  let (infile, outfile) = getFiles args -- get file names
  putStrLn ("Input: " ++ infile) -- show input file name
  putStrLn ("Output: " ++ outfile) -- show output file name
  -- read the file
  inHandle <- openFile infile ReadMode -- [from source 3]
  mdText <- hGetContents inHandle -- [from source 3]
  putStrLn "=== INPUT CONTENTS ==="
  print mdText
  -- run the lexer
  let lexed = lexer mdText
  putStrLn "=== LEXED TOKENS ==="
  print lexed
  -- run the parser
  let parsed = parser lexed
  putStrLn "=== PARSED TOKENS ==="
  print parsed
  -- run the converter
  let html = generateHTML parsed
  -- write the html file
  outHandle <- openFile outfile WriteMode -- [inferred from source 3]
  hPutStrLn outHandle html -- [inferred from source 3]
  -- close handles
  hClose inHandle -- [from source 3]
  hClose outHandle -- [from source 3]
  -- end the program
  print "OK. :)" -- inform user that program is done
