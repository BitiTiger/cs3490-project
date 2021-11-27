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
import System.IO (IOMode (ReadMode), hGetContents, openFile)

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

data Block
  = LI [Block] -- List Items
  | UL [Block] -- Unordered List
  | OL [Block] -- Ordered List
  | Heading1 ParagraphText -- Heading 1
  | Heading2 ParagraphText -- Heading 2
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
    OL
      [ LI [Paragraph [Normal "Python"]],
        LI [Paragraph [Normal "JavaScript"]],
        LI [Paragraph [Normal "GoLang"]]
      ]
  ]

-- a demo for nested lists
doc3 :: Document
doc3 =
  [ Heading1 [Normal "The families of programming languages"],
    UL
      [ LI
          [ Paragraph [Normal "Machine Code"],
            OL
              [ LI [Paragraph [Normal "CISC"]],
                LI [Paragraph [Normal "RISC"]]
              ]
          ],
        LI
          [ Paragraph [Normal "Assembly"],
            OL
              [ LI [Paragraph [Normal "ARM"]],
                LI [Paragraph [Normal "X86"]],
                LI [Paragraph [Normal "6502"]]
              ]
          ],
        LI
          [ Paragraph [Normal "High Level"],
            OL
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
        | x `elem` ['0' .. '9'] = q0 xs
        | x == '.' = q1 xs
        | otherwise = False
      q1 [] = True
      q1 xs = False
   in q0 s

-- helper to convert four spaces to tabs
convertSpacesToTabs :: String -> String
convertSpacesToTabs "" = ""
convertSpacesToTabs (' ' : ' ' : ' ' : ' ' : xs) = "\t" ++ convertSpacesToTabs xs
convertSpacesToTabs (x : xs) = x : convertSpacesToTabs xs

-- helper to add spaces between symbols
preproc :: String -> String
preproc "" = "" -- Base Case
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
preproc ('\n' : '\n' : xs) = ' ' : '\n' : '\n': ' ' : preproc xs -- Endline
preproc ('\n' : xs) = ' ' : '\n' : ' ' : preproc xs -- Newline
preproc ('\t' : xs) = ' ' : '\t' : ' ' : preproc xs -- Tab
preproc (x : xs) = x : preproc xs -- GenericText

-- this converts a single string to the correct token
classify :: String -> Token
classify [] = error "Token error: empty string."
classify "#" = H1Op
classify "##" = H2Op
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

-- parser uses some code from lecture
-- parser :: [Token] -> [Block]
-- parser input = sr input []

isUnparsedText :: Token -> Bool
isUnparsedText BoldOp = True 
isUnparsedText ItalicOp = True 
isUnparsedText BoldItalicOp = True 
isUnparsedText StrikethroughOp = True 
isUnparsedText PreformattedOp = True 
isUnparsedText (GenericText t) = True 
isUnparsedText _ = False


-- the shift-reduce helper
sr :: [Token] -> [Token] -> [Block]
sr (Err s : input) _ = error ("Lexical error: " ++ s) -- error case
sr [] [PB b] = [b] -- promote the last block element
--inline rules
sr input (GenericText t   : rs)                                    = sr input (PI (Normal t): rs) -- promote text to normal text
sr input (PI (Normal t2)  : PI (Normal t1)                   : rs) = sr input (PI (Normal (t1 ++ " " ++ t2)): rs)
sr input (BoldOp          : PI (Normal t)  : BoldOp          : rs) = sr input (PI (Bold t): rs) -- promote text to bold text
sr input (ItalicOp        : PI (Normal t)  : ItalicOp        : rs) = sr input (PI (Italic t): rs) -- promote text to italic text
sr input (BoldItalicOp    : PI (Normal t)  : BoldItalicOp    : rs) = sr input (PI (BoldItalic t): rs) -- promote text to bold italic text
sr input (StrikethroughOp : PI (Normal t)  : StrikethroughOp : rs) = sr input (PI (Strikethrough t): rs) -- promote text to strikethrough text
sr input (PreformattedOp  : PI (Normal t)  : PreformattedOp  : rs) = sr input (PI (Preformatted t): rs) -- promote text to preformatted text
sr input (PI i : x : rs) | not (isUnparsedText x) = sr input (PT [i]:x:rs) -- check if it is SAFE to promote the inline element
-- inline rules with paragraph mode
-- sr input (PT pt : GenericText t   : rs)                                    = sr input (PT pt : PI (Normal t): rs) -- promote text to normal text
-- sr input (PT pt : BoldOp          : PI (Normal t)  : BoldOp          : rs) = sr input (PT pt : PI (Bold t): rs) -- promote text to bold text
-- sr input (PT pt : ItalicOp        : PI (Normal t)  : ItalicOp        : rs) = sr input (PT pt : PI (Italic t): rs) -- promote text to italic text
-- sr input (PT pt : BoldItalicOp    : PI (Normal t)  : BoldItalicOp    : rs) = sr input (PT pt : PI (BoldItalic t): rs) -- promote text to bold italic text
-- sr input (PT pt : StrikethroughOp : PI (Normal t)  : StrikethroughOp : rs) = sr input (PT pt : PI (Strikethrough t): rs) -- promote text to strikethrough text
-- sr input (PT pt : PreformattedOp  : PI (Normal t)  : PreformattedOp  : rs) = sr input (PT pt : PI (Preformatted t): rs) -- promote text to preformatted text
sr input (PT pt : PI i : rs)                                               = sr input (PT (i:pt):rs)

--block rules
sr input (PT t : rs) = sr input (PB (Paragraph t):rs)
-- sr input (CodeOp:(PI i):CodeOp:rs) = sr input (PB (Code [i]): rs)
--shift-reduce rules
sr (i:input) stack = sr input (i:stack) -- shift stack
sr [p] stack        = error (show stack) -- ran out of options

-- this splits a token list into several lists for each block element
splitAtBlocks' :: [Token] -> [[Token]]
splitAtBlocks' [] = [[]]
splitAtBlocks' (x:xs) = [r1] ++ splitAtBlocks' r2
  where
    (r1, r2) = span (/= EndBlock) (if x == EndBlock then xs else x:xs) -- [from source 2]

-- this is a wrapper for splitAtBlocks that removes empty blocks from the list before returning
splitAtBlocks :: [Token] -> [[Token]]
splitAtBlocks x = filter (/= []) (splitAtBlocks' x)



--structureToHTML :: [Block] -> String

--generateHTML :: [Block] -> String

-- this is the backbone holding up the other functions
main :: IO ()
main = do
  args <- getArgs -- get system args [from source 1]
  let (infile, outfile) = getFiles args -- get file names
  putStrLn ("Input: " ++ infile) -- show input file name
  putStrLn ("Output: " ++ outfile) -- show output file name
  inHandle <- openFile infile ReadMode -- [from source 3]
  mdText <- hGetContents inHandle -- [from source 3]
  putStrLn "=== INPUT CONTENTS ==="
  print mdText
  let lexed = lexer mdText
  putStrLn "=== LEXED TOKENS ==="
  print lexed
  -- let parsed = parser lexed
  -- putStrLn "=== PARSED TOKENS ==="
  -- print parsed
  --  let html = generateHTML parsed
  --  write html outfile
  print "OK. :)" -- inform user that program is done
