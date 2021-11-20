-- This file is the group effort of Cameron Himes and Chase Faine

{-
	Import Statements
-}
import System.Environment
import System.IO

{-
	Data Type Definitions
-}
type Text = String

data Inline
  = Normal Text -- normal text
  | Bold Text -- bold text
  | Italic Text -- italic text
  | BoldItalic Text -- bold and italic text
  | Preformatted Text -- inline code

data Block
  = LI [Block] -- List Items
  | UL [Block] -- Unordered List
  | OL [Block] -- Ordered List
  | Heading1 [Inline] -- Heading 1
  | Heading2 [Inline] -- Heading 2
  | Paragraph [Inline] -- Paragraph
  | Code [Inline] -- Code block

type Document = [Block]

data Token
  = Star -- used for bold/italic
  | Dash -- used for unordered lists
  | BackTick -- used for code/preformatted text
  | NewLine -- used to shorten lines or end a block
  | B Block -- preparsed Block type
  | I Inline -- preparsed Inline type
  | T Text -- preparsed Text type

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
      [ Normal "This is not a normal paragraph. I can make things",
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
      [ Paragraph [Normal "Python"],
        Paragraph [Normal "JavaScript"],
        Paragraph [Normal "GoLang"]
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

--lexer :: String -> [Token]
--parser :: [Tokens] -> [Block]
--printHTML :: [Block] -> String

-- this is the backbone holding up the other functions
main :: IO ()
main = do
  args <- getArgs -- get system args
  let (infile, outfile) = getFiles args -- get file names
  putStrLn ("Input: " ++ infile) -- show input file name
  putStrLn ("Output: " ++ outfile) -- show output file name
  inHandle <- openFile infile ReadMode
  mdText <- hGetContents inHandle
  print mdText
  --  let lexed = lexer mdText
  --  let parsed = parser lexed
  --  let html = printHTML parsed
  --  write html outfile
  print "OK." -- inform user that program is done
