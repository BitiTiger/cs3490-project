-- This file is the group effort of Cameron Himes and Chase Faine

-- imports
import System.Environment

-- data types
type Text = String
data Inline = Normal Text       -- normal text
            | Bold Text         -- bold text
            | Italic Text       -- italic text
            | Preformatted Text -- inline code
data Block = UL [Block]         -- Unordered List
           | OL [Block]         -- Ordered List
           | Paragraph [Inline] -- Paragraph
           | Code [Inline]      -- Code block
type Document = [Block]
data Token = Star               -- used for bold/italic
           | Dash               -- used for unordered lists
           | BackTick           -- used for code/preformatted text
           | NewLine            -- used to shorten lines or end a block
           | B Block            -- preparsed Block type
           | I Inline           -- preparsed Inline type
           | T Text             -- preparsed Text type

-- function definitions

-- this gets the file names from the user
getFiles :: [String] -> (String, String)
getFiles []       = error "Error: No files provided."       -- no files
getFiles [i]      = error "Error: No output file provided." -- one file
getFiles [i,o]    = (i, o)                                  -- two files
getFiles (i:o:xs) = error "Error: Too many files provided." -- N files

--lexer :: String -> [Token]
--parser :: [Tokens] -> [Block]
--printHTML :: [Block] -> String

-- this is the backbone holding up the other functions
main :: IO ()
main = do
    args <- getArgs                       -- get system args
    let (infile, outfile) = getFiles args -- get file names
    putStrLn ("Input: " ++ infile)        -- show input file name
    putStrLn ("Output: " ++ outfile)      -- show output file name
--  let mdText = read infile
--  let lexed = lexer mdText
--  let parsed = parser lexed
--  let html = printHTML parsed
--  write html outfile
    print "OK."                           -- inform user that program is done
