module LSystems where

import IC.Graphics
import System.Random
import Data.Fixed

type Rule
  = (Char, String)

type Rules
  = [Rule]

type System
  = (Float, String, Rules)

cross, triangle, arrowHead, peanoGosper,
  dragon, snowflake, tree, bush :: System

type Vertex
  = (Float, Float)

type TurtleState
  = (Vertex, Float)

type Stack
  = [TurtleState]

type ColouredLine
  = (Vertex, Vertex, Colour)

--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
--  Functions for working with systems.

-- |Returns the rotation angle for the given system.
angle :: System -> Float
angle (ang, bse, rle) = ang

-- |Returns the base string for the given system.
base :: System -> String
base (ang, bse, rle) = bse

-- |Returns the set of rules for the given system.
rules :: System -> Rules
rules (ang, bse, rle) = rle


-- |Look up a character in the given set of rules.
--
--  Pre: the character exists in the set of rules.
lookupChar :: Char -> Rules -> String
lookupChar char rules = head [value | (i, value) <- rules, char == i]

-- |Expand a command once using the given set of rules.
expandOne :: Rules -> String -> String
expandOne rules string = concat (map (flip lookupChar rules) string)

-- |Expand a command `n' times using the given set of rules.
expand :: Rules -> String -> Int -> String
expand rules base 0 = base
expand rules base 1 = expandOne rules base
expand rules base n = expandOne rules (expand rules base (n - 1))

-- |Move a turtle.
--
--  * 'F' moves distance 1 in the current direction.
--  * 'L' rotates left according to the given angle.
--  * 'R' rotates right according to the given angle.
move :: Char -> TurtleState -> Float -> TurtleState
move 'L' (vertex, angle) turn = (vertex, (angle + turn))
move 'R' (vertex, angle) turn = (vertex, (angle - turn))
move 'F' ((x, y), angle) turn = ((newX, newY), angle)
  where
    newX       = x + cos angleToRad
    newY       = y + sin angleToRad
    angleToRad = (angle / 180) * pi

-- |Trace lines drawn by a turtle using the given colour, following the
--  commands in the string and assuming the given initial angle of rotation.
--  Method 2 (Stacks)
-- initial TurtleState ((0, 0), 90)
trace :: String -> Float -> Colour -> [ColouredLine]
trace commands@(c : cs) ang colour
  = trace' commands [((0, 0), 90)] ((0, 0), 90) colour
  where
    trace' :: String -> Stack -> TurtleState -> Colour -> [ColouredLine]
    trace' ('[' : cs) tStateStack state col
      = trace' cs (state : tStateStack) state col
    trace' (']' : cs) (top : stack) state col
      = trace' cs stack top col
    trace' ('F' : cs) tStateStack state@(point, a) colour
      = (point, newPoint, nColour) : trace' cs tStateStack state' nColour
      where
        nColour                = rgbColour colour
        state'@(newPoint, ang) = move 'F' state ang
    trace' (c : cs) tStateStack state col
      = trace' cs tStateStack state' col
      where
        state' = move c state ang
    trace' [] _ _ _
      = []

-- Functon creates LSystem with alternating line colours of red, green and blue
rgbColour :: Colour -> Colour
rgbColour (r, g, b)
  | r > 0.0   = (0.0, 0.0, 1.0)
  | g > 0.0   = (1.0, 0.0, 0.0)
  | b > 0.0   = (0.0, 1.0, 0.0)
  | otherwise = (1.0, 1.0, 1.0)

--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

--  Some test systems.

cross
  = ( 90
    , "M-M-M-M"
    , [ ('M', "M-M+M+MM-M-M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

triangle
  = ( 90
    , "-M"
    , [ ('M', "M+M-M-M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

arrowHead
  = ( 60
    , "N"
    , [ ('M', "N+M+N")
      , ('N', "M-N-M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

peanoGosper
  = ( 60
    , "M"
    , [ ('M', "M+N++N-M--MM-N+")
      , ('N', "-M+NN++N+M--M-N")
      , ('+', "+")
      , ('-', "-")
      ]
    )

dragon
  = ( 45
    , "MX"
    , [ ('M', "A")
      , ('X', "+MX--MY+")
      , ('Y', "-MX++MY-")
      , ('A', "A")
      , ('+', "+")
      , ('-', "-")
      ]
    )

snowflake
  = ( 60
    , "M--M--M"
    , [ ('M', "M+M--M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

tree
  = ( 45
    , "M"
    , [ ('M', "N[-M][+M][NM]")
      , ('N', "NN")
      , ('[', "[")
      , (']', "]")
      , ('+', "+")
      , ('-', "-")
      ]
    )

bush
  = ( 22.5
    , "X"
    , [ ('X', "M-[[X]+X]+M[+MX]-X")
      , ('M', "MM")
      , ('[', "[")
      , (']', "]")
      , ('+', "+")
      , ('-', "-")
      ]
    )

mapper :: Rules
mapper
  = [ ('M', "F")
    , ('N', "F")
    , ('X', "")
    , ('Y', "")
    , ('A', "")
    , ('[', "[")
    , (']', "]")
    , ('+', "L")
    , ('-', "R")
    ]

lSystem :: System -> Int -> String
lSystem (_, base, rs) n
  = expandOne mapper (expand rs base n)

drawLSystem :: System -> Int -> Colour -> IO ()
drawLSystem system n colour
  = drawLines (trace (lSystem system n) (angle system) colour)
