-- Project #1: Crusher
-- By: Allan Bisnar  17385089
--     Jeremy Krentz 14217129
	

-- TABLE OF CONTENTS
-- Move Generation Logic ........ Line 390
-- Board Evaluation ............. Line 324
-- MiniMax Algorithm ............ Line 554



type Coord = (Int, Int)
type Pawn = (Char, Coord)
type Boardstate = ([Pawn], [Pawn]) 




{-
 Analyse the current state of a Crusher board, and the board history, and
 return the best move to make for the specified player.
 
 history = crusher board history
 playerChar = identifies the current player whose turn it is
 maxLevel = the maximum number of moves ahead (depth) that can be examined
 n = the length of the side of the board. Maximum size we allow is 999.
-}
crusher_q5q8 :: [String] -> Char -> Int -> Int -> [String]
crusher_q5q8 history playerChar maxLevel n = 
	convertBoardstatesToListOfStrings_q5q8 (stateSearch_q5q8(parseHistory_q5q8 history n) maxLevel n playerChar) n



-----------------------------------------------------
-- PARSE SINGLE-LINE STRING BOARD HELPER FUNCTIONS
-- 'single line' board string to a list of strings representing the board

-- Parses a 'single line' board string to a list of strings representing the board
parseCurrentBoard_q5q8 :: String -> Int -> [String]
parseCurrentBoard_q5q8 board n = 
		(clean_upt_q5q8 n (createRows_q5q8 board n []) ++
		clean_upb_q5q8 n (createRows_q5q8 (reverse board) n []))

-- Convert a long string representing a board state into a list
-- of strings where each row of the board is contained in its
-- own string
createRows_q5q8 :: String -> Int -> [String] -> [String]
createRows_q5q8 board n acc
	| null board =  reverse acc
	| otherwise = createRows_q5q8 (drop n board) (n+1) ((take n board):acc)

-- Only include characters for elements actually on the top half of the board
clean_upt_q5q8 :: Int -> [String] -> [String]
clean_upt_q5q8 n rows_w_dup = clean_up_helpert_q5q8 rows_w_dup n []

clean_up_helpert_q5q8 :: [String] -> Int -> [String] -> [String]
clean_up_helpert_q5q8 rows_w_dup n acc
	| length (last rows_w_dup) == (2*n -1) 	= take n (rows_w_dup++acc)
	| otherwise 							= clean_up_helpert_q5q8 (init rows_w_dup) n acc

-- Only include characters for elements actually on the bottom half of the board
clean_upb_q5q8 :: Int -> [String] -> [String]
clean_upb_q5q8 n rows_w_dup = map reverse (clean_up_helperb_q5q8 rows_w_dup n [])

clean_up_helperb_q5q8 :: [String] -> Int -> [String] -> [String]
clean_up_helperb_q5q8 rows_w_dup n acc
	| length (last rows_w_dup) == (2*n -2) 		= reverse(rows_w_dup++acc)
	| otherwise 								= clean_up_helperb_q5q8 (init rows_w_dup) n acc
-----------------------------------------------------
-- PARSING ROW-SEPARATED STRING BOARD HELPER FUNCTIONS
-- For converting a board in string representation to our custom "Boardstate" representation
-- The coordinates use a axial coordinate system as shown here: http://www.redblobgames.com/grids/hexagons/

-- generate the y values of the board
getYVals_q5q8 :: Int -> [Int]
getYVals_q5q8 n = [-(n-1) ,(-(n-1) +1) .. (n-1)]

-- generate a list of x values for each row
getXvals_q5q8 :: Int -> [[Int]]
getXvals_q5q8 n = getXvals_h_q5q8 n [-(n-1), (-(n-1)+1) .. (n-1)] []

getXvals_h_q5q8 :: Int -> [Int] -> [[Int]] -> [[Int]]
getXvals_h_q5q8 n yVals acc
	| null yVals = reverse acc
	| otherwise = 
		getXvals_h_q5q8 n (tail yVals) ((generateXvals_q5q8 n (head yVals)):acc)

-- generate a list of x value coordinates that will exist in a given row
generateXvals_q5q8 :: Int -> Int -> [Int]
generateXvals_q5q8 n y
	| y <= 0 		= [(-y -n + 1), (-y - n + 2) .. (n-1)]
	| otherwise 	= [-(n-1), (-(n-1) +1) .. ((n-1) - y)]

-----------------------------------------------------------------------
-- COORDINATE GENERATION HELPER FUNCTIONS

-- generate the set of all possible axial coordinates for the given board
-- axial coordinate system explained here: http://www.redblobgames.com/grids/hexagons/
createCoords_q5q8 :: [[Int]] -> [Int] -> [[Coord]]
createCoords_q5q8 xInts yInts = createCoords_h_q5q8 xInts yInts []

createCoords_h_q5q8 :: [[Int]] -> [Int] -> [[Coord]] -> [[Coord]]
createCoords_h_q5q8 xInts yInts acc
	| null yInts = reverse acc
	| otherwise  = createCoords_h_q5q8 (tail xInts) (tail yInts) ((makeListCoords_q5q8 (head xInts) (head yInts)):acc)

-- Make a list of coordinates given a single y value, and a list of x values
makeListCoords_q5q8 :: [Int] -> Int -> [Coord]
makeListCoords_q5q8 xInts yInt = makeListCoords_h_q5q8 xInts yInt []

makeListCoords_h_q5q8 :: [Int] -> Int-> [Coord] -> [Coord]
makeListCoords_h_q5q8 xInts yInt acc
	| null xInts = reverse acc
	| otherwise  = makeListCoords_h_q5q8 (tail xInts) yInt (((head xInts),yInt):acc)

-- Helper function to generate all the coordinates of the gamespace
makeCoordRef_q5q8 :: Int -> [[Coord]]
makeCoordRef_q5q8 n = createCoords_q5q8 (getXvals_q5q8 n) (getYVals_q5q8 n)

-- Zip the coordinates to the characters of the board rows to get boardstate
makeBoards_q5q8 :: [String] -> [[Coord]] -> [Pawn]
makeBoards_q5q8 boardRows coords = makeBoards_h_q5q8 boardRows coords []

makeBoards_h_q5q8 :: [String] -> [[Coord]] -> [Pawn] -> [Pawn]
makeBoards_h_q5q8 boardRows coords acc
	| null boardRows = acc
	| otherwise =
		 makeBoards_h_q5q8 (tail boardRows) (tail coords) (createBoardRow_q5q8 (head boardRows) (head coords))++acc

-- Get the list of pawns a given row
createBoardRow_q5q8 :: String -> [Coord] -> [Pawn]
createBoardRow_q5q8 row loc = createBoardRow_h_q5q8 row loc []

createBoardRow_h_q5q8 :: String -> [Coord] -> [Pawn] -> [Pawn]
createBoardRow_h_q5q8 row loc acc
	| null row = acc
	| otherwise =
		createBoardRow_h_q5q8 (tail row) (tail loc) (((head row), (head loc)):acc)

-- Generate the list of all pawns for a given board
createAllpawns_q5q8 :: String -> Int -> [Pawn]
createAllpawns_q5q8 board n = reverse(
						makeBoards_q5q8 (parseCurrentBoard_q5q8 board n)
						(makeCoordRef_q5q8 n))

---------------------------------------------
-- BOARDSTATE HELPER FUNCTIONS

-- Given a list of pawns, return the associated Boardstate
getBoardState_q5q8 :: [Pawn] -> Boardstate
getBoardState_q5q8 listOfPawns = ((sortAllWPawns_q5q8 listOfPawns), (sortAllBPawns_q5q8 listOfPawns))

-- Given a list of white pawns, return the list in the expected order as
-- they would appear on the board from top to bottom, left to right
sortAllWPawns_q5q8 :: [Pawn] -> [Pawn]
sortAllWPawns_q5q8 listOfPawns = sortAllWPawns_h_q5q8 listOfPawns []

sortAllWPawns_h_q5q8 :: [Pawn] -> [Pawn] -> [Pawn]
sortAllWPawns_h_q5q8 listOfPawns acc
	| null listOfPawns = reverse acc
	| isWPawn_q5q8(head listOfPawns) = sortAllWPawns_h_q5q8 (tail listOfPawns) ((head listOfPawns):acc)
	| otherwise = sortAllWPawns_h_q5q8 (tail listOfPawns) acc

-- return true if the pawn is white
isWPawn_q5q8 :: Pawn -> Bool
isWPawn_q5q8 pawn = (fst pawn) == 'W'

-- sort all black pawns in the expected order as
-- they would appear on the board from top to bottom, left to right
sortAllBPawns_q5q8 :: [Pawn] -> [Pawn]
sortAllBPawns_q5q8 listOfPawns = sortAllBPawns_h_q5q8 listOfPawns []

sortAllBPawns_h_q5q8 :: [Pawn] -> [Pawn] -> [Pawn]
sortAllBPawns_h_q5q8 listOfPawns acc
	| null listOfPawns = reverse acc
	| isBPawn_q5q8(head listOfPawns) = sortAllBPawns_h_q5q8 (tail listOfPawns) ((head listOfPawns):acc)
	| otherwise = sortAllBPawns_h_q5q8 (tail listOfPawns) acc

-- return true if the pawn is black
isBPawn_q5q8 :: Pawn -> Bool
isBPawn_q5q8 pawn = (fst pawn) == 'B'

-- get the list of white pawns
getWPawns_q5q8 :: Boardstate -> [Pawn]
getWPawns_q5q8 boardstate = fst boardstate

-- get the list of black pawns
getBPawns_q5q8 :: Boardstate -> [Pawn]
getBPawns_q5q8 boardstate = snd boardstate

-- return the opponent's character
getOpponentChar_q5q8 :: Char -> Char
getOpponentChar_q5q8 playerChar
	| playerChar == 'W' 	= 'B'
	| otherwise				= 'W'

-- get the list of the player's pawns	
getPlayerPawns_q5q8 :: Boardstate -> Char -> [Pawn]
getPlayerPawns_q5q8 boardstate playerChar
	| playerChar == 'W' 	= getWPawns_q5q8 boardstate
	| otherwise				= getBPawns_q5q8 boardstate

-- Get the list of coordinates associated with the player's pawns	
getPlayerCoords_q5q8 :: Boardstate -> Char -> [Coord]
getPlayerCoords_q5q8 boardstate playerChar = 
	[snd x | x <- getPlayerPawns_q5q8 boardstate playerChar]

-- takes in a coordinate and n
-- checks to see if the coordinate is contained within possible board states
isValidCoord_q5q8 :: Coord -> Int -> Bool
isValidCoord_q5q8 coord n = elem coord (getAllValidBoardPositions_q5q8 n)

-- returns a list of all possible coordinates within bounds of n
getAllValidBoardPositions_q5q8 :: Int -> [Coord]
getAllValidBoardPositions_q5q8 n = listAllCoords_q5q8 (makeCoordRef_q5q8 n) []

-- returns a list of all coordinates for a given board
listAllCoords_q5q8 :: [[Coord]] -> [Coord] -> [Coord]
listAllCoords_q5q8 lloc acc
	| null lloc = reverse acc
	| otherwise = listAllCoords_q5q8 (tail lloc) ((head lloc) ++ acc)

-- Return the character associated with a coordinate on the board
-- If 'X' is returned, the coordinate is off the board
getCharAtCoord_q5q8 :: Coord -> Boardstate -> Int -> Char
getCharAtCoord_q5q8 coord boardstate n
	| elem coord (getWCoords_q5q8 boardstate)   = 'W'
	| elem coord (getBCoords_q5q8 boardstate)	= 'B'
	| isValidCoord_q5q8 coord n				 	= '-'
	| otherwise							 		= 'X'	

-------------------------------------------------------------------
-- CONVERT STRING-BOARD HISTORY TO CUSTOM BOARDSTATE HISTORY FORMAT

-- Given a list of strings representing a board history, convert the
-- history to a boardstate history representation using an axial coordinate system
parseHistory_q5q8 :: [String] -> Int -> [Boardstate]
parseHistory_q5q8 boardHistory n = parseHistory_h_q5q8 boardHistory n []

parseHistory_h_q5q8 :: [String] -> Int -> [Boardstate] -> [Boardstate]
parseHistory_h_q5q8 boardHistory n acc
	| null boardHistory = reverse acc
	| otherwise =
		parseHistory_h_q5q8 (tail boardHistory) n 
		(getBoardState_q5q8 (createAllpawns_q5q8 (head boardHistory) n):acc)


-----------------------------------------------------------------
-- CONVERT BOARDSTATE HISTORY TO STRING-BASED HISTORY	 
	 
-- Given a boardstate, return the associated board in string-based format 
convertBoardstateToStrings_q5q8 :: Boardstate -> Int -> String
convertBoardstateToStrings_q5q8 boardstate n = 
		convertToStrings_q5q8 (convertBSToStrings_q5q8 boardstate n)

-- Convert a batch of boardstates to string-based format
convertBoardstatesToListOfStrings_q5q8 :: [Boardstate] -> Int -> [String]
convertBoardstatesToListOfStrings_q5q8 lob n = 
	convertBoardstatesToListOfStrings_h_q5q8 lob n []

convertBoardstatesToListOfStrings_h_q5q8 :: [Boardstate] -> Int -> [String] -> [String]
convertBoardstatesToListOfStrings_h_q5q8 lob n acc
	| null lob  = reverse acc
	| otherwise = 
		convertBoardstatesToListOfStrings_h_q5q8 (tail lob) n
					((convertBoardstateToStrings_q5q8 (head lob) n) : acc)
-- Convert a boardstate to a list of strings, where each string represents a row
convertBSToStrings_q5q8 :: Boardstate -> Int -> [String]
convertBSToStrings_q5q8 boardstate n = 
	convertBSToStrings_h_q5q8 boardstate n (makeCoordRef_q5q8 n) []

convertBSToStrings_h_q5q8 :: Boardstate -> Int -> [[Coord]] -> [String] -> [String]
convertBSToStrings_h_q5q8 boardstate n lloc acc
	| null lloc = reverse acc
	| otherwise = 
		convertBSToStrings_h_q5q8 
			boardstate n (tail lloc) 
				((convertRowToString_q5q8 n (head lloc) boardstate):acc)

-- Convert a single row of the board to string-based format
convertRowToString_q5q8 :: Int -> [Coord] -> Boardstate -> String
convertRowToString_q5q8 n loc boardstate = convertRowToString_h_q5q8 n loc boardstate []

convertRowToString_h_q5q8 :: Int -> [Coord] -> Boardstate -> String -> String
convertRowToString_h_q5q8 n loc boardstate acc
	| null loc = reverse acc
	| otherwise =
		convertRowToString_h_q5q8 n (tail loc) boardstate
		((convertCoordToChar_q5q8 (head loc) boardstate):acc)

-- Convert a list of board-row-strings to one longer string
convertToStrings_q5q8 :: [String] -> String
convertToStrings_q5q8 los = convertToStrings_h_q5q8 los []

convertToStrings_h_q5q8 :: [String] -> String -> String
convertToStrings_h_q5q8 los acc
	| null los = reverse acc
	| otherwise = convertToStrings_h_q5q8 (tail los) ((reverse (head los))++acc)

	
-------------------------------------------------------------
-- FIND THE STATE OF A GIVEN COORDINATE
	
-- Takes in a coordinate and a board state, and n, and returns:
--  'W' if white pawn at given coord
--  'B' if black pawn at given coord
--  '-' if coord is empty
convertCoordToChar_q5q8:: Coord -> Boardstate -> Char
convertCoordToChar_q5q8 coord boardstate
	| elem coord (getWCoords_q5q8 boardstate) = 'W'
	| elem coord (getBCoords_q5q8 boardstate) = 'B'
	| otherwise 							  = '-'

-- Returns the list of all white pawn coordinates
getWCoords_q5q8 :: Boardstate -> [Coord]
getWCoords_q5q8 boardstate = map snd (getWPawns_q5q8 boardstate)

-- Returns the list of all black pawn coordinates
getBCoords_q5q8 :: Boardstate -> [Coord]
getBCoords_q5q8 boardstate = map snd (getBPawns_q5q8 boardstate) 

----------------------------------------------------------
-- EVALUATE THE VALUE OF A BOARD STATE

-- Takes in a board state, n, and a player character
-- return 'w' if black pawn count is < n
-- return 'b' if white pawn count is < n
-- otherwise return '-'
-- The extra guards ensure that if the board state is a goal state for
-- both players, the player whose turn it is will always be identified as the winner
-- This is necessary when doing deep searches that continue past a win condition
getWinnerByPawnCount_q5q8 :: Boardstate -> Int -> Char -> Char
getWinnerByPawnCount_q5q8 boardstate n playerChar
	| length (getBPawns_q5q8 boardstate) >= n 
		&& length (getWPawns_q5q8 boardstate) >= n  				= '-'
	| playerChar == 'W' && length (getBPawns_q5q8 boardstate) < n 
		&& length (getWPawns_q5q8 boardstate) >= n					= 'W'
	| playerChar == 'B' && length (getWPawns_q5q8 boardstate) < n 
		&& length (getBPawns_q5q8 boardstate) >= n					= 'B'
	| length (getBPawns_q5q8 boardstate) < n 						= 'W'
	| otherwise 													= 'B'

-- Takes in a board state, n, and the player's character 'w' or 'b', and 
-- returns 1000 if win condition for the player, -1000 if win condition for the opponent, and
-- if not a win condition for anyone, then return the difference 
-- of the number of player pawns - number of opponent pawns.
-- Assumes that there are moves available
getBoardValueIfMoves_q5q8 :: Boardstate -> Int -> Char -> Int
getBoardValueIfMoves_q5q8 boardstate n playerChar
	| getWinnerByPawnCount_q5q8 boardstate n playerChar == playerChar 						
		= 1000
	| getWinnerByPawnCount_q5q8 boardstate n playerChar == getOpponentChar_q5q8 playerChar	
		= -1000
	| otherwise 																			
		= (length (getPlayerPawns_q5q8 boardstate playerChar))
		   - (length (getPlayerPawns_q5q8 boardstate (getOpponentChar_q5q8 playerChar)))

-- takes in a list of board states, n, and the player's char
-- returns the min board value for the list of boards, 1000 if no moves available
findMinBoardValue_q5q8 :: [Boardstate] -> Int -> Char -> Int
findMinBoardValue_q5q8 boardstateList n playerChar = findMinBoardValue_helper_q5q8 boardstateList n playerChar 1000

findMinBoardValue_helper_q5q8 :: [Boardstate] -> Int -> Char -> Int -> Int
findMinBoardValue_helper_q5q8 boardstateList n playerChar min 
	| null boardstateList
			= 	min
	| getBoardValueIfMoves_q5q8 (head boardstateList) n playerChar < min 	
			=	findMinBoardValue_helper_q5q8 (tail boardstateList) n playerChar 
				(getBoardValueIfMoves_q5q8 (head boardstateList) n playerChar)
	| otherwise	
			=	findMinBoardValue_helper_q5q8 (tail boardstateList) n playerChar min

-- takes in a list of board states, n, and the player's char
-- returns the max board value for the list of boards, -1000 if no moves available
findMaxBoardValue_q5q8 :: [Boardstate] -> Int -> Char -> Int
findMaxBoardValue_q5q8 boardstateList n playerChar = findMaxBoardValue_helper_q5q8 boardstateList n playerChar (-1000)

findMaxBoardValue_helper_q5q8 :: [Boardstate] -> Int -> Char -> Int -> Int
findMaxBoardValue_helper_q5q8 boardstateList n playerChar max 
	| null boardstateList 													
			= max
	| getBoardValueIfMoves_q5q8 (head boardstateList) n playerChar > max 	
			= findMaxBoardValue_helper_q5q8 (tail boardstateList) n playerChar 
				(getBoardValueIfMoves_q5q8 (head boardstateList) n playerChar)
	| otherwise																
			= findMaxBoardValue_helper_q5q8 (tail boardstateList) n playerChar max	
	
-----------------------------------------------------------
-- FIND THE SET OF POSSIBLE NEXT MOVES

-- Takes in a coordinate (representing a pawn location), and a board state
-- returns a list of adjacent coordinates that are only one step away, are empty,
-- and are on the game board.
getEmptyAdjacentCoords_q5q8 :: Coord -> Boardstate -> Int -> [Coord]
getEmptyAdjacentCoords_q5q8 coord boardstate n = 
	[x | x <- getAdjacentCoords_q5q8 coord, getCharAtCoord_q5q8 x boardstate n == '-'] 

-- Return a list of coordinates surrounding the given coordinate
getAdjacentCoords_q5q8 :: Coord -> [Coord]
getAdjacentCoords_q5q8 coord = 
	[((fst coord), (snd coord) - 1), 
	((fst coord), (snd coord) + 1), 
	((fst coord) -1, (snd coord) + 1),
	((fst coord) +1, (snd coord) - 1),
	((fst coord) +1, (snd coord)),
	((fst coord) -1, (snd coord))]	
	
-- Returns a list of coordinates that can be 'jumped to' by jumping over
-- the player's pawn from a given coordinate
getJumpingSet_q5q8 :: Coord -> [(Coord,Coord)]
getJumpingSet_q5q8 coord = 
	zip (getAdjacentCoords_q5q8 coord) 
	[((fst coord), (snd coord) - 2), 
	((fst coord), (snd coord) + 2), 
	((fst coord) -2, (snd coord) + 2),
	((fst coord) +2, (snd coord) - 2),
	((fst coord) +2, (snd coord)),
	((fst coord) -2, (snd coord))]
	
-- Returns the list of valid coordinates that can be jumped to from a given coordinate
-- This includes both empty cells, and cells that contain an enemy pawn
getValidJumpingCoords_q5q8 :: Coord -> Boardstate -> Char -> Int -> [Coord]
getValidJumpingCoords_q5q8 coord boardstate playerChar n
	= [snd x | x <- getJumpingSet_q5q8 coord, 
		(getCharAtCoord_q5q8 (snd x) boardstate n == getOpponentChar_q5q8 playerChar) 
		|| (getCharAtCoord_q5q8 (snd x) boardstate n == '-'),
		getCharAtCoord_q5q8 (fst x) boardstate n == playerChar]	

--------------------------------------------------------
-- GENERATE A NEW BOARDSTATE AFTER MOVING									  
									  
-- Removes a given coordinate from a list of coordinates, and returns the new list of coordinates
removePawn_q5q8 :: Coord -> [Coord] -> [Coord]
removePawn_q5q8 coord coordList
	| null coordList 			= []
	| coord == (head coordList)	= tail coordList
	| otherwise					= (head coordList):(removePawn_q5q8 coord (tail coordList))

-- Takes in an old coordinate, a new coordinate and a list of coordinates,
-- replaces the old coord with the new coord, and returns the new list of coords
updatePawnCoords_q5q8 :: Coord -> Coord -> [Coord] -> [Coord]
updatePawnCoords_q5q8 oldCoord newCoord coordList
	| null coordList 				= []
	| oldCoord == (head coordList)	= newCoord:(tail coordList)
	| otherwise						= (head coordList):(updatePawnCoords_q5q8 oldCoord newCoord (tail coordList))

-- Return the list of all possible board states that can be
-- generated by 'sliding' a given pawn
generatePawnSlideStates_q5q8 :: Boardstate -> Coord -> Int -> Char -> [Boardstate]
generatePawnSlideStates_q5q8 boardstate pawnCoord n playerChar
	= do
		let slideCoords = getEmptyAdjacentCoords_q5q8 pawnCoord boardstate n
		generatePawnMoveStates_q5q8 boardstate pawnCoord slideCoords playerChar

-- Helper function that builds a list of new boardstates when moves are made
generatePawnMoveStates_q5q8 :: Boardstate -> Coord -> [Coord] -> Char -> [Boardstate]
generatePawnMoveStates_q5q8 boardstate pawnCoord newCoords playerChar
	| null newCoords = []
	| otherwise		 = (updateBoardState_q5q8 boardstate pawnCoord (head newCoords) playerChar):
					   (generatePawnMoveStates_q5q8 boardstate pawnCoord (tail newCoords) playerChar)
		

-- Generate a list of new board states, that only includes jump moves
-- this includes both jumps to empty spaces, and jumps to spaces that will remove an enemy pawn
-- the enemy pawn must of course be removed from the board state if this is the case.
generatePawnJumpStates_q5q8 :: Boardstate -> Coord -> Int -> Char -> [Boardstate]
generatePawnJumpStates_q5q8 boardstate pawnCoord n playerChar =
	do
		let jumpCoords = getValidJumpingCoords_q5q8 pawnCoord boardstate playerChar n
		generatePawnMoveStates_q5q8 boardstate pawnCoord jumpCoords playerChar

-- Create a list of pawn 'objects' given a set of coordinates and the player character		
createPawns_q5q8 :: [Coord] -> Char -> [Pawn]
createPawns_q5q8 coordsList playerChar
	| null coordsList 		= []
	| otherwise				= (playerChar, (head coordsList)):(createPawns_q5q8 (tail coordsList) playerChar)
		
-- Generate a new boardstate after moving a pawn to a new coordinate
-- Remove the enemy pawn at the target coorindate (if present)
updateBoardState_q5q8 :: Boardstate -> Coord -> Coord -> Char -> Boardstate
updateBoardState_q5q8 boardstate oldCoord newCoord playerChar
	| playerChar == 'W' = do
		let enemCoords = getBCoords_q5q8 boardstate
		let newEnemCoords = removePawn_q5q8 newCoord enemCoords
		let playerCoords = updatePawnCoords_q5q8 oldCoord newCoord (getWCoords_q5q8 boardstate)
		((createPawns_q5q8 playerCoords 'W'), (createPawns_q5q8 newEnemCoords 'B'))
	| otherwise = do
		let enemCoords = getWCoords_q5q8 boardstate
		let newEnemCoords = removePawn_q5q8 newCoord enemCoords
		let playerCoords = updatePawnCoords_q5q8 oldCoord newCoord (getBCoords_q5q8 boardstate)
		((createPawns_q5q8 newEnemCoords 'W'), (createPawns_q5q8 playerCoords 'B'))

-- Given a set of boardstates and a boardhistory, remove all boardstates from the set
-- that exist in the board history.
removeDuplicateBoards_q5q8 :: [Boardstate] -> [Boardstate] -> [Boardstate]
removeDuplicateBoards_q5q8 boardhistory boardlist = [x | x <- boardlist, not (isBoardInPath_q5q8 boardhistory x)]

-- takes in the boardHistory list and another boardstate
-- returns true if the boardstate exists in the boardhistory, otherwise returns false
isBoardInPath_q5q8 :: [Boardstate] -> Boardstate -> Bool
isBoardInPath_q5q8 boardHistory boardstate
	| null boardHistory 									= False
	| areBoardStatesEqual_q5q8 (head boardHistory) boardstate 	= True
	| otherwise												= isBoardInPath_q5q8 (tail boardHistory) boardstate

-- Takes in two board states and returns true only if the board states are the same. 
-- Ie., same number of pawns, and same colored pawns are
-- at the same coordinates. Should not matter what order the pawns are in inside the lists.
areBoardStatesEqual_q5q8 :: Boardstate -> Boardstate -> Bool
areBoardStatesEqual_q5q8 boardA boardB = 
	do
		let whitelistA = getWPawns_q5q8 boardA
		let blacklistA = getBPawns_q5q8 boardA
		let whitelistB = getWPawns_q5q8 boardB
		let blacklistB = getBPawns_q5q8 boardB
		(length whitelistA == length whitelistB) && 
			(length blacklistA == length blacklistB) && 
			(arePawnListsEqual_q5q8 whitelistA whitelistB) && 
			(arePawnListsEqual_q5q8 blacklistA blacklistB)

-- Returns true if two pawn lists are equal (order is irrelevant)		
arePawnListsEqual_q5q8 :: [Pawn] -> [Pawn] -> Bool
arePawnListsEqual_q5q8 listA listB
	| null listA 				= True 
	| elem (head listA) listB   = arePawnListsEqual_q5q8 (tail listA) listB
	| otherwise					= False

-- takes in board history, n, player char
-- returns a list of non-duplicate boardstates representing every possible next move for the player
generateNewPlayerStates_q5q8 :: [Boardstate] -> Int -> Char -> [Boardstate]
generateNewPlayerStates_q5q8 boardhistory n playerChar = 
	removeDuplicateBoards_q5q8 boardhistory (generateSlidesAndJumps_q5q8 (head boardhistory) 
		(getPlayerCoords_q5q8 (head boardhistory) playerChar) n playerChar)

-- takes in board history, n, player char
-- returns a list of non-duplicate boardstates representing every possible next move for the opponent
generateNewOpponentStates_q5q8 :: [Boardstate] -> Int -> Char -> [Boardstate]
generateNewOpponentStates_q5q8 boardhistory n playerChar = 
	removeDuplicateBoards_q5q8 boardhistory 
		(generateNewPlayerStates_q5q8 boardhistory n (getOpponentChar_q5q8 playerChar))

-- Given a boardstate and a list of coorindates, generate the set of all possible
-- new boardstates by moving pawns that exist in the list of coordinates.
generateSlidesAndJumps_q5q8 :: Boardstate -> [Coord] -> Int -> Char -> [Boardstate]
generateSlidesAndJumps_q5q8 boardstate coordList n playerChar
	| null coordList 	= []
	| otherwise			= do
		let slideStates = generatePawnSlideStates_q5q8 boardstate (head coordList) n playerChar
		let jumpStates = generatePawnJumpStates_q5q8 boardstate (head coordList) n playerChar
		slideStates ++ jumpStates ++ (generateSlidesAndJumps_q5q8 boardstate (tail coordList) n playerChar) 

--------------------------------------------------
-- MiniMax ALGORITHM 
-- As covered in class, this algorithm aims to select the best possible
-- move for the player given a boardstate. It assumes that the opponent
-- will always try to pick the optimal move, and accounts for this.
-- The algorithm only searches as deep as the maxLevel that is specified.
 


-- Given a board history, generate the next optimal move for the player,
-- and add it to the front of the board history.
stateSearch_q5q8 :: [Boardstate] -> Int -> Int -> Char -> [Boardstate]
stateSearch_q5q8 boardhistory maxLevel n playerChar = 
	(miniMaxCaller_q5q8 boardhistory 1 maxLevel n playerChar):boardhistory

-- return the max value associated with 'bubbled-up' values for the branches.
-- returns the same board if no move is possible
miniMaxCaller_q5q8 :: [Boardstate] -> Int -> Int -> Int -> Char -> Boardstate
miniMaxCaller_q5q8 boardhistory level maxLevel n playerChar
	| null (generateNewPlayerStates_q5q8 boardhistory n playerChar)	= head boardhistory 
	| otherwise 													= maxValueBoard_q5q8 (miniMaxCaller_helper_q5q8 boardhistory 
																		(generateNewPlayerStates_q5q8 boardhistory n playerChar) 
																		level maxLevel n playerChar)

-- If we are only able to search one move ahead, don't bother with the miniMax algorithm, just evaluate the top-level boards.
-- Otherwise, start the min-max algorithm and go as deep as required.
miniMaxCaller_helper_q5q8	:: [Boardstate] -> [Boardstate] -> Int -> Int -> Int -> Char -> [(Boardstate, Int)]
miniMaxCaller_helper_q5q8 boardhistory newBoardStates level maxLevel n playerChar
	| null newBoardStates    = []
	| maxLevel == 1			 = ((head newBoardStates), getBoardValueIfMoves_q5q8 (head newBoardStates) n playerChar):
									(miniMaxCaller_helper_q5q8 boardhistory (tail newBoardStates) level maxLevel n playerChar) 
	| otherwise				 = ((head newBoardStates),(miniMax_q5q8 ((head newBoardStates):boardhistory) (level+1) maxLevel n playerChar)):
									(miniMaxCaller_helper_q5q8 boardhistory (tail newBoardStates) level maxLevel n playerChar) 

-- Given a board state, and a maxLevel, evaluate the miniMax algorithm.
-- When evaluating the player's moves, assume the move with the best value.
-- When evaluating the opponent's moves, assume the move with the least value (for us).	
miniMax_q5q8 :: [Boardstate] -> Int -> Int -> Int -> Char -> Int
miniMax_q5q8 boardhistory level maxLevel n playerChar
	| level == maxLevel && mod level 2 == 0		= findMinBoardValue_q5q8 (generateNewOpponentStates_q5q8 boardhistory n playerChar) 
													n playerChar
	| level == maxLevel 						= findMaxBoardValue_q5q8 (generateNewPlayerStates_q5q8 boardhistory n playerChar)
													n playerChar 
	| mod level 2 == 0							= minValue_q5q8 (miniMax_helper_q5q8 boardhistory 
													(generateNewOpponentStates_q5q8 boardhistory n playerChar) (level+1) maxLevel n playerChar)
	| otherwise									= maxValue_q5q8 (miniMax_helper_q5q8 boardhistory 
													(generateNewPlayerStates_q5q8 boardhistory n playerChar) (level+1) maxLevel n playerChar)

-- build the list of values that have 'bubbled-up' from the lower levels.
miniMax_helper_q5q8 :: [Boardstate] -> [Boardstate] -> Int -> Int -> Int -> Char -> [Int]
miniMax_helper_q5q8 boardhistory newBoardStates level maxLevel n playerChar
	| null newBoardStates = []
	| otherwise			  =	(miniMax_q5q8 ((head newBoardStates):boardhistory) level maxLevel n playerChar):
								(miniMax_helper_q5q8 boardhistory (tail newBoardStates) level maxLevel n playerChar) 

-- For the top-level generated boards, associate each board with the
-- value that is 'bubbled-up' from the evaluation of its branches.
maxValueBoard_q5q8 :: [(Boardstate, Int)] -> Boardstate
maxValueBoard_q5q8 pairslist = maxValueBoard_helper_q5q8 (tail pairslist) (head pairslist)

maxValueBoard_helper_q5q8 :: [(Boardstate, Int)] -> (Boardstate, Int) -> Boardstate
maxValueBoard_helper_q5q8 pairslist maxPair
	| null pairslist							= fst maxPair
	| (snd (head pairslist)) > (snd maxPair)	= maxValueBoard_helper_q5q8 (tail pairslist) (head pairslist)
	| otherwise									= maxValueBoard_helper_q5q8 (tail pairslist) maxPair								
								
-- Given a list of ints, return the lowest value in the list
-- If the list is empty, return 1000.	
minValue_q5q8 :: [Int] -> Int
minValue_q5q8 list = minValue_helper_q5q8 list 1000 
	
minValue_helper_q5q8 :: [Int] -> Int -> Int
minValue_helper_q5q8 list min
	| null list				= min
	| (head list) < min		= minValue_helper_q5q8 (tail list) (head list)
	| otherwise 			= minValue_helper_q5q8 (tail list) min

-- Given a list of ints, return the largest value in the list.
-- If the list is empty return -1000.	
maxValue_q5q8 :: [Int] -> Int
maxValue_q5q8 list = maxValue_helper_q5q8 list (-1000) 
	
maxValue_helper_q5q8 :: [Int] -> Int -> Int
maxValue_helper_q5q8 list max
	| null list				= max
	| (head list) > max		= maxValue_helper_q5q8 (tail list) (head list)
	| otherwise 			= maxValue_helper_q5q8 (tail list) max

