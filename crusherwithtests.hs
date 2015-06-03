-- Notes --
--
-- Things we need
--
-- gameSearch
-- 	moveGenerator
-- 	staticBoardEmulator
-- 	minimaxAlgorithm
-- 	noPruning
--
--
-- generateBoard(n)
--
-- locationPoints
-- http://www.redblobgames.com/grids/hexagons/
-- 	- thinking of using an axial coordinate system
-- 	- 
-- 	px
-- 	py
--
-- move_helpers
-- 	move_forward
-- 	move_back
-- 	can_move
-- 	skip_pawn


----------Test Data-----------------

b3_board = 
	["WWW-WW-------BB-BBB"]
b4_board = ["WWWW-WWW---------------------BBB-BBBB"]
b5_board = ["WWWWW-WWWW-----------------------------------------BBBB-BBBBB"]

b3_history =
	["-WW-----BB---BB----",
	"-WW-----WB---BB--B-",
	"-WW-W----B---BB--B-",
	"-WW-W----W---BB--BB",
	"WWW-W----B---BB--BB",
	"WWW-W----W---BB-BBB",
	"WWW-WW-------BB-BBB"]

b3_board_original = "WWW-WW-------BB-BBB"
b4_board_original = "WWWW-WWW---------------------BBB-BBBB"
b5_board_original = "WWWWW-WWWW-----------------------------------------BBBB-BBBBB"
b6_board_original =
	"WWWWWW-WWWWW-------------------------------------------------------------------BBBBB-BBBBBB"

b3_board_edge ="abcdefghijklmnopqrs"
b4_board_edge = "abcd12345efghijklmnopqrstuvw67890xyz*"

b3_rows = 
	["WWW",
	"-WW-",
	"-----",
	"-BB-",
	"BBB"]

e_coords_r1 = [(0,-2), (1,-2), (2,-2)]

e_coords =
	[[(0,-2), (1,-2), (2,-2)],
	[(-1,-1), (0,-1), (1,-1), (2,-1)],
	[(-2,0), (-1,0), (0,0), (1,0), (2,0)],
	[(-2,1), (-1,1), (0,1), (1,1)],
	[(-2,2), (-1,2), (0,2)]]

e_allpawns =
	[('W',(0,-2)),('W',(1,-2)),('W',(2,-2)),
	('W',(0,-1)),('W',(1,-1)),
	('B',(0,2)),('B',(1,2)),('B',(2,2)),
	('B',(0,1)),('B',(1,1))]

e_all_wPawns = 
	[('W',(0,-2)),('W',(1,-2)),('W',(2,-2)),
	('W',(0,-1)),('W',(1,-1))]

e_all_bPawns = 
	[('B',(0,2)),('B',(1,2)),('B',(2,2)),
	('B',(0,1)),('B',(1,1))]

e_3_x = [[0,1,2],[-1,0,1,2],[-2,-1,0,1,2],[-2,-1,0,1],[-2,-1,0]]
e_3_y = [-2,-1,0,1,2]

e3_history :: [Boardstate]
e3_history =
	[([('W',(1,-2)),('W',(2,-2))],[('B',(-1,0)),('B',(0,0)),('B',(-1,1)),('B',(0,1))]),
	([('W',(1,-2)),('W',(2,-2)),('W',(-1,0))],[('B',(0,0)),('B',(-1,1)),('B',(0,1)),('B',(-1,2))]),
	([('W',(1,-2)),('W',(2,-2)),('W',(0,-1))],[('B',(0,0)),('B',(-1,1)),('B',(0,1)),('B',(-1,2))]),
	([('W',(1,-2)),('W',(2,-2)),('W',(0,-1)),('W',(0,0))],[('B',(-1,1)),('B',(0,1)),('B',(-1,2)),('B',(0,2))]),
	([('W',(0,-2)),('W',(1,-2)),('W',(2,-2)),('W',(0,-1))],[('B',(0,0)),('B',(-1,1)),('B',(0,1)),('B',(-1,2)),('B',(0,2))]),
	([('W',(0,-2)),('W',(1,-2)),('W',(2,-2)),('W',(0,-1)),('W',(0,0))],[('B',(-1,1)),('B',(0,1)),('B',(-2,2)),('B',(-1,2)),('B',(0,2))]),
	([('W',(0,-2)),('W',(1,-2)),('W',(2,-2)),('W',(0,-1)),('W',(1,-1))],[('B',(-1,1)),('B',(0,1)),('B',(-2,2)),('B',(-1,2)),('B',(0,2))])]

e3_boardstate1 :: Boardstate
e3_boardstate1 = ([('W',(0,(-2))),('W',(1,(-2))),('W',(2,(-2))),('W',(0,(-1))),('W',(1,(-1)))],
				[('B',((-1),1)),('B',(0,1)),('B',((-2),2)),('B',((-1),2)),('B',(0,2))] )

e3_boardstate2 :: Boardstate
e3_boardstate2 = ([('W',(1,-2)),('W',(2,-2))],[('B',(-1,0)),('B',(0,0)),('B',(-1,1)),('B',(0,1))])

e3_boardstatetest :: Boardstate
e3_boardstatetest = ([('W',(1,-2)),('W',(2,-2)), ('W', (0,-1))],[('B',(-1,0)),('B',(0,0)),('B',(-1,1)),('B',(0,1))])

maxValueBoardTest:: [(Boardstate, Int)]
maxValueBoardTest = [(([('B',(3,3))],[]), 3),(([('B',(4,4))],[]), 4), (([('B',(2,2))],[]), 2)]

obviousblack :: Boardstate
obviousblack = ([('W',(0,-2)),('W',(1,-2)),('W',(2,-2)),('W',(2,-1)),('W',(0,0))],[('B',(-2,1)),('B',(0,1)),('B',(-2,2)),('B',(-1,2)),('B',(0,2))])

blackwin :: [String]
blackwin = ["WW--W----B--B-B----"]

blacktrap :: [String]
blacktrap = ["WWWBBB-----B-------"]

whitetrapped :: [String]
whitetrapped = ["WWWBBBB------------"]

minMax1000 :: Boardstate
minMax1000 = ([('W',(0,-2)),('W',(1,-2)),('W',(2,-2))],[('B',(-1,0)),('B',(0,-1)),('B',(1,-1)),('B',(2,0))])

startboard3 :: Boardstate
startboard3 = 	([('W',(0,-2)),('W',(1,-2)),('W',(2,-2)),('W',(0,-1)),('W',(1,-1))],[('B',(-1,1)),('B',(0,1)),('B',(-2,2)),('B',(-1,2)),('B',(0,2))])

-------------------------------------------
----------Test Data ----------------------


crusher_q5q8 :: [String] -> Char -> Int -> Int -> [String]
crusher_q5q8 history playerChar maxLevel n = 
	convertBoardstatesToListOfStrings (stateSearch(parseHistory history n) maxLevel n playerChar) n
	
gametest :: [String] -> Int -> Int -> Int -> [String]
gametest history maxLevel n rounds 
	| rounds == 0 		= history
	| mod rounds 2 == 0 = gametest (crusher_q5q8 history 'W' maxLevel n) maxLevel n (rounds-1)
	| otherwise			= gametest (crusher_q5q8 history 'B' maxLevel n) maxLevel n (rounds-1) 

weakwhite :: [String] -> Int -> Int -> Int -> [String]
weakwhite history maxLevel n rounds 
	| rounds == 0 		= history
	| mod rounds 2 == 0 = weakwhite (crusher_q5q8 history 'W' 1 n) maxLevel n (rounds-1)
	| otherwise			= weakwhite (crusher_q5q8 history 'B' maxLevel n) maxLevel n (rounds-1) 

weakblack :: [String] -> Int -> Int -> Int -> [String]
weakblack history maxLevel n rounds 
	| rounds == 0 		= history
	| mod rounds 2 == 0 = weakblack (crusher_q5q8 history 'W' maxLevel n) maxLevel n (rounds-1)
	| otherwise			= weakblack (crusher_q5q8 history 'B' 1 n) maxLevel n (rounds-1) 

play :: Boardstate -> Char -> Int -> Int -> (Pawn, Pawn)
play boardstate playerChar maxLevel n = findMove (stateSearch [boardstate] maxLevel n playerChar ) playerChar


findMove :: [Boardstate] -> Char -> (Pawn, Pawn) 
findMove history playerChar = 
	do
		let board1 = (head history)
		let board2 = (head (tail history))
		let white1 = getWPawns board1
		let black1 = getBPawns board1
		let white2 = getWPawns board2
		let black2 = getBPawns board2
		findMove_helper white1 black1 white2 black2 playerChar

findMove_helper :: [Pawn] -> [Pawn] -> [Pawn] -> [Pawn] -> Char -> (Pawn,Pawn)
findMove_helper w1 b1 w2 b2 playerChar
	| playerChar == 'W'			= findChange w1 w2
	| otherwise					= findChange b1 b2

findChange :: [Pawn] -> [Pawn] -> (Pawn,Pawn)
findChange list1 list2 
	| (head list1) /= (head list2) = (head list2, head list1)
	| otherwise					   = findChange (tail list1) (tail list2)

	

-- PRIORITY FUNCTIONS LIST
----------------------------------------------------------
-- #1 parseCurrentBoard (DONE!) 
--A function that parses a 'single line' board string to a list of strings representing the board

parseCurrentBoard :: String -> Int -> [String]
parseCurrentBoard board n = 
		(clean_upt n (createRows board n []) ++
		clean_upb n (createRows (reverse board) n []))

createRows :: String -> Int -> [String] -> [String]
createRows board n acc
	| null board =  reverse acc
	| otherwise = 
		createRows (drop n board) (n+1) ((take n board):acc)

clean_upt :: Int -> [String] -> [String]
clean_upt n rows_w_dup = clean_up_helpert rows_w_dup n []

clean_up_helpert :: [String] -> Int -> [String] -> [String]
clean_up_helpert rows_w_dup n acc
	| length (last rows_w_dup) == (2*n -1) = take n (rows_w_dup++acc)
	| otherwise = clean_up_helpert (init rows_w_dup) n acc

clean_upb :: Int -> [String] -> [String]
clean_upb n rows_w_dup = map reverse (clean_up_helperb rows_w_dup n [])

clean_up_helperb :: [String] -> Int -> [String] -> [String]
clean_up_helperb rows_w_dup n acc
	| length (last rows_w_dup) == (2*n -2) = reverse(rows_w_dup++acc)
	| otherwise = clean_up_helperb (init rows_w_dup) n acc

-- #2 parseBoardStateFromStrings (DONE!)
-- A function that parses a list of strings representing a board, and returns a BoardState using lists of coordinates
-- for each player.
-- The below algorithm is based on axial coordinates as shown here: https://www.dropbox.com/s/lp53blqlpudjj4d/axial%20coords.png?dl=0
-- If we describe these coords as (x,y), then we can use the 'element number' of the STRING (row) in the list, paired with
-- the element number of the CHARACTER in the row string to determine the coordinates for a particular character. 
-- Recursively iterate through the rows (strings) in the board. This function also takes in a value 'y' that starts out at y = -(n-1).
-- When this function is called increment y by 1. On the final row y will equal (n-1), which is what we want.
-- For the x coordinates, start at x = 0, and when iterating through each row, decrement x by 1 (to a lowest possible value of -(n-1).
-- Then x goes up by one for each character in the string. Cons the coordinate onto the whitelist if 'w', and cons the coordinate onto
-- the blacklist if char = 'b', otherwise just continue iterating through the string and list of strings.

-- generate the y values of the board

getYVals :: Int -> [Int]
getYVals n = [-(n-1) ,(-(n-1) +1) .. (n-1)]
-------------------------------------------------------------------------------------
-- generate a list of x values for each row
getXvals :: Int -> [[Int]]
getXvals n = getXvals_h n [-(n-1), (-(n-1)+1) .. (n-1)] []

getXvals_h :: Int -> [Int] -> [[Int]] -> [[Int]]
getXvals_h n yVals acc
	| null yVals = reverse acc
	| otherwise = 
		getXvals_h n (tail yVals) ((generateXvals n (head yVals)):acc)

generateXvals :: Int -> Int -> [Int]
generateXvals n y
	| y <= 0 		= [(-y -n + 1), (-y - n + 2) .. (n-1)]
	| otherwise 	= [-(n-1), (-(n-1) +1) .. ((n-1) - y)]

-----------------------------------------------------------------------

-- Using the X values and y Values to create a list of coordinates
createCoords :: [[Int]] -> [Int] -> [[Coord]]
createCoords xInts yInts = createCoords_h xInts yInts []

createCoords_h :: [[Int]] -> [Int] -> [[Coord]] -> [[Coord]]
createCoords_h xInts yInts acc
	| null yInts = reverse acc
	| otherwise = createCoords_h (tail xInts) (tail yInts) ((makeListCoords (head xInts) (head yInts)):acc)

makeListCoords :: [Int] -> Int -> [Coord]
makeListCoords xInts yInt = makeListCoords_h xInts yInt []

makeListCoords_h :: [Int] -> Int-> [Coord] -> [Coord]
makeListCoords_h xInts yInt acc
	| null xInts = reverse acc
	| otherwise = makeListCoords_h (tail xInts) yInt (((head xInts),yInt):acc)

-- Helper function to generate all the coordinates of the gamespace
makeCoordRef :: Int -> [[Coord]]
makeCoordRef n = createCoords (getXvals n) (getYVals n)

-- Zipping the coordinates to the characters of the board rows to get boardstate

makeBoards :: [String] -> [[Coord]] -> [Pawn]
makeBoards boardRows coords = makeBoards_h boardRows coords []

makeBoards_h :: [String] -> [[Coord]] -> [Pawn] -> [Pawn]
makeBoards_h boardRows coords acc
	| null boardRows = acc
	| otherwise =
		 makeBoards_h (tail boardRows) (tail coords) (createBoardRow (head boardRows) (head coords))++acc

createBoardRow :: String -> [Coord] -> [Pawn]
createBoardRow row loc = createBoardRow_h row loc []

createBoardRow_h :: String -> [Coord] -> [Pawn] -> [Pawn]
createBoardRow_h row loc acc
	| null row = acc
	| otherwise =
		createBoardRow_h (tail row) (tail loc) (((head row), (head loc)):acc)

createAllpawns :: String -> Int -> [Pawn]
createAllpawns board n = reverse(
						makeBoards (parseCurrentBoard board n)
						(makeCoordRef n))

----------------Get BoardStates -------------------
getBoardState :: [Pawn] -> Boardstate
getBoardState listOfPawns = ((sortAllWPawns listOfPawns), (sortAllBPawns listOfPawns))

sortAllWPawns :: [Pawn] -> [Pawn]
sortAllWPawns listOfPawns = sortAllWPawns_h listOfPawns []

sortAllWPawns_h :: [Pawn] -> [Pawn] -> [Pawn]
sortAllWPawns_h listOfPawns acc
	| null listOfPawns = reverse acc
	| isWPawn(head listOfPawns) = sortAllWPawns_h (tail listOfPawns) ((head listOfPawns):acc)
	| otherwise = sortAllWPawns_h (tail listOfPawns) acc

isWPawn :: Pawn -> Bool
isWPawn pawn = (fst pawn) == 'W'

sortAllBPawns :: [Pawn] -> [Pawn]
sortAllBPawns listOfPawns = sortAllBPawns_h listOfPawns []

sortAllBPawns_h :: [Pawn] -> [Pawn] -> [Pawn]
sortAllBPawns_h listOfPawns acc
	| null listOfPawns = reverse acc
	| isBPawn(head listOfPawns) = sortAllBPawns_h (tail listOfPawns) ((head listOfPawns):acc)
	| otherwise = sortAllBPawns_h (tail listOfPawns) acc

isBPawn :: Pawn -> Bool
isBPawn pawn = (fst pawn) == 'B'

getWPawns :: Boardstate -> [Pawn]
getWPawns boardstate = fst boardstate

getBPawns :: Boardstate -> [Pawn]
getBPawns boardstate = snd boardstate

-- #3 convertBoardHistory (DONE!)
-- A function that takes in the list of board history stings, recursively runs these through function #1,
-- and creates a list of boardstates, [Boardstate], the current boardstate should be the first entry in the list.

--------- Parse Board Histories into List of Board States --------------

parseHistory :: [String] -> Int -> [Boardstate]
parseHistory boardHistory n = parseHistory_h boardHistory n []

parseHistory_h :: [String] -> Int -> [Boardstate] -> [Boardstate]
parseHistory_h boardHistory n acc
	| null boardHistory = reverse acc
	| otherwise =
	 parseHistory_h (tail boardHistory) n 
	 (getBoardState (createAllpawns (head boardHistory) n):acc)

-- #4 convertBoardstatesToStrings (DONE!)
-- A function that takes a board state, and converts it back to string format
-- For this I think it will probably mean iterating in a similar way as is done in #2. Iterate through
-- all coordinates starting with the first row. (0, -(n-1)), (1, -(n-1)) and so on. Then do a check through 
-- all the coordinates in the board state. If white is there, cons a 'w' onto the recursive call, if black then cons a 'b', otherwise cons a '-'.
-- Then you just need to write another function that joins all these row strings into a single string.

convertBoardstateToStrings :: Boardstate -> Int -> String
convertBoardstateToStrings boardstate n = 
		convertToStrings (convertBSToStrings boardstate n)

convertBoardstatesToListOfStrings :: [Boardstate] -> Int -> [String]
convertBoardstatesToListOfStrings lob n = 
	convertBoardstatesToListOfStrings_h lob n []

convertBoardstatesToListOfStrings_h :: [Boardstate] -> Int -> [String] -> [String]
convertBoardstatesToListOfStrings_h lob n acc
	| null lob  = reverse acc
	| otherwise = 
		convertBoardstatesToListOfStrings_h (tail lob) n
					((convertBoardstateToStrings (head lob) n) : acc)

convertBSToStrings :: Boardstate -> Int -> [String]
convertBSToStrings boardstate n = 
	convertBSToStrings_h boardstate n (makeCoordRef n) []

convertBSToStrings_h :: Boardstate -> Int -> [[Coord]] -> [String] -> [String]
convertBSToStrings_h boardstate n lloc acc
	| null lloc = reverse acc
	| otherwise = 
		convertBSToStrings_h 
			boardstate n (tail lloc) 
				((convertRowToString n (head lloc) boardstate):acc)

convertRowToString :: Int -> [Coord] -> Boardstate -> String
convertRowToString n loc boardstate = convertRowToString_h n loc boardstate []

convertRowToString_h :: Int -> [Coord] -> Boardstate -> String -> String
convertRowToString_h n loc boardstate acc
	| null loc = reverse acc
	| otherwise =
		convertRowToString_h n (tail loc) boardstate
		((convertCoordToChar (head loc) boardstate):acc)

convertToStrings :: [String] -> String
convertToStrings los = convertToStrings_h los []

convertToStrings_h :: [String] -> String -> String
convertToStrings_h los acc
	| null los = reverse acc
	| otherwise = convertToStrings_h (tail los) ((reverse (head los))++acc)

-- #5 convertCoordToChar (DONE!)
-- function that takes in a coordinate and a board state, and n, and returns:
--  'W' if white pawn at given coord
--  'B' if black pawn at given coord
--  '-' if coord is empty

convertCoordToChar:: Coord -> Boardstate -> Char
convertCoordToChar coord boardstate
	| elem coord (getWCoords boardstate) = 'W'
	| elem coord (getBCoords boardstate) = 'B'
	| otherwise = '-'

getWCoords :: Boardstate -> [Coord]
getWCoords boardstate = map snd (getWPawns boardstate)

getBCoords :: Boardstate -> [Coord]
getBCoords boardstate = map snd (getBPawns boardstate) 

-- #6 getWinnerByPawnCount (DONE!)
-- function that takes in a board state and n.
-- return 'w' if black pawn count is < n
-- return 'b' if white pawn count is < n
-- otherwise return '-'

getWinnerByPawnCount :: Boardstate -> Int -> Char -> Char
getWinnerByPawnCount boardstate n playerChar
	| length (getBPawns boardstate) >= n && length (getWPawns boardstate) >= n  						= '-'
	| playerChar == 'W' && length (getBPawns boardstate) < n && length (getWPawns boardstate) >= n		= 'W'
	| playerChar == 'B' && length (getWPawns boardstate) < n && length (getBPawns boardstate) >= n		= 'B'
	| length (getBPawns boardstate) < n 					 											= 'W'
	| otherwise 																						= 'B'


enoughPawns :: [Pawn] -> Int -> Bool
enoughPawns lop n
	| (length lop) > ((2*n -1) - n) = True
	| otherwise = False

-- #7 getBoardValue
-- a function that takes in a board state, n, and the player's character 'w' or 'b', and 
-- returns 1000 if win condition for the player, -1000 if win condition for the opponent, and
-- if not a win condition for anyone, then return ((length playerpawnslist) - (length opponentpawnslist))
-- assumes that there are moves available

getOpponentChar :: Char -> Char
getOpponentChar playerChar
	| playerChar == 'W' 	= 'B'
	| otherwise				= 'W'
	
getPlayerPawns :: Boardstate -> Char -> [Pawn]
getPlayerPawns boardstate playerChar
	| playerChar == 'W' 	= getWPawns boardstate
	| otherwise				= getBPawns boardstate
	
getPlayerCoords :: Boardstate -> Char -> [Coord]
getPlayerCoords boardstate playerChar = [snd x | x <- getPlayerPawns boardstate playerChar	]
	

getBoardValueIfMoves :: Boardstate -> Int -> Char -> Int
getBoardValueIfMoves boardstate n playerChar
	| getWinnerByPawnCount boardstate n playerChar == playerChar = 1000
	| getWinnerByPawnCount boardstate n playerChar == getOpponentChar playerChar	= -1000
	| otherwise 														= (length (getPlayerPawns boardstate playerChar)) - (length (getPlayerPawns boardstate (getOpponentChar playerChar)))
	

-- #8.1 isValidCoord
-- takes in a coordinate and n
-- checks if the coordinate is contained within possible board states

isValidCoord :: Coord -> Int -> Bool
isValidCoord coord n = elem coord (getAllValidBoardPositions n)

-- returns a list of all possible coordinates within bounds of n
getAllValidBoardPositions :: Int -> [Coord]
getAllValidBoardPositions n = listAllCoords (makeCoordRef n) []

listAllCoords :: [[Coord]] -> [Coord] -> [Coord]
listAllCoords lloc acc
	| null lloc = reverse acc
	| otherwise = listAllCoords (tail lloc) ((head lloc) ++ acc)


getCharAtCoord :: Coord -> Boardstate -> Int -> Char
getCharAtCoord coord boardstate n
	| elem coord (getWCoords boardstate) = 'W'
	| elem coord (getBCoords boardstate) = 'B'
	| isValidCoord coord n				 = '-'
	| otherwise							 = 'X'	
	

-- #8 getEmptyAdjacentCoords
-- a function that takes in a coordinate (representing a pawn location), and a board state
-- returns a list of adjacent coordinates that are both only one space away and are empty
-- we need n to pass in to keep track of valid coords within boards

getEmptyAdjacentCoords :: Coord -> Boardstate -> Int -> [Coord]
getEmptyAdjacentCoords coord boardstate n = 
	[x | x <- getAdjacentCoords coord, getCharAtCoord x boardstate n == '-'] 

getAdjacentCoords :: Coord -> [Coord]
getAdjacentCoords coord = 
	[((fst coord), (snd coord) - 1), 
	((fst coord), (snd coord) + 1), 
	((fst coord) -1, (snd coord) + 1),
	((fst coord) +1, (snd coord) - 1),
	((fst coord) +1, (snd coord)),
	((fst coord) -1, (snd coord))]	


-- #9 getFriendlyAdjacentCoords
-- a function that takes in a coordinate (representing a pawn location), 'B' or W', and a board state
-- returns a list of adjacent coordinates that contain friendly pawns


	
--getFriendlyAdjacentCoords :: Coord -> Boardstate -> Char -> Int -> [Coord]
--getFriendlyAdjacentCoords coord boardstate playerChar n
--	= [x | x <- getAdjacentCoords coord, getCharAtCoord x boardstate n == playerChar] 
	
-- #10 getJumpingCoords
-- a function that takes a coordinate (representing a pawn location)
-- uses function #9 to return a list of coordinates that can be 'jumped to' by jumping over
-- the player's pawn

getJumpingSet :: Coord -> [(Coord,Coord)]
getJumpingSet coord
	= zip (getAdjacentCoords coord) 
	[((fst coord), (snd coord) - 2), 
	((fst coord), (snd coord) + 2), 
	((fst coord) -2, (snd coord) + 2),
	((fst coord) +2, (snd coord) - 2),
	((fst coord) +2, (snd coord)),
	((fst coord) -2, (snd coord))]	

getValidJumpingCoords :: Coord -> Boardstate -> Char -> Int -> [Coord]
getValidJumpingCoords coord boardstate playerChar n
	= [snd x | x <- getJumpingSet coord, (getCharAtCoord (snd x) boardstate n == getOpponentChar playerChar) ||
										(getCharAtCoord (snd x) boardstate n == '-'),
									  getCharAtCoord (fst x) boardstate n == playerChar]	
	
-- #11 removePawn
-- a function that removes a given coordinate from a list of coordinates, and returns the new list
-- the order of the other elements must stay the same

removePawn :: Coord -> [Coord] -> [Coord]
removePawn coord coordList
	| null coordList 			= []
	| coord == (head coordList)	= tail coordList
	| otherwise					= (head coordList):(removePawn coord (tail coordList))


-- #12 updatePawnCoords
-- a function that takes in an old coordinate, a new coordinate and a list of coordinates,
-- replaces the old coord with the new coord, and returns the new list of coords
-- ** NOTE: This function MUST replace the coordinate in the exact location in the list,
-- otherwise the duplicate board detection function may not work (as a list of coords
-- may be seen as different if they are not in the same order).
-- ** NOTE #2: Replacing the pawns in exactly the same location may not be as critical after we implement
-- function #22, boardStatesEqual
updatePawnCoords :: Coord -> Coord -> [Coord] -> [Coord]
updatePawnCoords oldCoord newCoord coordList
	| null coordList 				= []
	| oldCoord == (head coordList)	= newCoord:(tail coordList)
	| otherwise						= (head coordList):(updatePawnCoords oldCoord newCoord (tail coordList))



-- #13 stateSearch
-- a function that takes in the board history (with the current board at the front of the history), 
-- n, the player's character
-- returns the next player for the player at the start of the board history



-------------------------------

--getBestBoard :: [Boardstate] -> Int -> Int -> Char -> Boardstate
--getBestBoard boardlist boardValue n playerChar
--	| boardValue == getBoardValueIfMoves (head boardlist) n playerChar		= head boardlist
--	| otherwise																= getBestBoard (tail boardlist) boardValue n playerChar
 
maxValueBoard :: [(Boardstate, Int)] -> Boardstate
maxValueBoard pairslist = maxValueBoard_helper (tail pairslist) (head pairslist)

maxValueBoard_helper :: [(Boardstate, Int)] -> (Boardstate, Int) -> Boardstate
maxValueBoard_helper pairslist maxPair
	| null pairslist							= fst maxPair
	| (snd (head pairslist)) > (snd maxPair)	= maxValueBoard_helper (tail pairslist) (head pairslist)
	| otherwise									= maxValueBoard_helper (tail pairslist) maxPair

stateSearch :: [Boardstate] -> Int -> Int -> Char -> [Boardstate]
stateSearch boardhistory maxLevel n playerChar = (minMaxCaller boardhistory 1 maxLevel n playerChar):boardhistory

minMaxCaller :: [Boardstate] -> Int -> Int -> Int -> Char -> Boardstate
minMaxCaller boardhistory level maxLevel n playerChar = 
		maxValueBoard (minMaxCaller_helper boardhistory (generateNewPlayerStates boardhistory n playerChar) level maxLevel n playerChar)

minMaxCaller_helper	:: [Boardstate] -> [Boardstate] -> Int -> Int -> Int -> Char -> [(Boardstate, Int)]
minMaxCaller_helper boardhistory newBoardStates level maxLevel n playerChar
	| null newBoardStates    = []
	| maxLevel == 1			 = ((head newBoardStates), getBoardValueIfMoves (head newBoardStates) n playerChar):(minMaxCaller_helper boardhistory (tail newBoardStates) level maxLevel n playerChar) 
	| otherwise				 = ((head newBoardStates),(minMax ((head newBoardStates):boardhistory) (level+1) maxLevel n playerChar)):(minMaxCaller_helper boardhistory (tail newBoardStates) level maxLevel n playerChar) 
		
minMax :: [Boardstate] -> Int -> Int -> Int -> Char -> Int
minMax boardhistory level maxLevel n playerChar
	| level == maxLevel && mod level 2 == 0								= findMinBoardValue (generateNewOpponentStates boardhistory n playerChar) n playerChar
	| level == maxLevel 												= findMaxBoardValue (generateNewPlayerStates boardhistory n playerChar) n playerChar 
	| mod level 2 == 0													= minValue (minMax_helper boardhistory (generateNewOpponentStates boardhistory n playerChar) (level+1) maxLevel n playerChar)
	| otherwise															= maxValue (minMax_helper boardhistory (generateNewPlayerStates boardhistory n playerChar) (level+1) maxLevel n playerChar)

minMax_helper :: [Boardstate] -> [Boardstate] -> Int -> Int -> Int -> Char -> [Int]
minMax_helper boardhistory newBoardStates level maxLevel n playerChar
	| null newBoardStates = []
	| otherwise			  =	(minMax ((head newBoardStates):boardhistory) level maxLevel n playerChar):(minMax_helper boardhistory (tail newBoardStates) level maxLevel n playerChar) 
	
minValue :: [Int] -> Int
minValue list = minValue_helper list 1000 
	
minValue_helper :: [Int] -> Int -> Int
minValue_helper list min
	| null list				= min
	| (head list) < min		= minValue_helper (tail list) (head list)
	| otherwise 			= minValue_helper (tail list) min
	
maxValue :: [Int] -> Int
maxValue list = maxValue_helper list (-1000) 
	
maxValue_helper :: [Int] -> Int -> Int
maxValue_helper list max
	| null list				= max
	| (head list) > max		= maxValue_helper (tail list) (head list)
	| otherwise 			= maxValue_helper (tail list) max









-------------------------------

-- #14 generateBoardStates
-- takes in board history, n, 'B' or 'W'
-- returns a list of non-duplicate boardstates representing every possible next move for the given set of
-- pawns associated with the input char

--generateSlideMoveStates :: Boardstate -> Int -> Char -> [Boardstate]
--generateSlideMoveStates boardstate n playerChar
--	= do 
--		playerPawns <- getPlayerPawns boardState playerChar

generatePawnSlideStates :: Boardstate -> Coord -> Int -> Char -> [Boardstate]
generatePawnSlideStates boardstate pawnCoord n playerChar
	= do
		let slideCoords = getEmptyAdjacentCoords pawnCoord boardstate n
		generatePawnMoveStates boardstate pawnCoord slideCoords playerChar
		

generatePawnMoveStates :: Boardstate -> Coord -> [Coord] -> Char -> [Boardstate]
generatePawnMoveStates boardstate pawnCoord newCoords playerChar
	| null newCoords = []
	| otherwise		 = (updateBoardState boardstate pawnCoord (head newCoords) playerChar):(generatePawnMoveStates boardstate pawnCoord (tail newCoords) playerChar)
		
		

-- #21 generateJumpingMoveStates
-- takes in a boardstate, n, 'B' or 'W'
-- generates a list of new board states, using function #9, that only includes jump moves
-- this includes both jumps to empty spaces, and jumps to spaces that will remove an enemy pawn
-- the enemy pawn must of course be removed from the board state if this is the case.		
generatePawnJumpStates :: Boardstate -> Coord -> Int -> Char -> [Boardstate]
generatePawnJumpStates boardstate pawnCoord n playerChar =
	do
		let jumpCoords = getValidJumpingCoords pawnCoord boardstate playerChar n
		generatePawnMoveStates boardstate pawnCoord jumpCoords playerChar
	
createPawns :: [Coord] -> Char -> [Pawn]
createPawns coordsList playerChar
	| null coordsList 		= []
	| otherwise				= (playerChar, (head coordsList)):(createPawns (tail coordsList) playerChar)
		
		
updateBoardState :: Boardstate -> Coord -> Coord -> Char -> Boardstate
updateBoardState boardstate oldCoord newCoord playerChar
	| playerChar == 'W' = do
		let enemCoords = getBCoords boardstate
		let newEnemCoords = removePawn newCoord enemCoords
		let playerCoords = updatePawnCoords oldCoord newCoord (getWCoords boardstate)
		((createPawns playerCoords 'W'), (createPawns newEnemCoords 'B'))
	| otherwise = do
		let enemCoords = getWCoords boardstate
		let newEnemCoords = removePawn newCoord enemCoords
		let playerCoords = updatePawnCoords oldCoord newCoord (getBCoords boardstate)
		((createPawns newEnemCoords 'W'), (createPawns playerCoords 'B'))

removeDuplicateBoards :: [Boardstate] -> [Boardstate] -> [Boardstate]
removeDuplicateBoards boardhistory boardlist = [x | x <- boardlist, not (isBoardInPath boardhistory x)]
		

-- #15 generateNewPlayerStates
-- takes in board history, n, player char
-- returns a list of non-duplicate boardstates representing every possible next move for the player
generateNewPlayerStates :: [Boardstate] -> Int -> Char -> [Boardstate]
generateNewPlayerStates boardhistory n playerChar = removeDuplicateBoards boardhistory (generateSlidesAndJumps (head boardhistory) (getPlayerCoords (head boardhistory) playerChar) n playerChar)



generateSlidesAndJumps :: Boardstate -> [Coord] -> Int -> Char -> [Boardstate]
generateSlidesAndJumps boardstate coordList n playerChar
	| null coordList 	= []
	| otherwise			= do
		let slideStates = generatePawnSlideStates boardstate (head coordList) n playerChar
		let jumpStates = generatePawnJumpStates boardstate (head coordList) n playerChar
		slideStates ++ jumpStates ++ (generateSlidesAndJumps boardstate (tail coordList) n playerChar) 

-- #16 generateNewOpponentStates
-- takes in board history, n, opponent char
-- returns a list of non-duplicate boardstates representing every possible next move for the opponent
generateNewOpponentStates :: [Boardstate] -> Int -> Char -> [Boardstate]
generateNewOpponentStates boardhistory n playerChar = removeDuplicateBoards boardhistory (generateNewPlayerStates boardhistory n (getOpponentChar playerChar))

findMinBoardValue :: [Boardstate] -> Int -> Char -> Int
findMinBoardValue boardstateList n playerChar = findMinBoardValue_helper boardstateList n playerChar 1000

-- #17 findMinBoardValue
-- takes in a list of board states, n, and the player's char
-- returns the min board value for the list of boards
findMinBoardValue_helper :: [Boardstate] -> Int -> Char -> Int -> Int
findMinBoardValue_helper boardstateList n playerChar min 
	| null boardstateList 												= min
	| getBoardValueIfMoves (head boardstateList) n playerChar < min 	= findMinBoardValue_helper (tail boardstateList) n playerChar (getBoardValueIfMoves (head boardstateList) n playerChar)
	| otherwise															= findMinBoardValue_helper (tail boardstateList) n playerChar min

-- #18 findMaxBoardValue
-- takes in a list of board states, n, and the player's char
-- returns the max board value for the list of boards
findMaxBoardValue :: [Boardstate] -> Int -> Char -> Int
findMaxBoardValue boardstateList n playerChar = findMaxBoardValue_helper boardstateList n playerChar (-1000)

-- #17 findMaxBoardValue
-- takes in a list of board states, n, and the player's char
-- returns the min board value for the list of boards
findMaxBoardValue_helper :: [Boardstate] -> Int -> Char -> Int -> Int
findMaxBoardValue_helper boardstateList n playerChar max 
	| null boardstateList 												= max
	| getBoardValueIfMoves (head boardstateList) n playerChar > max 	= findMaxBoardValue_helper (tail boardstateList) n playerChar (getBoardValueIfMoves (head boardstateList) n playerChar)
	| otherwise															= findMaxBoardValue_helper (tail boardstateList) n playerChar max


-- #19 isBoardInPath
-- takes in the boardHistory list and another boardstate
-- returns true if the boardstate exists in the boardhistory, otherwise returns false

isBoardInPath :: [Boardstate] -> Boardstate -> Bool
isBoardInPath boardHistory boardstate
	| null boardHistory 									= False
	| areBoardStatesEqual (head boardHistory) boardstate 	= True
	| otherwise												= isBoardInPath (tail boardHistory) boardstate



-- #22 boardStatesEqual
-- Function takes in two board states
-- Returns true only if the board states are the same. Ie., same number of pawns, and same colored pawns are
-- at the same coordinates. Should not matter what order the pawns are in inside the lists.
areBoardStatesEqual :: Boardstate -> Boardstate -> Bool
areBoardStatesEqual boardA boardB = 
	do
		let whitelistA = getWPawns boardA
		let blacklistA = getBPawns boardA
		let whitelistB = getWPawns boardB
		let blacklistB = getBPawns boardB
		(length whitelistA == length whitelistB) && (length blacklistA == length blacklistB) && (arePawnListsEqual whitelistA whitelistB) && (arePawnListsEqual blacklistA blacklistB)
		
arePawnListsEqual :: [Pawn] -> [Pawn] -> Bool
arePawnListsEqual listA listB
	| null listA 				= True 
	| elem (head listA) listB   = arePawnListsEqual (tail listA) listB
	| otherwise					= False





 
-------------------------------------------------------------------------





type Coord = (Int, Int)
type Pawn = (Char, Coord)

-- (whitelist, blacklist)
-- Jer's note: on second thought it might be much easier for later functions if instead of the boardstate being(whitelist, blacklist), it was instead
-- (MyPawnsList, OpponentPawnsList)
-- Jer's note: actually, it might not matter. we could just have a function called getmypawnslist that takes in the board state and the player char ('B' or 'W'), and returns the appropriate list
-- and then a getOpponentsPawnsList function that also takes in the player char, and returns the opposite list



-- Hey I have an idea for generating the coordinates and getting the pawns
-- 	#1 get the longest length of the board
-- 	#2 pass in the board but only get the coordinates from the middle row to 1 half of the board
-- 	#3 call the similar function but on the reverse of the board changing the values accordingly
--  #4 append the 3 list of coordinates into a single list of list of coordinates
--  #5 sort the list of coordinates into the proper rows
-- 	#6 zip the list of coordinates with the original boards to create the pawns <- this will give us the board states
--
type Boardstate = ([Pawn], [Pawn]) -- Q. shouldn't boardState be ([String],Int, Int, Bool, [Boards])
									-- (Board, numWhite, numBlack, isOver, statesTravelled) ??
									-- Jer's note: I think numWhite and numBlack can actually be functions that just return the length of whitelist and blacklist
									-- isOver can be a function too
									-- states travelled will definitely have to be kept track of during the minMax algorithm, but I don't think we need to include it in the board state.
									
-- history is passed as a list of strings
-- where the most recent move is appended to the original board
-- 


{-

# convert cube to axial
q = x
r = z

# convert axial to cube
x = q
z = r
y = -x-z

# convert cube to even-q offset
q = x
r = z + (x + (x&1)) / 2



http://www.gamedev.net/page/resources/_/technical/game-programming/isometric-n-hexagonal-maps-part-i-r747
-}