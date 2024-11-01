import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

{- 1 -}
cities :: RoadMap -> [City]  
--cities rm = undefined -- TODO:
cities rm = Data.List.nub [c | (c1, c2, _) <- rm, c <- [c1, c2]] -- FIXME: Are we allowed to use Data.List.nub?

{- 2 -}
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent [] _ _ = False
areAdjacent (rm:rms) c1 c2 | (ac1 == c1 && ac2 == c2) || (ac1 == c2 && ac2 == c1) = True
                     | otherwise = areAdjacent rms c1 c2
                     where (ac1, ac2, _) = rm

{- 3 -}
distance :: RoadMap -> City -> City -> Maybe Distance
distance [] _ _ = Nothing
distance (rm:rms) c1 c2 | (ac1 == c1 && ac2 == c2) || (ac1 == c2 && ac2 == c1) = Just d
                     | otherwise = distance rms c1 c2
                     where (ac1, ac2, d) = rm
                
{- 4 -}
adjacent :: RoadMap -> City -> [(City,Distance)] 
adjacent = undefined  -- TODO:

-- createPath :: Path -> [(City, City)]
-- createPath p = [(c1,c2) | (c1,c2) <- zip p (tail p)]


{- 5 -}
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance = undefined  -- TODO:
-- pathDistance rm [] = Just 0
-- pathDistance [] _ = Nothing
-- pathDistance rm (p:ps) = Just (distance rm p c2 + pathDistance rm ps)
--                   where c2 = head ps
                        
                    
{- 6 -}
-----------------------------------------------------------------------
-- 1. Create a list that maps each city to its number of occurrences --
-----------------------------------------------------------------------
-- Function that adds all the cities to a list (with duplicates), given a RoadMap
citiesToList :: RoadMap -> [City]
citiesToList [] = []
citiesToList ((c1, c2, _):xs) = c1 : c2 : citiesToList xs 

-- Helper function that adds a city to a *sorted* list that counts each occurrence of an element.
-- This function takes a city and a list of cities and corresponding degrees and adds the city to that list.
addToOccList :: City -> [(City, Int)] -> [(City, Int)]
addToOccList c1 [] = [(c1, 1)]
addToOccList c1 ((c2, v):xs) | c1 == c2 = (c2, v + 1):xs
                          | otherwise = (c1, 1):(c2, v):xs

-- Function that calculates the number of occurrences of each city
-- This function takes a RoadMap and returns a list of cities and corresponding number of occurrences (degree).
cityCount :: RoadMap -> [(City, Int)]
cityCount [] = []
cityCount rm = foldr addToOccList [] (Data.List.sort (citiesToList rm)) 
{-- 
NOTE: list must be sorted!!!
Example:
    foldr addToList [] ["1", "1", "2"] = 
    addToList "1" (addToList "1" (f "2" [])) =
    addToList "1" (addToList "1" [("2", 1)]) =
    addToList "1" [("1", 1), ("2", 1)] =
    [("1", 2), ("2, 1")] =
--}

------------------------------------------------------------
-- 2. Find the maximum number of occurrences in that list --
------------------------------------------------------------
-- Helper function that finds the maximum value of the occurrence list.
-- This function takes a list of cities and the corresponding degrees 
-- and the current maximum degree from which the overall maximum degree will be computed.
maxOccHelper :: [(City, Int)] -> Int -> Int
maxOccHelper [] currMax = currMax
maxOccHelper ((c1, count):xs) currMax | currMax > count = maxOccHelper xs currMax
                                      | otherwise = maxOccHelper xs count

-- Function that computes the maximum degree from a list of cities and corresponding degree.
maxOcc :: [(City, Int)] -> Int
maxOcc [] = 0
maxOcc [(_, v)] = v -- base case 
maxOcc ((c1, count):xs) = maxOccHelper xs count 

---------------------------------------------------------------------
-- 3. Find the cities that have that maximum number of occurrences --
---------------------------------------------------------------------
-- Function that creates a list with all the cities that have the maximum degree.
-- This function takes a list of cities and the corresponding degrees and returns 
-- a new filtered list of cities with the maximum degree.
maxOccCities :: [(City, Int)] -> [City]
maxOccCities [] = []
maxOccCities c = [c1 | (c1, count) <- c, count == maxVal]
        where maxVal = maxOcc c

-- Function that returns the names of the cities with the highest number of roads
-- connecting to them (given a RoadMap).
rome :: RoadMap -> [City]
rome rm = maxOccCities (cityCount rm)



{- 7 -}
---------------------------------------------------
-- 1. Choose an arbitrary city and perform a DFS --
---------------------------------------------------
-- Function that computes the neighbors of a city.
-- This function takes a RoadMap and a city and returns a list with the cities 
-- that are neighbors of the given city.
neighbors :: RoadMap -> City -> [City]
neighbors rm c = [c1 | (c1, c2, _) <- rm, c2 == c] ++ [c2 | (c1, c2, _) <- rm, c1 == c]

-- Function that returns a boolean indicating whether a city is contained in a list of cities.
-- This function takes a list of cities and a city and checks if that city is in the list.
containsCity :: [City] -> City -> Bool
containsCity cities c | [c1 | c1 <- cities, c1 == c] == [] = False
                      | otherwise = True

-- Helper function for the DFS.
-- This function takes a RoadMap, a list of cities (which initially only has the starting city) and another list of cities
-- (which initially is empty, and will be filled with the visited cities).
dfsAux :: RoadMap -> [City] -> [City] -> [City]
dfsAux _ [] visited = visited -- base case 
dfsAux rm (currCity:rest) visited | containsCity visited currCity = dfsAux rm rest visited -- Ignore the first city (already visited)
                                  | otherwise = dfsAux rm (currentNeighbors ++ rest) (currCity : visited)
                                  where currentNeighbors = neighbors rm currCity

-- Function that performs a Depth-first search.
-- This function takes a RoadMap and a city, which corresponds to the city from where the DFS should start.
dfs :: RoadMap -> City -> [City]
dfs rm start = dfsAux rm [start] []  

{-
Example: 
dfsAux [("0","1",4),("2","3",2)] ["0"] [] =
dfsAux [("0","1",4),("2","3",2)] (["1"] ++ []) ("0" : []) =
dfsAux [("0","1",4),("2","3",2)] ["1"] ["0"] =
dfsAux [("0","1",4),("2","3",2)] (["0"] ++ ["1"]) ("1" : ["0"]) =
dfsAux [("0","1",4),("2","3",2)] ["0", "1"] ["1", "0"] =            -- "0" is already visited
dfsAux [("0","1",4),("2","3",2)] ["1"] ["1", "0"] =                 -- "1" is already visited
dfsAux [("0","1",4),("2","3",2)] [] ["1", "0"] =
["1", "0"]
-}

---------------------------------------------------------------------------------
-- 2. Compare the DFS result size with the total number of cities in the graph --
---------------------------------------------------------------------------------
-- Function that returns a boolean indicating whether all the cities in the graph are connected in the roadmap.
-- This function takes a RoadMap and returns a Bool indicating whether the graph is strongly connected or not.
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected rm = length visitedCities == length allCities
                       where visitedCities = dfs rm (head allCities)
                             allCities = cities rm



{- 8 -}
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined  -- TODO:

{- 9  -}
travelSales :: RoadMap -> Path
travelSales = undefined  -- TODO:

{- 10 (ONLY FOR GROUPS OF 3) -}  
tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]
