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
{--
--   Helper function. Given a city and a list of cities, it checks if the given city is inside the list.
--   Returns true if the city is inside the list.
--           false if the city is not inside the list.
--}
checkIfInsideList :: City -> [City] -> Bool
checkIfInsideList _ [] = False
checkIfInsideList city (cm:cms) | city == cm    = True
                                | otherwise     = checkIfInsideList city cms

{--
--   Helper Function.
--   Given an array of cities, it extracts the first city and uses the function checkIfInsideList to see if the city is still
--   inside the tail of the list. If it is then it it will get added later so we drop the element and call the function with the list's tail
--   otherwise we add the element and move on to the next city in the list.
--   Returns an array with the duplicates removed
--}
getUniqueCities :: [City] -> [City]
getUniqueCities [] = []
getUniqueCities (c:cs) = if checkIfInsideList c cs 
                            then getUniqueCities cs 
                         else 
                            c : getUniqueCities cs

{--
--   Helper function.
--   Given a roadmap, we will extract all cities from edges.
--   Returns an array containing all cities (with duplicates).
--}
getAllCities :: RoadMap -> [City]
getAllCities [] = []
getAllCities (rm:rms) = fCity : sCity : getAllCities rms
                      where (fCity, sCity, _) = rm

{--
--   Given a roadmap, we will return all cities that are inside the graph with no duplicates.
--   Returns an empty list if the given RoadMap is empty.
--           a list of all cities in the graph.
--}
cities :: RoadMap -> [City]
cities [] = []
cities rm = cities
          where allCities = getAllCities rm
                cities = getUniqueCities allCities

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
rome :: RoadMap -> [City]
rome = undefined  -- TODO:

{- 7 -}
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected = undefined  -- TODO:

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
