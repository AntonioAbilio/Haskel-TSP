import qualified Data.List
import qualified Data.Array
--import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]


{- 1 -}
{-
    Description:
        Given a roadmap, we will return all cities that are inside the graph, with no duplicates.
   
    Arguments:
        RoadMap - Graph to be used
   
    Returns:
        [City] - list of all cities in the graph or an empty list if the given RoadMap is empty
-}
cities :: RoadMap -> [City]
cities rm = Data.List.nub [c | (c1, c2, _) <- rm, c <- [c1, c2]]



{- 2 -}
{-
    Description:
        Given a roadmap, this function checks if two cities are connected directly.

    Arguments:   
        RoadMap - Graph to be used
        City - First city
        City - Second city
    
    Returns:
        True if the cities are connected directly 
        False otherwise
-}
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent [] _ _ = False
areAdjacent (rm:rms) c1 c2 | (ac1 == c1 && ac2 == c2) || (ac1 == c2 && ac2 == c1) = True
                     | otherwise = areAdjacent rms c1 c2
                     where (ac1, ac2, _) = rm



{- 3 -}
{-
    Description:
        Given a roadmap, this function computes the distance between two cities, if the cities are connected directly. 

    Arguments:
        RoadMap - Graph to be used
        City - First city
        City - Second city

    Returns:
        Just Distance if the cities are connected directly
        Nothing otherwise
-}
distance :: RoadMap -> City -> City -> Maybe Distance
distance [] _ _ = Nothing
distance (rm:rms) c1 c2 | c1 == c2 = Just 0
                        | (ac1 == c1 && ac2 == c2) || (ac1 == c2 && ac2 == c1) = Just d
                        | otherwise = distance rms c1 c2
                        where (ac1, ac2, d) = rm



{- 4 -}
{-
    Description:
        Given a roadmap, this function returns the cities that have a direct edge to a particular city and the respective distance to them.

    Arguments: 
        RoadMap - Graph to be used
        City - City from where we want to look for adjacent cities

    Returns:
        [(City, Distance)] - list of tuples, where the first value is one of the adjacent cities and the second value is the distance to that city or
                             an empty list if the city does not have a direct link to any other city or if roadmap is empty
-}
adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent [] _ = []
adjacent (rm:rms) city | city == fcity = (scity, distance) : adjacent rms city
                       | city == scity = (fcity, distance) : adjacent rms city
                       | otherwise = adjacent rms city
                       where (fcity, scity, distance) = rm



{- 5 -}
{-
    Description:
        Helper Function. This function helps sum two Maybe values.

    Arguments:
        Maybe Int - First value
        Maybe Int - Second value

    Returns:
        Just (a + b) where a and b are of type Int if a came from (Just a) and b came from (Just b).
        Nothing for all other cases (Nothing + Maybe Int = Nothing).
-}
addMaybes :: Maybe Int -> Maybe Int -> Maybe Int
addMaybes (Just a) (Just b) = Just (a+b)
addMaybes  _ _ = Nothing

{-
    Description:
        This function helps get the total distance of a path.
        The function creates a zip with path and its tail and uses the distance function to calculate the distances between the cities,
        adding them to a list. Then it uses foldr with the base case (empty list of distances) being Just 0 and the function addMaybes
        to sum all values.
    
    Arguments:
        RoadMap - Graph to be used
        Path - Path to use to calculate the total distance
    
    Returns:
        Just Distance if the path is valid
        Nothing if the path is invalid (no edge between two cities was found)
-}
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance [] _ = Nothing
pathDistance _ [] = Just 0
pathDistance rm cs = foldr addMaybes (Just 0) [distance rm x y | (x,y) <- zip cs (tail cs)]



{- 6 -}
{-
    Description:
        Given a roadmap, this function adds all the cities to a list (with duplicates).
    
    Arguments:
        RoadMap - Graph to be used

    Returns:
        [City] - list of all occurrences of each city in RoadMap (with duplicates), so that if a city has degree x it will appear x times in that list.
-}
citiesToList :: RoadMap -> [City]
citiesToList [] = []
citiesToList ((c1, c2, _):xs) = c1 : c2 : citiesToList xs

{-
    Description:
        This helper function adds a city to a *sorted* list that counts each occurrence of an element.

    Arguments:
        City - city to be added to the occurrences list (or whose counter should be updated, if already in the list)
        [(City, Int)] - list of tuples where the first value is a city and the second value is the number of its occurrences in the graph so far
    
    Returns:
        [(City, Int)] - list of tuples where the first value is a city and the second value is the number of its occurrences in the graph so far 
-}
addToOccList :: City -> [(City, Int)] -> [(City, Int)]
addToOccList c1 [] = [(c1, 1)]
addToOccList c1 ((c2, v):xs) | c1 == c2 = (c2, v + 1):xs
                          | otherwise = (c1, 1):(c2, v):xs

{-
    Description:
        Given a roadmap, this function calculates the number of occurrences of each city in that roadmap (that number corresponds to the city's degree).

    Arguments:
        Roadmap - Graph to be used

    Returns:
        [(City, Int)] - list of tuples where the first value is a city and the second value is its degree
-}
cityCount :: RoadMap -> [(City, Int)]
cityCount [] = []
cityCount rm = foldr addToOccList [] (Data.List.sort (citiesToList rm))


{-
    Description:
        This helper function finds the maximum value of the occurrence list (using the second value of the tuple).

    Arguments:
        [(City, Int)] - List of tuples, where the first value is a city and the second value is its degree
        Int - Current maximum number (to be used as an accumulator, until the total maximum number is found)

    Returns:
        Int - maximum value of the list
-}
maxOccHelper :: [(City, Int)] -> Int -> Int
maxOccHelper [] currMax = currMax
maxOccHelper ((c1, count):xs) currMax | currMax > count = maxOccHelper xs currMax
                                      | otherwise = maxOccHelper xs count

{-
    Description:
        This function computes the maximum degree from a list of cities and corresponding degree, using the maxOccHelper function.
    
    Arguments:
        [(City, Int)] - List of tuples, where the first value is a city and the second value is its degree
    
    Returns:
        Int - maximum value of the list or 0 if the list is empty
-}
maxOcc :: [(City, Int)] -> Int
maxOcc [] = 0
maxOcc [(_, v)] = v -- base case 
maxOcc ((c1, count):xs) = maxOccHelper xs count

{-
    Description:
        This function creates a list with all the cities that have the maximum degree.
        The algorithm works as follows:
        - 1. Create a list that maps each city to its number of occurrences
        - 2. Find the maximum number of occurrences in that list
        - 3. Find the cities that have that maximum number of occurrences
    
    Arguments:
        [(City, Int)] - list of cities and the corresponding degrees 
    
    Returns:
        [City] - Filtered list of cities with the maximum degree.
-}
maxOccCities :: [(City, Int)] -> [City]
maxOccCities [] = []
maxOccCities c = [c1 | (c1, count) <- c, count == maxVal]
        where maxVal = maxOcc c

{-
    Description:
        Given a RoadMap, this function returns the names of the cities with the highest number of roads connecting to them.
    
    Arguments:
        RoadMap - Graph to be used

    Returns:
        [City] - Cities with the highest number of roads connecting to them
-}
rome :: RoadMap -> [City]
rome rm = maxOccCities (cityCount rm)



{- 7 -}
{-
    Description:
        Given a roadmap, this function computes the neighbors of a city.
     
    Arguments:
        RoadMap - Graph to be used
        City - City whose neighbors we want to return
    
    Returns:
        [City] - List of neighbor cities of the input city
-}
neighbors :: RoadMap -> City -> [City]
neighbors rm c = [c1 | (c1, c2, _) <- rm, c2 == c] ++ [c2 | (c1, c2, _) <- rm, c1 == c]

{-
    Description:
        This function returns a boolean indicating whether a city is contained in list of cities.
        
    Arguments:
        [City] - List of cities where we want to look for the input city
        City - City we want to look for

    Returns:
        True if the city is in the list
        False otherwise
-}
containsCity :: [City] -> City -> Bool
containsCity cities c | [c1 | c1 <- cities, c1 == c] == [] = False
                      | otherwise = True

{-
    Description:
        This function is a helper function for the DFS.
    
    Arguments:
        RoadMap - Graph to be used
        [City] - List of cities (which initially only has the starting city)
        [City] - List of visited cities (which initially is empty)

    Returns:
        [City] - List of visited cities (after performing the DFS)

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
dfsAux :: RoadMap -> [City] -> [City] -> [City]
dfsAux _ [] visited = visited -- base case 
dfsAux rm (currCity:rest) visited | containsCity visited currCity = dfsAux rm rest visited -- Ignore the first city (already visited)
                                  | otherwise = dfsAux rm (currentNeighbors ++ rest) (currCity : visited)
                                  where currentNeighbors = neighbors rm currCity

{-
    Description:
        This function performs a Depth-first search.

    Arguments:
        RoadMap - Graph to be used
        City - Starting city for the DFS
        
    Returns:
        [City] - List of cities that the DFS could reach from the starting city
-}
dfs :: RoadMap -> City -> [City]
dfs rm start = dfsAux rm [start] []

{-
    Description:
        Given a roadmap, this function returns a boolean indicating whether all the cities in that roadmap are connected.
        The algorithm works as follows:
        - 1. Choose an arbitrary city and perform a DFS
        - 2. Compare the DFS result size with the total number of cities in the graph 
        
    Arguments:
        RoadMap - Graph to be used
    
    Returns:
        True if the roadmap is strongly connected
        False otherwise
-}
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected [] = True
isStronglyConnected rm = length visitedCities == length allCities
                       where visitedCities = dfs rm (head allCities)
                             allCities = cities rm



{- 8 -}
-- QUEUE --
data Queue a = Que [a] -- Queue (we use this for the BFS)

-- Returns an updated queue, with a given value pushed to it.
push :: a -> Queue a -> Queue a
push x (Que xs) = Que (xs ++ [x])

-- Returns the front value of the queue.
front :: Queue a -> a
front (Que (x:xs)) = x

-- Returns an updated queue, without the front value.
pop :: Queue a -> Queue a
pop (Que (x:xs)) = Que xs

-- Checks if a queue is empty (Returns True if it is empty and false otherwise).
isQueueEmpty :: Queue a -> Bool
isQueueEmpty (Que xs) = null xs

-- Returns an empty queue.
emptyQueue :: Queue a
emptyQueue = Que []
-----------

{-
    Description:
        This function checks if a path ends with a given city.
    
    Arguments:
        Path - Path to check.
        City - City we are looking for.
    
    Returns:
        True if the Path ends in the city we are looking for.
        False if the Path ends in a different city.
-}
checkIfCorrectPath :: Path -> City -> Bool
checkIfCorrectPath [] _ = False
checkIfCorrectPath path city | last path == city = True
                             | otherwise = False

{-
    Description:
        This function takes a path and a list of cities and creates new paths, one for each city.
        Each new path is the same as the original one, but with a city of the list appended.
    
    Arguments:
        Path - Path to which we want to append the cities
        [City] - List of cities to append to the path

    Returns: 
        [Path] - List of new paths that were created using the original path and the list of cities.
-}
addNeighboursToPath :: Path -> [City] -> [Path]
addNeighboursToPath [] _ = []
addNeighboursToPath path [] = []
addNeighboursToPath path (n1:ns) = (path ++ [n1]) : addNeighboursToPath path ns

{-
    Description:
        Given a list of paths, this function uses the checkIfCorrectPath function to determine if city "c" is the last
        city. This is because we start building new paths from city "c".
        
    Arguments:
        City - Sink City (to know when to stop)
        [Path] - List of paths found so far
        City - City to append neighbors to
        [City] - List with neighbors of city

    Returns: 
        [Path] - updated paths list

    Examples:
        updatePaths 6 [[1,2], [1,3]] 3 [4,5] = [[1,2], [1,3,4], [1,3,5]]
-}
updatePaths :: City -> [Path] -> City -> [City] -> [Path]
updatePaths _ [] _ _ = []
updatePaths _ paths _ [] = paths
updatePaths sink (path1:paths) c (n1:ns) | c == sink = (path1:paths)
                                         | checkIfCorrectPath path1 c = addNeighboursToPath path1 (n1:ns) ++ updatePaths sink paths c (n1:ns)
                                         | otherwise = path1 : updatePaths sink paths c (n1:ns)


{-
    Description:
        This function performs a Breadth-first search to find all the paths from a start city to an end city.

    Arguments:
        RoadMap - Graph to be used
        City - Sink city
        [Path] - List of paths found so far
        Queue City - Queue for cities to be visited in the next iterations
        [City] - List of visited cities

    Returns:
        [Path] - List of paths found
-}
bfs :: RoadMap -> City -> [Path] -> Queue City -> [City] -> [Path]
bfs rm sink paths q vis | isQueueEmpty q = paths -- If queue is empty we don't have any more nodes to visit, return visited nodes.
                        | front q `elem` vis = bfs rm sink paths (pop q) vis -- If the element is already visited then ignore it and run bfs again.
                        | otherwise = bfs rm sink (updatePaths sink paths nodeToVisit nei) (foldr push (pop q) nei) (nodeToVisit : vis)
                        where nodeToVisit = front q
                              nei = neighbors rm nodeToVisit

{-
    Description:
        This function uses the bfs function to perform a Breadth-first search from the start city to the sink city, in order to find all the paths between these cities.

    Arguments:
        RoadMap - Graph to be used
        City - Source, this is where we want the bfs to start.
        City - Sink, this is where we want the bfs to end.
        
    Returns:
        [Path] - List of paths found from start to sink
-}
bfsStart :: RoadMap -> City -> City -> [Path]
bfsStart rm source sink =  filter (\x -> last x == sink) (bfs rm sink [[source]] (push source emptyQueue) [])

{-
    Description:
        This function takes a list of paths and calculates the corresponding total distances.

    Arguments:
        RoadMap - Graph to be used
        [Path] - Paths whose distances we want to compute

    Returns:
        [Maybe Distance] - Distances of the input paths  
-}
calculatePathDistances :: RoadMap -> [Path] -> [Maybe Distance]
calculatePathDistances rm [] = []
calculatePathDistances rm (path1:paths) = [pathDistance rm path1] ++ calculatePathDistances rm paths

{-
    Description:
        This function takes two Maybe Distance values and compares them.

    Arguments:
        Maybe Distance - First distance
        Maybe Distance - Second distance

    Returns:
        True if the first distance is less than the second or the second distance is Nothing
        False otherwise (and when the first distance is Nothing)
-}
lessMaybe :: Maybe Distance ->  Maybe Distance -> Bool
lessMaybe (Just a) (Just b) = a < b
lessMaybe (Just a) Nothing = True
lessMaybe Nothing (Just a) = False
lessMaybe _ _ = False

{-
    Description:
        This function uses the bfsStart function to get list of all the paths between a source and a sink city.
        Then it uses these paths to calculate the distance of the shortest paths of that list. 
    
    Arguments:
        RoadMap - Graph to be used
        City - Source city
        City - Sink city

    Returns:
        Maybe Distance - Smallest distance between the source city and the sink city 
-}
getSmallestDist :: RoadMap -> City -> City -> Maybe Distance
getSmallestDist rm source sink = foldr (\x y -> if lessMaybe x y then x else y) Nothing shortestPathsDists
                                where shortestPaths      = bfsStart rm source sink
                                      shortestPathsDists = calculatePathDistances rm shortestPaths

{-
    Description:
        This function compares two Maybe Distance values.

    Arguments:
        Maybe Distance - First value
        Maybe Distance - Second value
    
    Returns:
        True if both values are Just values and the same
        False otherwise
-}
areMaybeEqual :: Maybe Distance ->  Maybe Distance -> Bool
areMaybeEqual (Just a) (Just b) = a == b
areMaybeEqual _ _ = False

{-
    Description:
        This function calculates the shortest paths between two cities.
        It uses a BFS to compute all the paths between the cities and then it extracts the shortest ones and returns them as
        a list of paths.

    Arguments:
        RoadMap - Graph to be used
        City - Start city
        City - Sink city

    Returns:
        [Path] - List of shortest paths
-}
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath rm source sink = [p | p <- bfsStart rm source sink, areMaybeEqual (getSmallestDist rm source sink) (pathDistance rm p)]



{- 9 -}
-- SET --
type Set = Int -- Set is represented as an Int (which can be seen as a bitmask)

-- Returns an empty set
emptySet :: Set
emptySet = 0

-- Check if a set is empty (True if it is empty, False otherwise)
setEmpty :: Set -> Bool
setEmpty n = n==0

-- Maximum size that a set can have
maxSet :: Int
maxSet = truncate (logBase 2 (fromIntegral (maxBound::Int))) - 1

-- Given a set, this function returns the corresponding list
set2List :: Set -> [Int]
set2List s = s2l s 0
        where s2l 0 _ = []
              s2l n i | odd n = i : s2l (n `div` 2) (i + 1)
                      | otherwise = s2l (n `div` 2) (i + 1)

-- This function removes the i-th bit from a given set
delSet :: Int -> Set -> Set
delSet i s = d' * e + m
           where (d, m) = divMod s e
                 e = 2^i
                 d' = if odd d then d - 1 else d

-- Function that creates a bitmask with n bits set to 1 but with the first bit at 0.
fullSet :: Int -> Set
fullSet n | (n >= 0) && (n <= maxSet) = 2 ^ (n + 1) - 2
          | otherwise = error ("fullset : illegal set = " ++ show n)
---------


-- TSP types --
type TspCoord = (Int , Set)
type TspEntry = (Maybe Distance, [Int])
---------------


-- TABLE --
newtype Table a b = Tbl (Data.Array.Array TspCoord TspEntry) -- Table is represented as two-dimensional array
                  deriving Show

-- Given a list of entries, create a table
newTable :: [(TspCoord , TspEntry)] -> Table TspEntry TspCoord
newTable l = Tbl (Data.Array.array ((lolo, lohi), (hilo, hihi)) l)
           where indices = map fst l
                 (lolo, lohi) = minimum indices
                 (hilo, hihi) = maximum indices

-- Given a table and a coordinate, return the corresponding entry
findTable :: Table TspEntry TspCoord -> TspCoord -> TspEntry
findTable (Tbl a) (i,j) = a Data.Array.! (i,j)
-----------


{-
    Helper function.

    Arguments:
        RoadMap - Graph to be used.
        Int - An index for the cities list.
        Int - Another index for the cities list.
        [City] - The cities list.

    Explanation:
        Takes 2 indexes, a RoadMap and a city list. Them it uses the two indexes to access
        the city list and finally it uses the actual cities to calculate the distances.

    Returns:
        Maybe Distance - Distance between the two cities.
-}
distCalcAux :: RoadMap -> Int -> Int -> [City] -> Maybe Distance
distCalcAux rm idxCity1 idxCity2 cities = distance rm city1 city2
            where city1 = cities !! (idxCity1 - 1)
                  city2 = cities !! (idxCity2 - 1)

{-
    Helper Function.

    Arguments:
        (Maybe Distance, [Int]) - One tuple that is going to be used in the comparison.
        (Maybe Distance, [Int]) - Another tuple that is going to be used in the comparison.

    Explanation:    
        Used to compare maybes.
        Any just is bigger than nothing (Nothing is used as +infinity).
        Nothings are not bigger or equal to Nothings so in the case where we are comparing two of them, either one can be returned.

    Returns:
        (Maybe Distance, [Int]) - The tuple that has the minimum distance of the two received.
-}
minMaybe :: (Maybe Distance, [Int]) -> (Maybe Distance, [Int]) -> (Maybe Distance, [Int])
minMaybe (Nothing, _) (Just a, xs) = (Just a, xs)
minMaybe (Just a, xs) (Nothing, _) = (Just a, xs)
minMaybe (Just a, xs) (Just b, ys) = if a < b then (Just a, xs) else (Just b, ys)
minMaybe (Nothing, xs) (Nothing, _) = (Nothing, xs)

{-
    Helper Function.

    Arguments:
        [(Maybe Distance, [Int])] - List of TableEntries.
    
    Explanation:
        Because we use Maybe Distance we needed to implement a new way to find the minimum distance between two cities
        due to Nothing being used as +infinity (no possible path).
        This function uses foldr and minMaybe to determine the smallest possible distance in a list of possible paths.
        Returns the smallest path.
        
    Returns:
        (Maybe Distance, [Int]) - The TableEntry with the smallest distance.
-}
maybeMinimum :: [(Maybe Distance, [Int])] -> (Maybe Distance, [Int])
maybeMinimum (x:xs) = foldr minMaybe x xs

{-
    Helper Function.

    Arguments:
        RoadMap - Graph to be used.
        Int - Starting node.
        Table TspEntry TspCoord - Our memoization table that stores precomputed values for the tsp.
        TspCoord - A tuple (i,j) where i represent the current city and j the cities we have not yet visited.

    Explanation:
        compTsp is the function that actually computes the tsp.
        
        The first case is for when we don't have any more cities to visit. In this case we want to
        calculate the distance between the starting node (n) and the last city we visited, i.
        
        The second case is for when we still have cities to visit. In this case we make every possible choice by
        using z <- set2List j. This will allow us to compute every distance along with every single path starting 
        from the current city, i, to every other city (addFst is the function that helps create this list of possible paths).

        Also, we can assume that the path that we use to connect z and i is the shortest path because we use 
        findTable dynTable (z, delSet z j) which gives us the optimal path that ended in city z.

         currentNode     nextToVisit
            "2" ------------ "3"        -> findTable returns the entry where the last city visited was z (3).

        Now that we have these paths we really only want the one that has the shortest distance. So for this we use the
        maybeMinimum function which will actually return the smallest path.

        Finally we return this value and add it to the table. The returned value will have the minimum distance found for
        current city and the set won't contain the element that was visited ("added to the path by the addFst function").

    Returns:
        TspEntry - Returns the computed distance of a path.      
-}
compTsp :: RoadMap -> Int -> Table TspEntry TspCoord -> TspCoord -> TspEntry
compTsp rm n dynTable (i,j) | setEmpty j = (distCalcAux rm i n citiesList, [i, n])
                            | otherwise = maybeMinimum [addFst (findTable dynTable (z, delSet z j)) (distCalcAux rm i z citiesList) | z <- set2List j]
                            where addFst (c, p) w = (addMaybes w c, i:p)
                                  citiesList = cities rm

{-
    Arguments:
        Int - Length

    Explanation:
        This function is called to make a tuple that is going to be used to generate every table coordinate using
        Data.Array.range.
-}
bndsTsp :: Int -> (TspCoord, TspCoord)
bndsTsp n = ((1, emptySet), (n, fullSet n))

{-
    Arguments:
        RoadMap - Graph to be used
        Int - Starting node
        (TspCoord, TspCoord) - Table bounds

    Explanation:
        The next function that is relevant is compTsp rm n t (x, y) that is inside the lambda function. This allows
        for the calculation of the tsp path where the result will be placed inside the table at the (x,y) coordinates. 
        This function call uses a reference of the current table that is going to be created so that it can be 
        used as a memoization table (dynamic programming table).
    
    Returns:
        Table TspEntry TspCoord - The constructed memoization table
-}
dynamicTSP :: RoadMap -> Int -> (TspCoord, TspCoord) -> Table TspEntry TspCoord
dynamicTSP rm n bnds = t
                 where t = newTable (map (\(x,y) -> ((x,y), compTsp rm n t (x,y))) (Data.Array.range bnds))

{-
    Arguments:
        RoadMap - Graph to be used
    
    Explanation:
        Given a roadmap, this function uses it to determine the number of cities using length (cities rm).
        Then it calls dynamicTSP which builds the memoization table.
        Afterwards it uses the table that was created and returns the last element of that table which
        corresponds to the tuple (distance of tsp path, tsp path of the graph).
-}
tsp :: RoadMap -> TspEntry
tsp rm = findTable t (n, fullSet (n-1))
       where n = length (cities rm)
             t = dynamicTSP rm n (bndsTsp n)

{-
    Arguments:
        [Int] - List of nodes
        [City] - List of cities

    Explanation:
        Given a list of ints and a the cities list the function will construct the path
        by using the integers as an index for the cities list.
        
        Base case: If list of indexes is [] then we have alredy reached then end. No more cities need to be added -> Return [].
        Recursive step: Takes an integer from [Int] uses it to determine the city and appends it to the Path.
    
    Returns:
        Constructed path that results from mapping the node indices to the actual cities in the graph
-}
buildPath :: [Int] -> [City] -> Path
buildPath [] _ = []
buildPath (x:xs) cities = (cities !! (x - 1)) : buildPath xs cities

{-
    Arguments:
        RoadMap - Graph to be used

    Explanation:
        Receives a roadmap and checks if there exists a tsp path by calling the tsp function.
        If tsp returns Nothing as the minimum distance then there's no tsp path otherwise it calls
        the buildPath helper function with the found path (indexes for city list) and the city list.

    Returns:
        [] if no tsp path found
        Path if a tsp path is found
-}
travelSales :: RoadMap -> Path
travelSales [] = []
travelSales rm = if minDist /= Nothing then buildPath foundPath listCities else []
               where listCities = cities rm
                     foundEntry = tsp rm
                     (minDist, foundPath) = foundEntry



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
