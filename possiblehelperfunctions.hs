-- Possible helper functions ?

{--
--   Helper Function.
--   The function takes in a city (the one we are trying to find out if it is adjacent) 
--   and a list of adjacent cities (previously calculated for another city).
--   If the city is inside the list then we know that there exists a direct edge between the cities
--   and we can return the distance.
--   Returns 0 if the city is not adjacent
--           distance between them if it is adjacent.
--
--   Other definition...
--
--   getAdjacentDistance :: City -> [(City, Distance)] -> Distance
--   getAdjacentDistance _ [] = 0
--   getAdjacentDistance city (ac:acs) | city == adjacentCity = distance
--                                   | otherwise = getAdjacentDistance city acs
--                                   where (adjacentCity, distance) = ac
--}                                        
getAdjacentDistance :: City -> [(City, Distance)] -> Distance
getAdjacentDistance city adjCities = if null adjCity 
                                        then 0
                                     else 
                                        (\(_,y) -> y) (head adjCity)
                                     where adjCity = filter (\(x,_) -> city == x) adjCities