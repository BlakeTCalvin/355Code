-- CptS 355 - Spring 2022 -- Homework1 - Haskell
-- Name: Blake Calvin
-- Collaborators: 

module HW1
     where

-- P1 - list_diff 15%                            

list_diff xs ys = listHelper xs ys xs ys
        where
            listHelper [] [] [] [] = []
            listHelper [] ys ox oy = []
            listHelper xs [] ox oy = []
            listHelper (x:xs) (y:ys) ox oy | elem x oy && elem y ox = listHelper xs ys ox oy
                                           | elem x oy && not (elem y ox) = y:(listHelper xs ys ox oy)
                                           | not (elem x oy) && elem y ox = x:(listHelper xs ys ox oy)
                                           | otherwise = x:y:(listHelper xs ys ox oy)

-- P2  replace  15%

replace [] p q n = []
replace xs p q 0 = xs
replace (x:xs) p q n | x == p && n /= 0 = q:(replace xs p q (n-1)) -- letter matches replacement and we havent replaced too many times
                     | otherwise = x:(replace xs p q n)

-- P3  max_date 10%

-- Helper function takes two dates and finds out which one is most recent and return which one is closest
dateHelper (a,b,c) (x,y,z) | (c > z) || ((c == z) && (a > x)) || ((c == z) && (a == x) && (b > y)) = (a,b,c)
                           | otherwise = (x,y,z)


max_date [x] = x    -- base case conditional 
max_date (x:xs) | dateHelper x (max_date xs) == x = x  -- first date was more recent
                | otherwise = max_date xs -- second date more recent

-- P4  num_paths  10%

pathsHelper x y n | x == 1 && y == 1 = n = 1
                  | x == 1 && y > 1 = n + num_paths (x-1) y n
                  | otherwise = n + num_paths x (y-1) n

num_paths x y = pathsHelper x y 0

-- P5  (a) find_courses 10%
-- P5  (b) max_count  15%
-- P6  split_at_duplicate -- 15%