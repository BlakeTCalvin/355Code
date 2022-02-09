{- Example of using the HUnit unit test framework.  See  
http://hackage.haskell.org/package/HUnit for additional documentation.
To run the tests type "run" at the Haskell prompt.  -} 

module HW1Tests
    where
import Test.HUnit
import Data.Char
import Data.List (sort)
import HW1

-- P1. list_diff tests
 
p1_test1 = TestCase (assertEqual "list_diff-test1" 
                                 (sort [46,31,21,4,1,6,3])
                                 (sort $ list_diff [5,46,5,9,0,1,3] [5,31,21,4,9,6,0]) )
p1_test2 = TestCase (assertEqual "list_diff-test2" 
                                 (sort "BuvcCuS355wyumr!")
                                 (sort $ list_diff "I love CptS 355 and Haskell!" "But I could always use more practice in Haskell.") ) 
p1_test3 = TestCase (assertEqual "list_diff-test3" 
                                 []  
                                 (list_diff ["Reshala", "Glukhar", "Sanitar", "Killa", "Tagilla", "Shturman"] ["Tagilla", "Shturman", "Killa", "Reshala", "Glukhar", "Sanitar"]) ) 

-- P2. replace tests

p2_test1 = TestCase (assertEqual "replace-test1" 
                                  "I really hope that i can get an A in this class"
                                  (replace "I rea00y hope that i can get an A in this class" '0' 'l' 2) ) 
p2_test2 = TestCase (assertEqual "replace-test2" 
                                  [("Shturman", "Woods", 2),("Tagilla", "Factory", 0),("Glukhar", "Reserve", 6),("Reshala", "Customs", 3),("Sanitar", "Shoreline", 2),("Shturman", "Woods", 1)]
                                  (replace [("Shturman", "Woods", 1),("Tagilla", "Factory", 0),("Glukhar", "Reserve", 6),("Reshala", "Customs", 3),("Sanitar", "Shoreline", 2),("Shturman", "Woods", 1)] ("Shturman", "Woods", 1) (("Shturman", "Woods", 2)) 1) ) 
p2_test3 = TestCase (assertEqual "replace-test3" 
                                  [1,2,3,4,5,6,7,8,9,10,1,2,3,4,55,6,7,8,9,10]  
                                  (replace [1,2,3,4,55,6,7,8,9,10,1,2,3,4,55,6,7,8,9,10] 55 5 1) ) 

-- P3. max_date tests 

p3_test1 = TestCase (assertEqual "max_date-test1" 
                                  (7,6,2004)
                                  (max_date [(7, 6, 2004),(1, 1, 2003)]) ) 
p3_test2 = TestCase (assertEqual "max_date-test2" 
                                  (4,26,2021)   
                                  (max_date [(4, 6, 1456),(3, 11, 2003), (1, 6, 2000),(2, 32, 1998), (1, 7, 2021),(4, 26, 2021)]) ) 
p3_test3 = TestCase (assertEqual "max_date-test3" 
                                  (7,1,1997)  
                                  (max_date [(7, 1, 1997)]) ) 

-- P4. num_paths tests                                  

p4_test1 = TestCase (assertEqual "num_paths-test1" 
                                  1  
                                  (num_paths 1 1) ) 
p4_test2 = TestCase (assertEqual "num_paths-test2" 
                                  2
                                  (num_paths 2 2) ) 
p4_test3 = TestCase (assertEqual "num_paths-test3" 
                                  15
                                  (num_paths 5 3) ) 
p4_test4 = TestCase (assertEqual "num_paths-test3" 
                                  1 
                                  (num_paths 30000 1) ) 

-- P5. (a) and (b)

-- character list
onGameRelease =
     [("Hunter" , ["Artemis"]),
     ("Mage" , ["Agni", "Anubis", "Hades", "He Bo", "Hel", "Kukulkan", "Ra", "Zeus"]),
     ("Guardian" , ["Sobek", "Ymir"]),
     ("Warrior" , ["Odin", "Vamana"]),
     ("Assasin" , ["Arachne", "Bastet", "Hun Batz", "Kali"]),
     ("PhysicalDamage", ["Artemis", "Odin", "Vamana", "Arachne", "Bastet", "Hun Batz", "Kali"]),
     ("MagicalDamage", ["Agni", "Anubis", "Hades", "He Bo", "Hel", "Kukulkan", "Ra", "Zeus", "Sobek", "Ymir"]),
     ("Damage", ["Artemis", "Agni", "Anubis", "Hades", "He Bo", "Hel", "Kukulkan", "Ra", "Zeus", "Arachne", "Bastet", "Hun Batz", "Kali"]),
     ("Tank", ["Sobek", "Ymir", "Odin", "Vamana"])
     ]

-- find_courses tests 

p5a_test1 = TestCase (assertEqual "(find_courses-test1)" 
                                  ["Mage","MagicalDamage","Damage"] 
                                  (find_courses onGameRelease "He Bo") ) 
p5a_test2 = TestCase (assertEqual "(find_courses-test2)" 
                                  ["Guardian","MagicalDamage","Tank"]
                                  (find_courses onGameRelease "Sobek") )                             
p5a_test3 = TestCase (assertEqual "(find_courses-test3)" 
                                  []
                                  (find_courses onGameRelease "") )

-- max_count tests  (one test is sufficient)

p5b_test1 = TestCase (assertEqual "(max_count-test1)" 
                                   ("Damage",13)
                                   (max_count onGameRelease) ) 

-- P6. split_at_duplicate tests

p6_test1 = TestCase (assertEqual "(split_at_duplicate-test1)"  
                                 [[1,2,3,4,5],[5,4,3,2,1]] 
                                 (split_at_duplicate [1,2,3,4,5,5,4,3,2,1] ) )
p6_test2 = TestCase (assertEqual "(split_at_duplicate-test2)" 
                                 [[5],[5],[5],[5],[5],[5]]  
                                 (split_at_duplicate [5,5,5,5,5,5]) ) 
p6_test3 = TestCase (assertEqual "(split_at_duplicate-test3)"  
                                 [[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100]] 
                                 (split_at_duplicate [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100] ) )
p6_test4 = TestCase (assertEqual "(split_at_duplicate-test4)"  
                                 []  
                                 (split_at_duplicate ([]::[Int]) ) ) 

-- add the test cases you created to the below list. 
tests = TestList [ TestLabel "Problem 1- test1 " p1_test1, -- Question 1 Tests
                   TestLabel "Problem 1- test2 " p1_test2,  
                   TestLabel "Problem 1- test3 " p1_test3,

                   TestLabel "Problem 2- test1 " p2_test1, -- Question 2 Tests
                   TestLabel "Problem 2- test2 " p2_test2,  
                   TestLabel "Problem 2- test3 " p2_test3,

                   TestLabel "Problem 3- test1 " p3_test1, -- Question3 3 Tests
                   TestLabel "Problem 3- test2 " p3_test2, 
                   TestLabel "Problem 3- test3 " p3_test3,

                   TestLabel "Problem 4- test1 " p4_test1, -- Question 4 Tests
                   TestLabel "Problem 4- test2 " p4_test2,
                   TestLabel "Problem 4- test3 " p4_test3, 
                   TestLabel "Problem 4- test4 " p4_test4,

                   TestLabel "Problem 5a- test1 " p5a_test1, -- Question 5 Tests
                   TestLabel "Problem 5a- test2 " p5a_test2,
                   TestLabel "Problem 5a- test3 " p5a_test3, 
                   TestLabel "Problem 5b- test1 " p5b_test1,

                   TestLabel "Problem 6- test1 " p6_test1, -- Question 6 Tests
                   TestLabel "Problem 6- test2 " p6_test2,
                   TestLabel "Problem 6- test3 " p6_test3, 
                   TestLabel "Problem 6- test4 " p6_test4
                 ] 
                  
-- shortcut to run the tests
run = runTestTT tests