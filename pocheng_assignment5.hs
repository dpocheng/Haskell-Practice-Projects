-- Name: Pok On Cheng
-- ID:   74157306

-- NOTES
-- Don't import any libraries.
-- You CAN'T use the built in sort or maximum functions.
-- Type "test_all" to run all tests.
-- We will use extra tests when grading.
-- Problem 5: You must use a higher-order function. See P5 for more info.

-- Problem 1
insert :: (Ord a) => [a] -> a -> [a]
-- insert xs x = [] -- this is wrong, delete this!
insert [] x = [x]
insert (h:t) x = if x < h
                   then x:h:t
                   else h:insert t x

-- Problem 2
-- Remember to use the insertion sort algorithm here.
-- (Wikipedia has a good explanation).
insertSort :: (Ord a) => [a] -> [a]
-- insertSort xs = [] -- this is wrong, delete this!
insertSort [x] = [x]
insertSort (h:t) = insert (insertSort t) h

-- Problem 3
mergeTwoLists :: (Ord a) => [a] -> [a] -> [a]
mergeTwoLists [] x = x
mergeTwoLists x [] = x
mergeTwoLists (x:xs) (y:ys) = if x < y
                                then x:(mergeTwoLists (y:ys) xs) 
                                else y:(mergeTwoLists ys (x:xs))

mergeMultiLists :: (Ord a) => [[a]] -> [[a]]
mergeMultiLists [] = []
mergeMultiLists (x:[]) = [x]
mergeMultiLists (x:y:tail) = mergeMultiLists ((mergeTwoLists x y):(mergeMultiLists tail))

merge :: (Ord a) => [[a]] -> [a]
-- merge xs = [] -- this is wrong, delete this!
merge [] = []
merge x = head (mergeMultiLists x)

-- Problem 4
center :: [a] -> Int -> a -> [a]
-- center xs n fill = [] -- this is wrong, delete this!
center arg1 arg2 arg3 = lFilled ++ arg1 ++ rFilled where
    {num = arg2 - (length arg1);
     lNum = (div num 2) + (mod num 2);
     rNum = div num 2;
     lFilled = replicate lNum arg3;
     rFilled = replicate rNum arg3}

-- Problem 5
-- Remember to use a higher order function here.
--
-- Example of using higher-order function (map in this case):
-- doubleList xs = map (* 2) xs
-- Other higher-order functions to look at:
-- filter
-- foldl, foldr, foldl1, foldr1 (all similar)
largestElment :: (Ord a) => a -> a -> a
largestElment x y = if x > y then x else y

largest :: (Ord a) => [a] -> a
-- largest xs = head xs -- this is wrong, delete this!
largest (head:tail) = foldl largestElment head tail

---------- TESTING -----------

failure output expected     = " --> TEST FAILED. Your output: " 
                              ++ (show output) ++ " expected: " 
                              ++ (show expected)
failures output expecteds    = " --> TEST FAILED. Your output: " 
                              ++ (show output) ++ " expected: " 
                              ++ (show (head expecteds)) ++ " OR " 
                              ++ (show (last expecteds))

test output expected        = if (output == expected)
                              then putStrLn " OK"
                              else putStrLn (failure output expected)

-- For tests where there are 2 answers allowed.
testMultiple output expecteds = if (any (== output) expecteds)
                              then putStrLn " OK"
                              else putStrLn (failures output expecteds)

problem index name  = do
                      putStrLn ""
                      putStrLn ("Problem " ++ (show index) ++ ": " ++ name)
testname name       = putStr ("-- " ++ name)

-- Add your own tests here.
test_insert = do 
                problem 1 "Insert"
                testname "Testing floats"
                test (insert [1.0, 2.2, 3.3, 6.5, 42.0] 3.1) [1.0, 2.2, 3.1, 3.3, 6.5, 42.0]
                testname "Testing with duplicates"
                test (insert [0, 5, 10, 10] 5) [0,5,5,10,10]

test_insertSort = do 
                problem 2 "InsertSort"
                testname "Simple test"
                test (insertSort [2,4,0,1]) [0,1,2,4]

test_merge = do 
                problem 3 "Merge"
                testname "Simple test"
                test (merge [[1,2,3],[4,5,6]]) [1,2,3,4,5,6]

test_center = do 
                problem 4 "Center"
                testname "Testing strings"
                testMultiple (center "hello" 8 '+') ["+hello++", "++hello+"]
                testname "Testing integers"
                test (center [0, 5, 10, 10] 8 2) [2,2,0,5,10,10,2,2]

test_largest = do 
                problem 5 "Largest"
                testname "Testing sorted list"
                test (largest [1,2,3,4,5]) 5
        
-- Comment out any functions that haven't been implemented yet.    
test_all = do
           test_insert
           test_insertSort
           test_merge
           test_center
           test_largest