-- This is part of Ninety-Nine Haskell Problems, based on Ninety-Nine Prolog Problems and Ninety-Nine Lisp Problems.

-- Problem 1
-- (*) Find the last element of a list.

-- (Note that the Lisp transcription of this problem is incorrect.)

-- Example in Haskell:

-- λ> myLast [1,2,3,4]
-- 4
-- λ> myLast ['x','y','z']
-- 'z'
-- Solutions

myLast :: [a] -> a
myLast [] = error "no end for empty list"
myLast [x] = x 
myLast (_:xs) = myLast xs

-- Problem 2
-- (*) Find the last but one element of a list.

-- (Note that the Lisp transcription of this problem is incorrect.)

-- Example in Haskell:

-- λ> myButLast [1,2,3,4]
-- 3
-- λ> myButLast ['a'..'z']
-- 'y'
-- Solutions
myButLast :: [a] -> a 
myButLast = last . init -- last returns the last element of the list must be finite non empty.  init returns all the elements of the list except the last one. 
                        -- therefore last . init will first look at all the elements and return them with the last element removed. and last will return the last element of the new list -1 size
-- Problem 3
-- (*) Find the K'th element of a list. The first element in the list is number 1.

-- Example:

-- * (element-at '(a b c d e) 3)
-- c
-- Example in Haskell:

-- λ> elementAt [1,2,3] 2
-- 2
-- λ> elementAt "haskell" 5
-- 'e'
-- Solutions
elementAt :: [a]->Int -> a -- this is because with the list we will use a number to look for and that be the int and then return an a value
elementAt list i = list !! (i-1) -- Explanation below.
-- !!
-- Returns the element of a list located at the specified index. Note that an 'index' starts counting from zero.
-- Indexing lists
-- These functions treat a list xs as a indexed collection, with indices ranging from 0 to length xs - 1.
-- (!!) :: [a] -> Int -> a
-- List index (subscript) operator, starting from 0. It is an instance of the more general genericIndex, which takes an index of any integral type.


-- Problem 4
-- (*) Find the number of elements of a list.

-- Example in Haskell:

-- λ> myLength [123, 456, 789]
-- 3
-- λ> myLength "Hello, world!"
-- 13
-- Solutions
myLength :: [a]-> Int -- this is important because we are giving the integer list a return integer just like the prior examples
myLength [] = 0 -- if the list is empty then nothing is in it therefore is 0
myLength (_:xs) = 1 + myLength xs    -- to understand this piece of code

-- Problem 5
-- (*) Reverse a list.

-- Example in Haskell:

-- λ> myReverse "A man, a plan, a canal, panama!"
-- "!amanap ,lanac a ,nalp a ,nam A"
-- λ> myReverse [1,2,3,4]
-- [4,3,2,1]
-- Solutions
myReverse :: [a]-> [a]
myReverse [] = [] -- empty is empty
myReverse (x:xs)= myReverse xs ++ [x] -- the naive solution is that we will look at the list and pull from the list into X from the back to the front

-- Problem 6
-- (*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).

-- Example in Haskell:

-- λ> isPalindrome [1,2,3]
-- False
-- λ> isPalindrome "madamimadam"
-- True
-- λ> isPalindrome [1,2,4,8,16,8,4,2,1]
-- True
-- Solutions
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [_] =True
isPalindrome [] = True
isPalindrome x = x == reverse x

-- Problem 7
-- (**) Flatten a nested list structure.

-- Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).

-- Example:

-- * (my-flatten '(a (b (c d) e)))
-- (A B C D E)
-- Example in Haskell:

-- We have to define a new data type, because lists in Haskell are homogeneous.

data NestedList a = Elem a | List [NestedList a]
-- λ> flatten (Elem 5)
-- [5]
-- λ> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
-- [1,2,3,4,5]
-- λ> flatten (List [])
-- []
-- Solutions
flatten :: NestedList a -> [a]
flatten (Elem a)  = [a]
flatten (List (x:xs))= flatten x ++ flatten (List xs)
flatten (List []) = []

-- Problem 8
-- (**) Eliminate consecutive duplicates of list elements.

-- If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.

-- Example:

-- * (compress '(a a a a b c c a a d e e e e))
-- (A B C A D E)
-- Example in Haskell:

-- λ> compress "aaaabccaadeeee"
-- "abcade"
-- Solutions
compress:: Eq a => [a] -> [a]
compress (x:ys@(y:_))  
        | x==y = compress ys
        |otherwise = x: compress ys
compress ys=ys 
-- Problem 9
-- (**) Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.

-- Example:

-- * (pack '(a a a a b c c a a d e e e e))
-- ((A A A A) (B) (C C) (A A) (D) (E E E E))
-- Example in Haskell:

-- λ> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 
--              'a', 'd', 'e', 'e', 'e', 'e']
-- ["aaaa","b","cc","aa","d","eeee"]
-- Solutions

-- Problem 10
-- (*) Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

-- Example:

-- * (encode '(a a a a b c c a a d e e e e))
-- ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
-- Example in Haskell:

-- λ> encode "aaaabccaadeeee"
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
-- Solutions

