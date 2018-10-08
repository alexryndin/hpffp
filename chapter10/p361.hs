-- foldl (flip (*)) 1 [1..3]
-- foldl (flip (*)) ((*) 1 1) [2..3]
-- foldl (flip (*)) ((*) 2 ((*) 1 1)) [3]
-- foldl (flip (*)) ((*) 3 ((*) 2 ((*) 1 1)) []
-- ((*) 3 ((*) 2 ((*) 1 1))
-- 3 * (2 * (1 * 1))

-- foldr const 'a' [1..5]
-- 1 `const` (2 `const` (3 `const` 'a'))foldr
