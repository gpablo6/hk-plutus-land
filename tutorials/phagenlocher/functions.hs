-- In HK there is no explicit return statement. The the expression is what the function returns f(x) = <expr> 
inRange min max x =
    x >= min && x <= max

-- 'in_range' is a composition of other functions, because 'min' and 'max' are already other functions [infix notation]
-- Then ` in_range 0 5 3 => True ` ; ` in_rage 4 5 3 => False `

-- Types are defined 'name :: <type>'. But HK compiler usually deal with it, so is not always mandatory to write down the type.
-- Examples: ` x :: Integer ` then ` x = 1 ` ; ` y :: Bool ` then ` y = true `; ...

-- Functions have their own type, since functions are values. ' in_rage :: Ord a => a -> a -> a -> Bool ' for the 'in_range' function.

inRangeInteger :: Integer -> Integer -> Integer -> Bool
inRangeInteger min max x = x >= min && x <= max

-- For using variables inside a function we use ` let `, which binds a result to reference by the expression.

inRangeWithLet min max x =
    let inLowerBound = min <= x
        inUpperBound = max >= x
    in
        inLowerBound && inUpperBound

-- This could also be done trough ` where ` binding.

inRangeWithWhere min max x = ilb && iub
    where
        ilb = min <= x
        iub = max >= x

-- Control flows with if, for this case would be redundant but can be done in the following way:

inRangeWithIf min max x = 
    if ilb then iub else False
    where
        ilb = min <= x
        iub = max >= x

-- In fix functions enable to write function in between the arguments

