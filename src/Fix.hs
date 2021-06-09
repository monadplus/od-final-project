module Fix where

newtype Fix f = Fix {unFix :: f (Fix f)}

fix :: (a -> a) -> a
fix f = let r = f r in r
