-- file: ch00/KMinima.hs
-- lines beginning with "--" are comments.
import Data.List

minima k xs = take k (sort xs)
