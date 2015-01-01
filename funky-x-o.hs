import Data.List;
import qualified Data.Sequence as S;
import qualified Data.Foldable as F;
import Data.Maybe;

data Player = One | Two deriving (Eq)

instance Show Player where
    show One = "X"
    show Two = "O"

type Location = (Int, Int)
type Board = [[Either String Player]]

showTile :: Either String Player -> String
showTile (Right p) = show p
showTile (Left s) = s

showBoard :: Board -> String
showBoard = concat . intercalate ["\n"] . map (map showTile)

nextPlayer :: Player -> Player
nextPlayer One = Two
nextPlayer Two = One

toLocation :: Board -> Int -> Location
toLocation b n = (r, c)
    where r = n `div` (length b)
          c = n `mod` (length . head $ b)

isLeft :: Either a b -> Bool
isLeft (Left a) = True
isLeft _        = False

isRight :: Either a b -> Bool
isRight = not . isLeft

isValid :: Board -> Location -> Bool
isValid board (r, c) = r >= 0 && r < length board
                       && c >= 0 && c < (length . head $ board)
                       && isLeft (board !! r !! c)

setLocation :: Location -> Player -> Board -> Board
setLocation (r, c) player board = updatedBoard
    where row = board !! r
          updatedRow = F.toList . S.update c (Right player) . S.fromList $ row
          updatedBoard = F.toList . S.update r updatedRow . S.fromList $ board

move :: Location -> Player -> Board -> Board
move loc player board
    | isValid board loc = setLocation loc player board
    | otherwise         = board

winner :: Board -> Maybe Player
winner board
    | length winningWays == 0 = Nothing
    | otherwise               = head winningWays
    where rows = id
          cols = transpose
          diagonals [[a1, _, b1],
                     [ _, c,  _],
                     [b2, _, a2]] = [[a1, c, a2], [b1, c, b2]]
          ways = rows board ++ cols board ++ diagonals board
          same w = all (== head w) (tail w)
          winningWays = [Just p | way@((Right p):_) <- ways, same way]

isTied :: Board -> Bool
isTied b = (isNothing . winner $ b) && (all isRight . concat $ b)

initialBoard :: Board
initialBoard = [[Left . show $ r * 3 + c + 1 | c <- [0..2]] | r <- [0..2]]

game :: Board -> Player -> IO ()
game board player = do
    putStrLn ""
    putStrLn . showBoard $ board
    let win = winner board
    case win of
        Just p  -> putStrLn ((show p) ++ " wins!")
        Nothing -> if (isTied board)
                       then putStrLn "Cat's game!"
                       else do
                           putStrLn ((show player) ++ "'s Turn:")
                           userInput <- getLine
                           let loc = toLocation board . pred . read $ userInput
                           if (isValid board loc)
                               then do
                                   let newBoard = move loc player board
                                   game newBoard (nextPlayer player)
                               else do
                                   putStrLn "Invalid move."
                                   game board player
    return ()

main = game initialBoard One
