import System.Environment
import System.IO
import Data.List
import Data.Function
import Debug.Trace
maze_path = "C:\\Users\\Rares\\Desktop\\Haskill\\Fackin Jeleuri\\maze3.txt"



get :: [String] -> Int -> Int -> Char
get maze x y = (maze !! y) !! x

modify_list :: [a] -> Int -> a -> [a]
modify_list list pos new =
    let
        before = take  pos    list
        after  = drop (pos+1) list
    in
        before ++ [new] ++ after

set :: [String] ->Int -> Int -> Char -> [String]
set maze x y char =
    let
        line = maze !! y
        new_line = modify_list line x char
        new_maze = modify_list maze y new_line
    in
        new_maze



get_maze :: String -> IO [String]
get_maze path = do
        maze <- readFile path
        let output = lines maze
        return output




print_maze :: [String] -> IO ()
print_maze maze = do
       putStrLn $ unlines maze



is_wall :: [String] -> (Int, Int) -> Bool
is_wall maze (x,y) = if get maze x y == '#' then True else False


place_player :: [String] -> (Int, Int) -> [String]
place_player maze (x,y) =   set maze x y '@'


move :: (Int, Int) -> Char -> (Int, Int)
move (x,y) 'w' = (x,y-1)
move (x,y) 's' = (x,y+1)
move (x,y) 'a' = (x-1,y)
move (x,y) 'd' = (x+1,y)
move (x,y) _ = (x,y)


can_move :: [String] -> (Int, Int) -> Char -> Bool
can_move maze (x,y) direction = let
              moved = (move (x,y) direction)
                                in
              if is_wall maze moved == True then False else True


game_loop :: [String] -> (Int, Int) -> IO ()
game_loop maze (x,y) = do
              print_maze $ place_player maze (x,y)
              imput <- getLine
              let direction = imput !! 0
              if can_move maze (x,y) direction then game_loop maze (move (x,y) direction) else game_loop maze (x,y)




--My approach to the problem : Because the player starts in the top left corner and the goal is at the bottom right corner, the player should prioritise moving down first, then right, and lastly up or left
--When the player moves, it will mark every position he's been through with '.'
--every time the player gets stuck and can't go anywhere else except back on his own tracks, it will mark the current position with a 'b' indicating that the position belongs to a bad path that leads nowhere
--the places marked with b will be treated like walls.

is_bad_path maze (x,y) = if get maze x y == 'b' then True else False

have_been_there maze (x,y)  = get maze x y == '.'

can_move_plus maze (x, y) direction = can_move maze (x, y) direction && if is_bad_path maze (move (x, y) direction ) == True then False else True


find_a_path maze (x, y) (a, b)
    | (x, y) == (a, b) = [(x, y)]
    | (can_move_plus maze (x, y) 's' && not  (have_been_there maze (move (x,y) 's'))) = [(x, y)] ++ get_path (draw '.') (move (x, y) 's') (a,b)
    | (can_move_plus maze (x, y) 'd' && not  (have_been_there maze (move (x,y) 'd'))) = [(x, y)] ++ get_path (draw '.') (move (x, y) 'd') (a,b)
    | (can_move_plus maze (x, y) 'w' && not  (have_been_there maze (move (x,y) 'w'))) = [(x, y)] ++ get_path (draw '.') (move (x, y) 'w') (a,b)
    | (can_move_plus maze (x, y) 'a' && not  (have_been_there maze (move (x,y) 'a'))) = [(x, y)] ++ get_path (draw '.') (move (x, y) 'a') (a,b)
    | (can_move_plus maze (x, y) 's' && have_been_there maze (move (x,y) 's')) = [(x, y)] ++ get_path (draw 'b') (move (x, y) 's') (a,b)
    | (can_move_plus maze (x, y) 'd' && have_been_there maze (move (x,y) 'd')) = [(x, y)] ++ get_path (draw 'b') (move (x, y) 'd') (a,b)
    | (can_move_plus maze (x, y) 'w' && have_been_there maze (move (x,y) 'w')) = [(x, y)] ++ get_path (draw 'b') (move (x, y) 'w') (a,b)
    | (can_move_plus maze (x, y) 'a' && have_been_there maze (move (x,y) 'a')) = [(x, y)] ++ get_path (draw 'b') (move (x, y) 'a') (a,b)
    where
        draw char = set maze x y char





--this is a function that removes the bad paths that are still in the frontier, leaving only the correct one.
remove_bad_paths [] = []
remove_bad_paths ((x, y):paths)
      | (elem (x, y) paths) = [(x, y)] ++ remove_bad_paths (reverse $ takeWhile (\a -> a /= (x, y)) (reverse paths))
      | otherwise = [(x, y)] ++ remove_bad_paths paths

get_path :: [String] -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
get_path maze (x, y) (a, b) =    find_a_path maze (x, y) (a, b)




print_path maze [] = maze
print_path maze ((x, y):paths) = print_path (set maze x y '.') paths


main = do
    args <- getArgs
    let maze = head args
    m <- get_maze maze
    let x = length (m !! 0)
        y = length m
        path = get_path m (1, 1) (x - 2, y - 2)
    print_maze $ print_path m path
