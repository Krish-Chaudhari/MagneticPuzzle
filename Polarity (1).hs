module Polarity (polarity) where

import Data.Maybe (isNothing, fromMaybe)
import Data.List (transpose)
import Control.Monad (guard)

type Grid = [[Char]]
type Pos = (Int, Int)
type Upd = (Int, Int, Char)
type Cnts = ([Int], [Int], [Int], [Int]) 


polarity :: [String] -> ([Int], [Int], [Int], [Int]) -> [String]
polarity brd (lft, rgt, top, bot) =
  let grd = brd
      rNum = length grd
      cNum = length (head grd)
      solInit = replicate rNum (replicate cNum Nothing)
      cntsInit = (replicate rNum 0, replicate rNum 0, replicate cNum 0, replicate cNum 0)
  in case solve grd solInit 0 rNum cNum (lft, rgt, top, bot) cntsInit of
       Nothing -> replicate rNum (replicate cNum 'X')
       Just sol -> map (map (fromMaybe 'X')) sol





solve :: Grid -> [[Maybe Char]] -> Int -> Int -> Int -> ([Int], [Int], [Int], [Int]) -> Cnts -> Maybe [[Maybe Char]]
solve _ sol p rNum cNum spec _
  | p == rNum * cNum =
      if finalChk sol spec then Just sol else Nothing

solve grd sol p rNum cNum spec cnts =
  let (r, c) = divMod p cNum
      cur = (sol !! r) !! c
  in if cur /= Nothing
     then solve grd sol (p + 1) rNum cNum spec cnts
     else tryOpt (genOpts grd sol r c rNum cNum) grd sol p rNum cNum spec cnts

tryOpt :: [[Upd]] -> Grid -> [[Maybe Char]] -> Int -> Int -> Int -> ([Int], [Int], [Int], [Int]) -> Cnts -> Maybe [[Maybe Char]]
tryOpt [] _ _ _ _ _ _ _ = Nothing
tryOpt (u:us) grd sol p rNum cNum spec cnts =
  let newSol = apply sol u
  in if adjOk newSol u rNum cNum
     then
       let newCnts = updCnts u cnts
       in if invalid newSol spec newCnts
          then tryOpt us grd sol p rNum cNum spec cnts
          else case solve grd newSol (p + 1) rNum cNum spec newCnts of
                 Just res -> Just res
                 Nothing -> tryOpt us grd sol p rNum cNum spec cnts
     else tryOpt us grd sol p rNum cNum spec cnts




genOpts :: Grid -> [[Maybe Char]] -> Int -> Int -> Int -> Int -> [[Upd]]
genOpts grd sol r c rNum cNum =
  let ch = (grd !! r) !! c
      base = [[(r, c, 'X')]]
  in case ch of
       'T' | r + 1 < rNum
           , (grd !! (r + 1)) !! c == 'B'
           , (sol !! (r + 1)) !! c == Nothing ->
               base ++ [[(r, c, '+'), (r + 1, c, '-')], [(r, c, '-'), (r + 1, c, '+')]]
       'L' | c + 1 < cNum
           , (grd !! r) !! (c + 1) == 'R'
           , (sol !! r) !! (c + 1) == Nothing ->
               base ++ [[(r, c, '+'), (r, c + 1, '-')], [(r, c, '-'), (r, c + 1, '+')]]
       _ -> base

apply :: [[Maybe Char]] -> [Upd] -> [[Maybe Char]]
apply sol us = foldl (\s (r, c, v) -> set2D s r c (Just v)) sol us

set2D :: [[a]] -> Int -> Int -> a -> [[a]]
set2D grd r c val =
  take r grd ++
  [take c (grd !! r) ++ [val] ++ drop (c + 1) (grd !! r)] ++
  drop (r + 1) grd




adjOk :: [[Maybe Char]] -> [Upd] -> Int -> Int -> Bool
adjOk sol us rNum cNum =
  all (\(r, c, v) ->
        if v `elem` "+-"
        then all (\(dr, dc) ->
                    let nr = r + dr
                        nc = c + dc
                    in nr < 0 || nc < 0 || nr >= rNum || nc >= cNum ||
                       (sol !! nr) !! nc /= Just v)
                 [(-1, 0), (1, 0), (0, -1), (0, 1)]
        else True)
      us



updCnts :: [Upd] -> Cnts -> Cnts
updCnts us (rp, rm, cp, cm) =
  foldl (\(rp', rm', cp', cm') (r, c, v) ->
           case v of
             '+' -> (inc rp' r, rm', inc cp' c, cm')
             '-' -> (rp', inc rm' r, cp', inc cm' c)
             _   -> (rp', rm', cp', cm'))
        (rp, rm, cp, cm)
        us
  where
    inc xs i = take i xs ++ [xs !! i + 1] ++ drop (i + 1) xs




invalid :: [[Maybe Char]] -> ([Int], [Int], [Int], [Int]) -> Cnts -> Bool
invalid sol (lft, rgt, top, bot) (rp, rm, cp, cm) =
  let rNum = length sol
      cNum = length (head sol)
      rBad = any (\r ->
                    let row = sol !! r
                        na = length (filter isNothing row)
                        pReq = lft !! r
                        mReq = rgt !! r
                    in (pReq /= -1 && rp !! r > pReq) ||
                       (pReq /= -1 && rp !! r + na < pReq) ||
                       (mReq /= -1 && rm !! r > mReq) ||
                       (mReq /= -1 && rm !! r + na < mReq))
                [0..rNum - 1]
      cBad = any (\c ->
                    let col = map (!! c) sol
                        na = length (filter isNothing col)
                        pReq = top !! c
                        mReq = bot !! c
                    in (pReq /= -1 && cp !! c > pReq) ||
                       (pReq /= -1 && cp !! c + na < pReq) ||
                       (mReq /= -1 && cm !! c > mReq) ||
                       (mReq /= -1 && cm !! c + na < mReq))
                [0..cNum - 1]
  in rBad || cBad







finalChk :: [[Maybe Char]] -> ([Int], [Int], [Int], [Int]) -> Bool
finalChk sol (lft, rgt, top, bot) =
  let rOk = all (\(row, i) ->
                  let p = length (filter (== Just '+') row)
                      m = length (filter (== Just '-') row)
                  in (lft !! i == -1 || p == lft !! i) &&
                     (rgt !! i == -1 || m == rgt !! i))
                (zip sol [0..])
      cols = transpose sol
      cOk = all (\(col, i) ->
                  let p = length (filter (== Just '+') col)
                      m = length (filter (== Just '-') col)
                  in (top !! i == -1 || p == top !! i) &&
                     (bot !! i == -1 || m == bot !! i))
                (zip cols [0..])
  in rOk && cOk
