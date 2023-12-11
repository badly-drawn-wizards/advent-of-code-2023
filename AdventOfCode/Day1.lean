open Option List

def problem1part1 : IO Unit := do
  let input ← IO.FS.readFile "./input/day1"
  let lines := input.splitOn "\n"
  let lineCalibration : String → Int := 
    flip Option.getD 0 ∘ String.toInt? ∘ String.mk ∘ flip map [flip headD '0', flip getLastD '0'] ∘ flip (· $ ·) ∘ filter Char.isDigit ∘ String.data 
  let ans := foldr (· + ·) 0 $ map lineCalibration lines
  IO.println ans

def digitOfWordAssoc : List (String × Char) := [
  ("0", '0'),
  ("1", '1'),
  ("2", '2'),
  ("3", '3'),
  ("4", '4'),
  ("5", '5'),
  ("6", '6'),
  ("7", '7'),
  ("8", '8'),
  ("9", '9'),
  ("zero", '0'),
  ("one", '1'),
  ("two", '2'),
  ("three", '3'),
  ("four", '4'),
  ("five", '5'),
  ("six", '6'),
  ("seven", '7'),
  ("eight", '8'),
  ("nine", '9')
]

def findFirstWordDigitAux (assocMod : String → String) (str : String) : Option Char := do
  let mut res := none
  for pos in [:str.length] do
    let sub := str.drop pos
    for (word, digit) in digitOfWordAssoc do
      if (assocMod word).isPrefixOf sub then
        res := some digit
        break
    if res.isSome then break
  res
  -- (·.2) <$> flip List.find? digitOfWordAssoc (flip String.isPrefixOf s ∘ (·.1))

def findFirstWordDigit : String → Option Char := findFirstWordDigitAux id

def reverse := String.mk ∘ List.reverse ∘ String.data

def findLastWordDigit := findFirstWordDigitAux reverse ∘ reverse

def problem1part2 : IO Unit := do
  let input ← IO.FS.readFile "./input/day1"
  let lines := input.splitOn "\n"
  let lineCalibration str := ([·,·]) <$> findFirstWordDigit str <*> findLastWordDigit str >>= (String.toInt? ∘ String.mk) |>.getD 0
  let ans := foldr (· + ·) 0 $ map lineCalibration lines
  IO.println ans
