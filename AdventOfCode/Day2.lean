import Parser 
import Parser.Char
import Std.Data.HashMap

open Parser Parser.Char Std

abbrev Round := Array (Nat × String)

structure Game where
  id : Nat 
  rounds : Array Round

abbrev P := SimpleParser Substring Char

abbrev Constraint := HashMap String Nat

def parseRound : P Round := do
    sepBy1 (string ", ") ((·,·) 
      <$> (ASCII.parseNat <* char ' ')
      <*> (String.mk <$> Array.toList <$> (takeMany1 ASCII.alpha)))
def parseGame : P Game := do
  _ ← string "Game "
  let id ← ASCII.parseNat
  _ ← string ": "
  let round ← sepBy1 (string "; ") parseRound <* eol
  pure ⟨id, round⟩

def isValidRound (c : Constraint) (r : Round) : Bool := r.all λ ⟨num, color⟩ => num <= c.findD color 0
def isValidGame (c : Constraint) (g : Game) := g.rounds.all <| isValidRound c
def gameSum (c : Constraint) (gs : Array Game) := gs.filter (isValidGame c) |>.map Game.id |>.foldr (·+·) 0

def constraint : Constraint := 
  HashMap.ofList [
    ("red", 12),  
    ("green", 13),  
    ("blue", 14),  
  ]

def zeroConstraint : Constraint := 
  HashMap.ofList [
    ("red", 0),  
    ("green", 0),  
    ("blue", 0),  
  ]

def minConstraint (g : Game): Constraint := g.rounds 
  |>.map (Array.foldr (λ (num, color) m => HashMap.insert m color num) HashMap.empty) 
  |>.foldr (HashMap.mergeWith $ Function.const (β:=String) Nat.max) zeroConstraint

def constraintPower (c : Constraint) := c.toList.map (·.2) |> List.foldr (·*·) 1
def constraintPowerSum (gs : Array Game) := gs.map (constraintPower ∘ minConstraint) |>.foldr (·+·) 0


def part1 : IO Unit := do
  IO.println "Day 2 Part 1"
  let input ← IO.FS.readFile "./input/day2"
  let games := Parser.run (takeMany1 parseGame <* endOfInput) input 
  match games with
    | .error err => IO.eprintln err
    | .ok _ games => IO.println (gameSum constraint games)

def part2 : IO Unit := do
  IO.println "Day 2 Part 2"
  let input ← IO.FS.readFile "./input/day2"
  let games := Parser.run (takeMany1 parseGame <* endOfInput) input 
  match games with
    | .error err => IO.eprintln err
    | .ok _ games => IO.println (constraintPowerSum games)
