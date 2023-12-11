import Parser 
import Parser.Char
import Std.Data.HashMap
import Init

#check List

open Parser Parser.Char Std

def Coord := Int × Int
  deriving BEq, Hashable
def Coord.init : Coord := (0,0)

abbrev PError := Error.Simple Substring Char
abbrev CoordM := StateT Coord (ExceptT PError Id)
abbrev P := SimpleParserT Substring Char CoordM

instance monadLiftParserT [Monad m] [Parser.Stream σ α] [Parser.Error ε σ α] : MonadLift m (ParserT ε σ α m) where
  monadLift m state := (·, state) <$> m

def incX' (c : Int) : Coord → Coord 
  | (x,y) => (x+c,y)
def incY' (c : Int) : Coord → Coord 
  | (_,y) => (0,y+c)
def incX (c : Int) : P Unit := liftM <| (modify <| incX' c : CoordM Unit)
def incY (c : Int) : P Unit := liftM <| (modify <| incY' c : CoordM Unit)
def getCoord : P Coord := liftM <| (get : CoordM _) 

structure Entry where
  part : Option (Coord × Nat)
  symbol : Bool

def Entry.empty : Entry := ⟨none, false⟩
def Entry.combine : Entry → Entry → Entry
    | ⟨p₁, s₁⟩, ⟨p₂, s₂⟩ => ⟨Option.merge (Function.const _) p₁ p₂, s₁ && s₂⟩
def Entry.isValidPart (entry : Entry) : Bool := entry.part.isSome && entry.symbol

structure Span where
  entry : Entry
  coords : List Coord

def Span.mkPart (span : Nat) (n : Nat) (coord : Coord) : Span where
  entry := ⟨some (coord, n), false⟩
  coords := (flip incX' coord ∘ Int.ofNat) <$> List.range span

def Span.mkSymbol (coord : Coord) : Span where
  entry := ⟨none, true⟩
  coords := Id.run do
    let mut res := []
    for dx in [-1, 0, 1] do
      for dy in [-1, 0, 1] do
        res := ⟨coord.1 + dx, coord.2 + dy⟩ :: res
    pure res

def EntryMap := HashMap Coord Entry
def EntryMap.empty : EntryMap := HashMap.empty
def EntryMap.combine : EntryMap → EntryMap → EntryMap := HashMap.mergeWith (Function.const _ Entry.combine)

def P.run {α : Type} (parser : P α) (string : String) : Except PError (Coord × α) := 
  ParserT.run parser string |>.run Coord.init |>.bind λ
    | (Result.ok _ res, coord) => pure (coord, res)
    | (Result.error err, _) => throw err

def parseDot : P Unit := pure () <* char '.' <* incX 1
def parseSymbol : P Span := notFollowedBy (char '.' <|> ASCII.numeric) *> anyToken *> (Span.mkSymbol <$> getCoord) <* incX 1
def parsePart : P Span := do
  let p₁ ← String.Pos.byteIdx <$> getPosition
  let n ← ASCII.parseNat
  let p₂ ← String.Pos.byteIdx <$> getPosition
  let span := (p₂ - p₁)
  Span.mkPart span n <$> getCoord <* incX span
def parseNextEntry : P Span := takeMany parseDot *> (parseSymbol <|> parsePart)
def parseEol : P Unit := eol *> incY 1

def insertSpan (entries : EntryMap) (span : Span) : EntryMap :=
  entries.combine <| HashMap.ofList <| (., span.entry) <$> span.coords
      
def parseLine : P EntryMap := Parser.foldl insertSpan (pure EntryMap.empty) parseNextEntry <* (parseEol <|> endOfInput)
def parseLines : P EntryMap := Parser.foldl EntryMap.combine (pure EntryMap.empty) parseLine <* endOfInput

#check HashMap

def part1 : IO Unit := do
  IO.println "Day 3 Part 1"
  let input ← IO.FS.readFile "./input/day3"
  match P.run parseLines input with
    | .error err => IO.println err
    | .ok ⟨_, entries⟩ => 
      entries.filter (Function.const _ Entry.isValidPart) 
      |>.toList 
      |>.map (flip Option.getD (Coord.init, 0) ∘ Entry.part ∘ (·.2)) 
      |> HashMap.ofList 
      |>.fold (fun sum _ n => sum + n) 0 
      |> IO.println
