import Parser 
import Parser.Char
import Std.Data.HashMap
import Init

open Parser Parser.Char Std

def Coord := Int × Int
  deriving BEq, Hashable, ToString
instance : LT Coord where
  lt := Prod.lexLt
instance (c₁ c₂ : Coord) : Decidable (LT.lt c₁ c₂) := Prod.lexLtDec c₁ c₂
  
  
def Coord.init : Coord := (0,0)
def Coord.transpose : Coord → Coord
  | (x, y) => (y, x)

abbrev PError := Error.Simple Substring Char
abbrev CoordM := StateT Coord (ExceptT PError IO)
abbrev P := SimpleParserT Substring Char CoordM
abbrev SymMap := HashMap Coord Char
def SymMap.gears : SymMap → SymMap := HashMap.filter (Function.const _ (·=='*')) 
def SymMap.hasGears : SymMap → Bool := (. |> SymMap.gears |>.size |> (· != 0))

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
  symbols : HashMap Coord Char
instance : ToString Entry where
  toString
    | ⟨some _, hsyms⟩ => if SymMap.hasGears hsyms then ";" else "."
    | ⟨none, hsyms⟩ => if SymMap.hasGears hsyms then "," else " "

def Entry.empty : Entry := ⟨none, HashMap.empty⟩
def Entry.combine : Entry → Entry → Entry
    | ⟨p₁, s₁⟩, ⟨p₂, s₂⟩ => ⟨Option.merge (Function.const _) p₁ p₂, HashMap.mergeWith (λ _ _ _ => panic "Should not have the same symbol twice") s₁ s₂⟩
def Entry.isValidPart (entry : Entry) : Bool := entry.part.isSome && entry.symbols.size != 0

structure Span where
  entry : Entry
  coords : List Coord

def Span.mkPart (span : Nat) (n : Nat) (coord : Coord) : Span where
  entry := ⟨some (coord, n), HashMap.empty⟩
  coords := (flip incX' coord ∘ Int.ofNat) <$> List.range span

def Span.mkSymbol (s : Char) (coord : Coord) : Span where
  entry := ⟨none, HashMap.ofList <| List.pure (coord, s)⟩
  coords := Id.run do
    let mut res := []
    for dx in [-1, 0, 1] do
      for dy in [-1, 0, 1] do
        res := ⟨coord.1 + dx, coord.2 + dy⟩ :: res
    pure res

protected def Array.slidingMap (arr : Array α) (f : Option α → α → β) : Array β := Id.run do
  let mut x := #[]
  for h : i in [:arr.size] do
    let prev := match i with
      | 0 => none
      | .succ j => arr[j]?
      
    let curr := arr[i]'h.upper
    x := x.push <| f prev curr
  return x
    

def EntryMap := HashMap Coord Entry

protected def Char.repeat (c : Char) (n : Nat) : String := String.mk <| List.replicateTR n c

instance : ToString EntryMap where
  toString em := em 
    |>.filter (flip <| Function.const _ λ ⟨x, y⟩ => 0 <= x && 0 <= y)
    |>.toArray |>.insertionSort (λ ⟨c₁,_⟩ ⟨c₂,_⟩ => LT.lt c₁.transpose c₂.transpose |> decide)
    |>.slidingMap (λ
      | .none, y => ' '.repeat y.1.1.toNat ++ toString y.2
      | .some x, y => if x.1.2 < y.1.2 
          then "\n" ++ ' '.repeat y.1.1.toNat ++ toString y.2
          else ' '.repeat (y.1.1.toNat - x.1.1.toNat - 1) ++ toString y.2
        )
    |>.foldl String.append ""

def EntryMap.empty : EntryMap := HashMap.empty
def EntryMap.combine : EntryMap → EntryMap → EntryMap := HashMap.mergeWith (Function.const _ Entry.combine)

def P.run {α : Type} (parser : P α) (string : String) : IO (Except PError (Coord × α)) := 
  ParserT.run parser string |>.run Coord.init |>.bind λ
    | (Result.ok _ res, coord) => pure (coord, res)
    | (Result.error err, _) => throw err

def parseDot : P Unit := incX =<< count (char '.')
def parseSymbol : P Span := Span.mkSymbol <$> (notFollowedBy (char '.' <|> ASCII.numeric <|> eol) *> anyToken) <*> getCoord <* incX 1
def parsePart : P Span := do
  let p₁ ← String.Pos.byteIdx <$> getPosition
  let n ← ASCII.parseNat
  let p₂ ← String.Pos.byteIdx <$> getPosition
  let span := (p₂ - p₁)
  Span.mkPart span n <$> getCoord <* incX span
def parseNextEntry : P Span := parseDot *> (parsePart <|> parseSymbol)

def insertSpan (entries : EntryMap) (span : Span) : EntryMap :=
  entries.combine <| HashMap.ofList <| (., span.entry) <$> span.coords
      
def parseLine : P EntryMap := flip insertSpan <$> parseNextEntry <*> (Parser.foldl insertSpan EntryMap.empty parseNextEntry <* parseDot <* (Functor.mapConst () eol <|> lookAhead endOfInput) <* incY 1)
def parseLines : P EntryMap := Parser.foldl EntryMap.combine EntryMap.empty parseLine <* endOfInput

def part1 : IO Unit := do
  IO.println "Day 3 Part 1"
  let input ← IO.FS.readFile "./input/day3"
  match ←P.run parseLines input with
    | .error err => IO.println err
    | .ok ⟨_, entries⟩ => do
      IO.println entries
      entries.filter (Function.const _ Entry.isValidPart) 
        |>.toList 
        |>.map (flip Option.getD (Coord.init, 0) ∘ Entry.part ∘ (·.2)) 
        |> HashMap.ofList 
        |>.fold (fun sum _ n => sum + n) 0 
        |> IO.println

def part2 : IO Unit := do
  IO.println "Day 3 Part 2"
  let input ← IO.FS.readFile "./input/day3"
  match ←P.run parseLines input with
    | .error err => IO.println err
    | .ok ⟨_, entries⟩ => do
      let res := entries.toList 
        |>.bind (λ 
          | (_, entry) => 
            entry.symbols 
              |> SymMap.gears
              |>.toList 
              |>.bind (fun ⟨coord,_⟩ => 
                ((coord,·) ∘ List.pure) <$> entry.part |>.toList)) 
        |> flip HashMap.ofListWith List.append 
        |>.filterMap (λ _ parts => parts |> HashMap.ofList |>.toList |>.map Prod.snd |> Option.some |>.filter (Nat.beq 2 ∘ List.length) |>.map (List.foldl (·*·) 1))
        |>.toArray 
        /- |>.insertionSort (λ (c₁,_) (c₂, _) => LT.lt c₁.transpose c₂.transpose) -/
        |>.map Prod.snd
        |>.foldl (.+.) 0
      IO.println res

