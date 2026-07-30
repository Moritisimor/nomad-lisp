type expr = 
  | Lambda of string list * expr list
  | Symbol of string
  | NumLit of float
  | StringLit of string
  | BoolLit of bool
  | List of expr list
  | Unit
