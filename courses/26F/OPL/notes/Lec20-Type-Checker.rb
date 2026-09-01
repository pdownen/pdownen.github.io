# coding: utf-8
#############################################
###      Implementing a Type Checker      ###
###                                       ###
### Organization of Programming Languages ###
###               Fall 2025               ###
###              Paul Downen              ###
#############################################


################################
## Context-Free Type Checking ##
################################

# The Mixed language of conditions and arithmetic

#     n ::= 0 | 1 | 2 | 3 | ...
#     b ::= true | false

#     M ::= n | b
#         | plus(M, M) | minus(M, M)
#         | geq?(M, M) | or(M, M)
#         | if(M, M, M)

#     T ::= number | boolean

# Translation between abstract syntax and hash-table representation:
#
#     {num:   n}             =  n
#     {bool:  b}             =  b
#     {plus:  [M₁, M₂]}      =  plus(M₁, M₂)
#     {minus: [M₁, M₂]}      =  minus(M₁, M₂)
#     {geq?:  [M₁, M₂]}      =  geq?(M₁, M₂)
#     {or:    [M₁, M₂]}      =  or(M₁, M₂)
#     {if:    [M₁, M₂, M₃]}  =  if(M₁, M₂, M₃)

#     :number   =  number
#     :boolean  =  boolean

# infer_type(M) returns T  when  M : T is derivable
def infer_type(expr)
  case expr
  # ——————————
  # n : number

  in {num: n}
    :number

  # ———————————
  # b : boolean

  in {bool: b}
    :boolean
    
  # M₁ : number    M₂ : number
  # ——————————————————————————
  # plus(M₁, M₂) : number

  in {plus: [m1, m2]}
    if infer_type(m1) == :number and
       infer_type(m2) == :number
    then
      :number
    else
      raise TypeError.new(
              "Not a number in plus: #{m1} or #{m2}")
    end

  # M₁ : number    M₂ : number
  # ——————————————————————————
  # minus(M₁, M₂) : number

  in {minus: [m1, m2]}
    case [infer_type(m1), infer_type(m2)]
    in :number, :number
      :number
    else
      raise TypeError.new(
              "Not a number in minus: #{m1} or #{m2}")
    end

  # M₁ : number    M₂ : number
  # ——————————————————————————
  # geq?(M₁, M₂) : boolean

  in {geq?: [m1, m2]}
    case [infer_type(m1), infer_type(m2)]
    in :number, :number
      :boolean
    else
      raise TypeError.new(
              "Not a number in geq?: #{m1} or #{m2}")
    end

  # M₁ : boolean    M₂ : boolean
  # ————————————————————————————
  # or(M₁, M₂) : boolean

  in {or: [m1, m2]}
    case [infer_type(m1), infer_type(m2)]
    in :boolean, :boolean
      :boolean
    else
      raise TypeError.new(
              "Not a boolean in or: #{m1} or #{m2}")
    end
    
  # M₁ : boolean    M₂ : T    M₃ : T
  # ————————————————————————————————
  # if(M₁, M₂, M₃) : T

  in {if: [m1, m2, m3]}
    case infer_type(m1)
    in :boolean
      ty2 = infer_type(m2)
      ty3 = infer_type(m3)

      if ty2 == ty3
      then
        return ty2
      else
        raise TypeError.new(
                "Return types of if don't match: #{m2}:#{ty2} vs #{m3}:#{ty3}")
      end
    else
      raise TypeError.new(
              "Not a boolean in if: #{m1}")
    end
  end
end


# ex1 = (5 + 4) - 3
# ex1 = minus(plus(5,4),3)
ex1   = {minus: [{plus: [{num: 5}, {num: 4}]},
                 {num: 3}]}

# ex2 = 5 + 1 - (2 + 2)
# ex2 = minus(plus(5,1),plus(2,2))
ex2   = {minus: [{plus: [{num: 5}, {num: 1}]},
                 {plus: [{num: 2}, {num: 2}]}]}

# ex3 = if 1 + 1 >= 2 then (1 + 1) - 2 else 0
# ex3 = if(geq?(plus(1,1),2), minus(plus(1,1), 2), 0)
ex3   = {if: [{geq?: [{plus: [{num: 1}, {num: 1}]},
                      {num: 2}]},
              {minus: [{plus: [{num: 1}, {num: 1}]},
                       {num: 2}]},
              {num: 0}]}

# ex4 = true or 0 - 2 >= 0
# ex4 = or(true,geq?(minus(0,2), 0))
ex4   = {or: [{bool: true},
              {geq?: [{minus: [{num: 0}, {num: 2}]},
                      {num: 0}]}]}

# ex5 = 3 >= 4 or 4 >= 5
# ex5 = or(geq?(3, 4), geq?(4, 5))
ex5   = {or: [{geq?: [{num: 3}, {num: 4}]},
              {geq?: [{num: 4}, {num: 5}]}]}

# ex6 = 1 + false
# ex6 = plus(1, false)
ex6 = {plus: [{num: 1}, {bool: false}]}

# ex7 = if true then 1 else false
ex7 = {if: [{bool: true}, {num: 1}, {bool: false}]}

examples = [ex1, ex2, ex3, ex4, ex5, ex6, ex7]

for ex in examples
  begin
    puts "", ex, ":", infer_type(ex)
  rescue TypeError => err
    puts "", ex, "⌢̈", err
  end
end

###############################
## Depending on your Context ##
###############################

# Lets add variables (x) into the language

#     x ::= any symbolic name ...
#     n ::= 0 | 1 | 2 | 3 | ...
#     b ::= true | false

#     M ::= n | b
#         | plus(M, M) | minus(M, M)
#         | geq?(M, M) | or(M, M)
#         | if(M, M, M)
#         | let(x, M, M)

#     T ::= number | boolean
#     Γ ::= ε | Γ, x:T

#     ε       =  {}               # the empty hash table
#     Γ, x:T  =  Γ.merge({x: T})  # extending a hash table

gamma1 = {x: :number, y: :boolean}
gamma2 = gamma1.merge({z: :string})
puts "", "before:", gamma1, "after:", gamma2


# infer_type_in(M, Γ) returns T  when  Γ ⊢ M : T is derivable
def infer_type_in(expr, env)
  case expr
  # ——————————————
  # Γ ⊢ n : number

  in {num: n}
    :number

  # ———————————————
  # Γ ⊢ b : boolean

  in {bool: b}
    :boolean

  # (x:T) ∈ Γ
  # —————————
  # Γ ⊢ x : T

  in {var: x}
    ty = env[x]
    return ty if ty
    raise TypeError.new("Unknown variable #{x}")

  # Γ ⊢ M₁ : T₁    Γ, x:T₁ ⊢ M₂ : T₂
  # ————————————————————————————————
  # Γ ⊢ let(x, M₁, M₂) : T₂
    
  in {let: [x, m1, m2]}
    ty1 = infer_type_in(m1, env)
    infer_type_in(m1, env.merge({x => ty1}))

  # Γ ⊢ M₁ : number    Γ ⊢ M₂ : number
  # ——————————————————————————————————
  # Γ ⊢ plus(M₁, M₂) : number

  in {plus: [m1, m2]}
    if infer_type_in(m1, env) == :number and
       infer_type_in(m2, env) == :number
    then
      :number
    else
      raise TypeError.new(
              "Not a number in plus: #{m1} or #{m2}")
    end

  # Γ ⊢ M₁ : number    Γ ⊢ M₂ : number
  # ——————————————————————————————————
  # Γ ⊢ minus(M₁, M₂) : number

  in {minus: [m1, m2]}
    case [infer_type_in(m1, env), infer_type_in(m2, env)]
    in :number, :number
      :number
    else
      raise TypeError.new(
              "Not a number in minus: #{m1} or #{m2}")
    end

  # Γ ⊢ M₁ : number    Γ ⊢ M₂ : number
  # ——————————————————————————————————
  # Γ ⊢ geq?(M₁, M₂) : boolean

  in {geq?: [m1, m2]}
    case [infer_type_in(env, m1), infer_type_in(env, m2)]
    in :number, :number
      :boolean
    else
      raise TypeError.new(
              "Not a number in geq?: #{m1} or #{m2}")
    end

  # Γ ⊢ M₁ : boolean    Γ ⊢ M₂ : boolean
  # ————————————————————————————————————
  # Γ ⊢ or(M₁, M₂) : boolean

  in {or: [m1, m2]}
    case [infer_type_in(m1, env), infer_type_in(m2, env)]
    in :boolean, :boolean
      :boolean
    else
      raise TypeError
    end
    
  # Γ ⊢ M₁ : boolean    Γ ⊢ M₂ : T    Γ ⊢ M₃ : T
  # ————————————————————————————————————————————
  # Γ ⊢ if(M₁, M₂, M₃) : T

  in {if: [m1, m2, m3]}
    case infer_type_in(m1, env)
    in :boolean
      ty2 = infer_type_in(m2, env)
      ty3 = infer_type_in(m3, env)

      if ty2 == ty3
      then
        return ty2
      else
        raise TypeError.new(
                "Return types of if don't match: #{m2}:#{ty2} vs #{m3}:#{ty3}")
      end
    else
      raise TypeError.new(
              "Not a boolean in if: #{m1}")
    end
  end
end

init_env = {x: :number,
            y: :number,
            z: :boolean}

# ex8 = x + y
# ex8 = plus(x, y)
ex8 = {plus: [{var: :x}, {var: :y}]}

# ex9 = if z then x else y
# ex9 = if(z, x, y)
ex9 = {if: [{var: :z}, {var: :x}, {var: :y}]}

# ex10 = if z then false else true
# ex10 = if(z, false, true)
ex10 = {if: [{var: :z}, {bool: false}, {bool: true}]}

# ex11 = x + w
# ex11 = plus(x, w)
ex11 = {plus: [{var: :x}, {var: :w}]}

# ex12 = let w = 3 in x + w
# ex12 = let(w, 3, plus(x, w))
ex12 = {let: [{var: :w}, {num: 3}, {plus: [{var: :x}, {var: :w}]}]}

var_examples = [ex8, ex9, ex10, ex11, ex12]

for ex in var_examples
  begin
    puts "", init_env, "⊢", ex, ":", infer_type_in(ex, init_env)
  rescue TypeError => err
    puts "", init_env, "⊢", ex, "⌢̈", err
  end
end
