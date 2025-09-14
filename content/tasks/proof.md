+++
title = "Proof"
description = ""
date = 2019-08-28T19:30:06Z
aliases = []
[extra]
id = 2429
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "acl2",
  "agda",
  "coq",
  "haskell",
  "idris",
  "j",
  "ocaml",
  "omega",
  "phix",
  "racket",
  "tcl",
  "twelf",
  "salmon"
]
+++

{{Omit From|Ruby}} <!-- same reason as Tcl -->
{{Omit From|ALGOL 68}} <!-- might be possible, but would require a complex library like Maple, not sure -->
This task only makes sense for [[wp:dependently-typed language|dependently-typed languages]] and [[wp:proof assistant|proof assistants]], or for languages with a type system strong enough to emulate certain dependent types. It does ''not'' ask you to implement a theorem prover yourself.

In the following task the word "define" implies the need to build the system of [[wp:Peano_axioms|Peano axioms]] using the language itself, that is a way to construct natural and even natural numbers in the [[wp:Canonical_form|canonical forms]], as well as a definition of the rules of addition and a way to construct all other acceptable terms. The word "prove" means that some form of [[wp:Unification_(computer_science)|logical unification]] is used (i.e., it requires a type checker in the case of languages ​​with dependent types and a verifying algorithm in the case of proof assistants). Thus, the metatheory of a language must be expressive enough to allow embedding of the Peano axioms and the opportunity to carry out [[wp:Constructive_proof|constructive proofs]]. Examples of the ''trusted'' mathematical metatheories can be given: [[wp:System_F|SystemF]] for Haskell, [[wp:Intuitionistic_type_theory|MLTT]] for Agda, [[wp:Calculus_of_constructions|CoC]]/[[wp:Calculus_of_inductive_constructions|CoIC]] for Coq.

'''Task''':

# To illustrate the possibility of type formation and type introduction:
## Define a countably infinite set of natural numbers {0, 1, 2, 3, ...}.
## Define a countably infinite set of even natural numbers {0, 2, 4, 6, ...} within the previously defined set of natural numbers.
## Define a countably infinite set of odd natural numbers {1, 3, 5, 7, ...} within the previously defined set of natural numbers.
# To illustrate the possibility of type elimination:
## Define the addition on natural numbers.
# To demonstrate constructive proofs:
## Prove that the addition of ''any'' two even numbers is even.
## Prove that the addition is ''always'' associative.
## Prove that the addition is ''always'' commutative.
## Try to prove that the addition of ''any'' two even numbers is odd (it should be rejected).
# To demonstrate the ability of disproofs:
## Prove that the addition of ''any'' two even numbers cannot be odd.
## Try to prove that the addition of ''any'' two even numbers cannot be even (it should be rejected).

3. and 4. can't be done using a simple number enumeration since there is a countable many natural numbers which is quantified in propositions.


## ACL2


3.1. using built-in natural numbers:


```Lisp
(thm (implies (and (evenp x) (evenp y))
              (evenp (+ x y))))
```



## Agda


```agda
module PeanoArithmetic where

-- 1.1. The natural numbers.
-- 
--   ℕ-formation:     ℕ is set.
-- 
--   ℕ-introduction:  0 ∈ ℕ,
--                    a ∈ ℕ | (1 + a) ∈ ℕ.
-- 
data ℕ : Set where
  zero : ℕ
  1+_  : ℕ → ℕ

{-# BUILTIN NATURAL ℕ    #-}
{-# BUILTIN ZERO    zero #-}
{-# BUILTIN SUC     1+_  #-}

-- 2.1. The rule of addition.
-- 
--   via ℕ-elimination.
-- 
infixl 6 _+_
_+_ : ℕ → ℕ → ℕ
0    + n = n
1+ m + n = 1+ (m + n)

-- 1.2. The even natural numbers.
-- 
data 2×ℕ : ℕ → Set where
  zero : 2×ℕ 0
  2+_  : {m : ℕ} → 2×ℕ m → 2×ℕ (2 + m)

-- 1.3. The odd natural numbers.
-- 
data 2×ℕ+1 : ℕ → Set where
  one : 2×ℕ+1 1
  2+_ : {m : ℕ} → 2×ℕ+1 m → 2×ℕ+1 (2 + m)

-- 3.1. Sum of any two even numbers is even.
-- 
--   This function takes any two even numbers and returns their sum as an even
--   number, this is the type, i.e. logical proposition, algorithm itself is a
--   proof which builds a required term of a given (inhabited) type, and the
--   typechecker performs that proof (by unification, so that this is a form of
--   compile-time verification).
-- 
even+even≡even : {m n : ℕ} → 2×ℕ m → 2×ℕ n → 2×ℕ (m + n)
even+even≡even zero   n = n
even+even≡even (2+ m) n = 2+ (even+even≡even m n)

-- The identity type for ℕ (for propositional equality).
-- 
infix 4 _≡_
data _≡_ (m : ℕ) : ℕ → Set where
  refl : m ≡ m

sym : {m n : ℕ} → m ≡ n → n ≡ m
sym refl = refl

trans : {m n p : ℕ} → m ≡ n → n ≡ p → m ≡ p
trans refl n≡p = n≡p

-- refl, sym and trans forms an equivalence relation.

cong : {m n : ℕ} → m ≡ n → 1 + m ≡ 1 + n
cong refl = refl

-- 3.2.1. Direct proof of the associativity of addition using propositional
-- equality.
-- 
+-associative : (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
+-associative 0      _ _ = refl
+-associative (1+ m) n p = cong (+-associative m n p)

-- Proof _of_ mathematical induction on the natural numbers.
-- 
--   P 0, ∀ x. P x → P (1 + x) | ∀ x. P x.
-- 
ind : (P : ℕ → Set) → P 0 → ((m : ℕ) → P m → P (1 + m)) → (m : ℕ) → P m
ind _ P₀ _    0      = P₀
ind P P₀ next (1+ n) = next n (ind P P₀ next n)

-- 3.2.2. The associativity of addition by induction (with propositional
-- equality, again).
-- 
+-associative′ : (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
+-associative′ m n p = ind P P₀ is m
  where
    P : ℕ → Set
    P m = m + n + p ≡ m + (n + p)
    P₀ : P 0
    P₀ = refl
    is : (m : ℕ) → P m → P (1 + m)
    is _ Pi = cong Pi

-- Syntactic sugar for equational reasoning (we don't use preorders here).

infix 4 _≋_
data _≋_ (m n : ℕ) : Set where
  refl : m ≡ n → m ≋ n

infix 1 begin_
begin_ : {m n : ℕ} → m ≋ n → m ≡ n
begin refl m≡n = m≡n

infixr 2 _~⟨_⟩_
_~⟨_⟩_ : (m : ℕ){n p : ℕ} → m ≡ n → n ≋ p → m ≋ p
_ ~⟨ m≡n ⟩ refl n≡p = refl (trans m≡n n≡p)

infix 2 _∎
_∎ : (m : ℕ) → m ≋ m
_∎ _ = refl refl

-- Some helper proofs.

m+0≡m : (m : ℕ) → m + 0 ≡ m
m+0≡m 0      = refl
m+0≡m (1+ m) = cong (m+0≡m m)

m+1+n≡1+m+n : (m n : ℕ) → m + (1 + n) ≡ 1 + (m + n)
m+1+n≡1+m+n 0      n = refl
m+1+n≡1+m+n (1+ m) n = cong (m+1+n≡1+m+n m n)

-- 3.3. The commutativity of addition using equational reasoning.
-- 
+-commutative : (m n : ℕ) → m + n ≡ n + m
+-commutative 0      n = sym (m+0≡m n)
+-commutative (1+ m) n =
  begin
    1+ m + n   ~⟨ refl ⟩
    1+ (m + n) ~⟨ cong (+-commutative m n) ⟩
    1+ (n + m) ~⟨ sym (m+1+n≡1+m+n n m) ⟩
    n + 1+ m
  ∎

-- 3.4.
-- 
even+even≡odd : {m n : ℕ} → 2×ℕ m → 2×ℕ n → 2×ℕ+1 (m + n)
even+even≡odd zero zero = {!!}
even+even≡odd _ _ = {!!}
-- ^
-- That gives
-- 
--   ?0 : 2×ℕ+1 (zero + zero)
--   ?1 : 2×ℕ+1 (.m + .n)
-- 
-- but 2×ℕ+1 (zero + zero) = 2×ℕ+1 0 which is uninhabited, so that this proof
-- can not be writen.
-- 

-- The absurd (obviously uninhabited) type.
-- 
--   ⊥-introduction is empty.
-- 
data ⊥ : Set where

-- The negation of a proposition.
-- 
infix  6 ¬_
¬_ : Set → Set
¬ A = A → ⊥

-- 4.1. Disproof or proof by contradiction.
-- 
--   To disprove even+even≡odd we assume that even+even≡odd and derive
--   absurdity, i.e. uninhabited type.
-- 
even+even≢odd : {m n : ℕ} → 2×ℕ m → 2×ℕ n → ¬ 2×ℕ+1 (m + n)
even+even≢odd zero   zero   ()
even+even≢odd zero   (2+ n) (2+ m+n) = even+even≢odd zero n m+n
even+even≢odd (2+ m) n      (2+ m+n) = even+even≢odd m n m+n

-- 4.2.
-- 
-- even+even≢even : {m n : ℕ} → 2×ℕ m → 2×ℕ n → ¬ 2×ℕ (m + n)
-- even+even≢even zero zero ()
-- ^
-- rejected with the following message:
-- 
--   2×ℕ zero should be empty, but the following constructor patterns
--   are valid:
--     zero
--   when checking that the clause even+even≢even zero zero () has type
--   {m n : ℕ} → 2×ℕ m → 2×ℕ n → ¬ 2×ℕ (m + n)
-- 
```



## Coq


1.1., 1.2., 2.1. and 3.1.:


```coq
Inductive nat : Set :=
  | O : nat
  | S : nat -> nat.

Fixpoint plus (n m:nat) {struct n} : nat :=
  match n with
    | O => m
    | S p => S (p + m)
  end

where "n + m" := (plus n m) : nat_scope.


Inductive even : nat -> Set :=
  | even_O : even O
  | even_SSn : forall n:nat,
                even n -> even (S (S n)).


Theorem even_plus_even : forall n m:nat,
  even n -> even m -> even (n + m).
Proof.
  intros n m H H0.
  
  elim H.
  trivial.
  
  intros.
  simpl.
  
  case even_SSn.
  intros.
  apply even_SSn; assumption.
  
  assumption.
Qed.

```



## Haskell


Using [http://www.haskell.org/haskellwiki/GADT GADTs] and [http://www.haskell.org/haskellwiki/GHC/Type_families type families] it is possible to write a partial adaptation of the Agda version:


```haskell
{-# LANGUAGE TypeOperators, TypeFamilies, GADTs #-}

module PeanoArithmetic where

-- 1.1. Natural numbers.

data Z = Z
data S m = S m

-- 2.1. Addition.

infixl 6 :+
type family x :+ y
type instance Z :+ n = n
type instance S m :+ n = S (m :+ n)

-- 1.2. Even natural numbers.

data En :: * -> * where
  Ez :: En Z
  Es :: En m -> En (S (S m))

-- 1.3. Odd natural numbers.

data On :: * -> * where
  Oo :: On (S Z)
  Os :: On m -> On (S (S m))

-- 3.1. Sum of any two even numbers is even.

sum_of_even_is_even :: En m -> En n -> En (m :+ n)
sum_of_even_is_even Ez n = n
sum_of_even_is_even (Es m) n = Es $ sum_of_even_is_even m n

-- The identity type for natural numbers.

infix 4 :=
data (:=) m :: * -> * where
  Refl :: m := m

sym :: m := n -> n := m
sym Refl = Refl

trans :: m := n -> n := p -> m := p
trans Refl np = np

cong :: m := n -> S m := S n
cong Refl = Refl

-- 3.2. Associativity of addition (via propositional equality).

class AssocAdd m where
  proof :: m -> n -> p -> (m :+ n) :+ p := m :+ (n :+ p)

instance AssocAdd Z where
  proof Z _ _ = Refl

instance AssocAdd m => AssocAdd (S m) where
  proof (S m) n p = cong $ proof m n p

-- Induction, associativity of addition by induction, equational reasoning and
-- commutativity of addition is too tricky.

-- 3.4. Bad proof.

sum_of_even_is_odd :: En m -> En n -> On (m :+ n)
-- ^
-- Сan not be written totally:
-- 
sum_of_even_is_odd Ez Ez = undefined
-- ^
-- then, in GHCi:
-- 
--   *PeanoArithmetic> :t sum_of_even_is_odd Ez Ez
--   sum_of_even_is_odd Ez Ez :: On (Z :+ Z)
--   *PeanoArithmetic> :t undefined :: On (Z :+ Z)
--   undefined :: On (Z :+ Z) :: On Z
--   *PeanoArithmetic> :t sum_of_even_is_odd Ez Ez :: On Z
--   sum_of_even_is_odd Ez Ez :: On Z :: On Z
--   *PeanoArithmetic> :t Oo
--   Oo :: On (S Z)
--   *PeanoArithmetic> :t Os Oo
--   Os Oo :: On (S (S (S Z)))
--   *PeanoArithmetic> :t Os (Os Oo)
--   Os (Os Oo) :: On (S (S (S (S (S Z)))))
-- 
-- so that sum_of_even_is_odd Ez Ez :: On Z, but On Z is empty, it is impossible
-- to write such a proof.
-- 

-- Uninhabited type.

data Bot

-- Negation.

type Not a = a -> Bot

-- 4.1. Disproof.

sum_of_even_is_not_odd :: En m -> En n -> Not (On (m :+ n))
sum_of_even_is_not_odd Ez (Es n) (Os mn) = sum_of_even_is_not_odd Ez n mn
sum_of_even_is_not_odd (Es m) n (Os mn) = sum_of_even_is_not_odd m n mn
sum_of_even_is_not_odd Ez Ez _ =
  error "impossible happened in sum_of_even_is_not_odd!"
-- ^
-- partial, however, we know that Ez :: En Z, Z + Z = Z and On Z is
-- uninhabited, so that this clause is unreachable.
--
-- Also, GHC complains:
--
--   Warning: Pattern match(es) are non-exhaustive
--           In an equation for `sum_of_even_is_not_odd':
--               Patterns not matched:
--                   Ez (Es _) Oo
--                   (Es _) _ Oo
--
-- and can't find that this clauses is unreachable too, since this isn't type check:
-- 
--   sum_of_even_is_not_odd Ez (Es _) Oo = undefined
--   sum_of_even_is_not_odd (Es _) _ Oo = undefined
--

-- 4.2. Bad disproof.

sum_of_even_is_not_even :: En m -> En n -> Not (En (m :+ n))
-- 
-- Starting from a partial definition:
-- 
sum_of_even_is_not_even Ez Ez _ = undefined
-- 
-- we can show that it can not be rewritten totally:
-- 
--   *PeanoArithmetic> :t sum_of_even_is_not_even Ez Ez
--   sum_of_even_is_not_even Ez Ez :: Not (En (Z :+ Z))
--   *PeanoArithmetic> :t sum_of_even_is_not_even Ez Ez :: Not (En Z)
--   sum_of_even_is_not_even Ez Ez :: Not (En Z) :: Not (En Z)
--   *PeanoArithmetic> :t sum_of_even_is_not_even Ez Ez :: En Z -> Bot
--   sum_of_even_is_not_even Ez Ez :: En Z -> Bot :: En Z -> Bot
--   *PeanoArithmetic> :t Ez
--   Ez :: En Z
--   *PeanoArithmetic> :t (sum_of_even_is_not_even Ez Ez :: En Z -> Bot) Ez
--   (sum_of_even_is_not_even Ez Ez :: En Z -> Bot) Ez :: Bot
-- 
-- since we have a "citizen" of an uninhabited type here (contradiction!).
-- 
```


See also [[Proof/Haskell]] for implementation of a small theorem prover.



## Idris



Idris supports two types of proofs:   using tactics, like Coq, or just write functions like Agda.
These ways can be combined.


```Idris

module Proof

-- Enable terminator checker by default
%default total

-- 1.1 Natural numbers
%elim
data MyNat 
  = Z 
  | S MyNat


-- 1.2 Even naturals
%elim
data EvNat : MyNat -> Type where
  EvO  : EvNat Z
  EvSS : EvNat x -> EvNat (S (S x))


-- 1.3 Odd naturals
%elim
data OddNat : MyNat -> Type where
  Odd1  : OddNat (S Z)
  OddSS : OddNat x -> OddNat (S (S x))


-- 2.1 addition

infixl 4 :+ 

(:+) : MyNat -> MyNat -> MyNat
(:+) Z b = b
(:+) (S a) b = S (a :+ b)
  

-- 3.1, Prove that the addition of any two even numbers is even. 


evensPlus1 : {a : MyNat} -> {b : MyNat} -> (EvNat a) -> (EvNat b) -> (EvNat (a :+ b))
evensPlus1 ea eb = ?proof31

congS : {a : MyNat} ->  {b : MyNat} -> (a = b) -> (S a = S b)
congS refl = refl

evensPlus2 : {a : MyNat} -> {b : MyNat} -> (EvNat a) ->(EvNat b) -> (EvNat (a :+ b))
evensPlus2 EvO eb = eb
evensPlus2 {a=(S (S a))} (EvSS ea) eb = EvSS (evensPlus2 ea eb)

-- 3.2  Prove that the addition is always associative. 
plusAssoc : (a : MyNat) -> (b : MyNat) -> (c : MyNat) -> (a :+ b) :+ c = a :+ (b :+ c)
plusAssoc Z b c = refl
plusAssoc (S a) b c = congS (plusAssoc a b c)


-- 3.3 Prove that the addition is always commutative. 

plus0_r : (a : MyNat) -> a :+ Z = a
plus0_r Z = refl
plus0_r (S a) = congS (plus0_r a)

plusS_r : (a : MyNat) -> (b : MyNat) -> a :+ S b = S (a :+ b)
plusS_r Z b = refl
plusS_r (S a) b = congS (plusS_r a b)

plusComm : (a : MyNat) -> (b : MyNat) -> a :+ b = b :+ a
plusComm a b = ?proof33


-- 4.1 Prove that the addition of any two even numbers cannot be odd. 

evenNotOdd : (ea : EvNat a) -> (oa : OddNat a) -> _|_ 
evenNotOdd (EvSS e) (OddSS o) = evenNotOdd e o

evensPlusNotOdd : (ea : EvNat a) -> (eb : EvNat b) -> (OddNat (a :+ b)) -> _|_
evensPlusNotOdd EvO (EvSS eb) (OddSS ob) = evenNotOdd eb ob
evensPlusNotOdd (EvSS y) EvO oab = ?epno_so
evensPlusNotOdd (EvSS y) (EvSS z) oab = ?epno_ss



---------- Proofs ----------



Proof.proof31 = proof
  intro a
  intro b
  intro ea
  intro eb
  induction ea
  compute
  exact eb
  intro x
  intro ex
  intro exh
  exact (EvSS exh)


Proof.proof33 = proof
  intros
  induction a
  compute
  rewrite sym (plus0_r b)
  trivial
  intro a'
  intro ha'
  compute
  rewrite sym (plusS_r b a')
  exact (congS ha')


Proof.epno_ss = proof
  intro x
  intro ex
  intro y
  intro ey
  compute
  rewrite sym ( plusS_r x (S y))
  rewrite sym ( plusS_r x y)
  intro os4xy
  let es4xy = EvSS(EvSS (evensPlus1 ex ey))
  exact evenNotOdd es4xy os4xy


Proof.epno_so = proof
  intro x
  intro ex
  rewrite sym (plus0_r x)
  compute
  rewrite sym (plus0_r x)
  intro ossx
  exact evenNotOdd (EvSS ex) ossx


```



## J


A Peano number needs a zero, a mechanism for distinguishing equality from its absence, a way of getting a successor to a number, and a system of induction.

We know that a computer can never fully implement peano numbers because a computer can only represent a finite number of distinct values (if necessary, Ackerman's function can be used to illustrate the existence of this limitation).  So our implementation of Peano Numbers will represent these mechanisms rather than be a complete implementation of these mechanisms.

So, these can be our definitions:


```J
context=:3 :0
  if. 0 = L. y do. context (,: ; ]) y return. end.
    kernel=. > {: y
    symbols=. (#$kernel) {. > {. y
    order=. /: symbols
    (symbols /: order); order |: kernel
)
symbols=: >@{.
kernel=: >@{:

zero=: context0x
  
monadic=: (1 :'[:context u@symbols; u&kernel')( :[:)
dyadic=:  (1 :'[:context ,&symbols; u&kernel')([: :)
successor=: +&1x monadic
equals=: -:&kernel
all=: i. >. %: kernel successor@$: ::] zero
is_member_of=: e.&(-. -.&all)&,&kernel ([: :)
exists_in=: 1&e.@e.&kernel ([: :)
not=: -. :[:
induction=:4 :0
   3 :'(y)=:context ?~#all'&.>;:x
   assert (#all) > #;._1 LF,y
   assert 0 = # y -.&;: LF,defined,x
   assert 0!:3 y
)

addition=: +/ dyadic
isN=: ([ assert@is_member_of&(context all))
even=: [: context kernel@addition~"0@,@kernel :[: @isN
odd=: successor@even
 
defined=: '(zero not exists_in odd successor equals is_member_of addition even)'
```


Here, '''even''' is a function which, given a natural number, produces a corresponding even natural number.  '''odd''' is a similar function which gives us odd numbers.

And, '''induction''' is a verb where the left argument lists values which represent any natural number and the right argument represents an expression to be considered.  If induction succeeds, the expression is true for all natural numbers.

Meanwhile '''-.''' is J's implementation of "logical negation" -- a function which has been grafted onto boolean algebra as a convenience for people working with logical systems.  ("Grafted on" here means: [a] logical negation was not originally a part of the definition of boolean algebra, and [b] logical negation cannot be a valid part of an infinite set of systems that were valid boolean algebras before someone decided that logical negation should be a part of boolean algebra.)

Also '''is_member_of''' represents universal set membership, '''exists_in''' represents the existential quantifier.

Note that we do not have to prove that our definitions are correct -- they are our axioms that we use in our proof.

Thus, this is proof that 

# all natural numbers are even, and
# addition is associative, and
# addition is commutative, and
# the sum of two even numbers can never be odd.


```J
'A B C' induction 0 :0
  ((even A) addition (even B)) is_member_of (even C)
  ((A addition B) addition C) equals (A addition (B addition C))
  (A addition B) equals (B addition A)
  not ((even A) addition (even B)) exists_in (odd C)
)
```


Meanwhile, here is how the invalid proofs fail:


```J
   'A B C' induction '((even A) addition (even B)) is_member_of (odd C)'
|assertion failure: assert
```


and


```J
   'A B C' induction 'not ((even A) addition (even B)) is_member_of (even C)'
|assertion failure: assert
```



As an aside, note that peano numbers show us that numbers can represent recursive processes.


## OCaml

Using GADT, we can port the Coq version to OCaml.


```OCaml

type zero = Zero
type 'a succ = Succ

(* 1.1 *)
type _ nat =
  | Zero : zero nat
  | Succ : 'a nat -> 'a succ nat

(* 1.2 *)
type _ even =
  | Even_zero : zero even
  | Even_ss : 'a even -> 'a succ succ even

(* 2.1: define the addition relation *)
type (_, _, _) plus =
  | Plus_zero : ('a, zero, 'a) plus
  | Plus_succ : ('a, 'b, 'c) plus -> ('a, 'b succ, 'c succ) plus

(* 3.1 *)

(* Define the property: there exists a number 'sum that is the sum of 'a and 'b and is even. *)
type ('a, 'b) exists_plus_even = Exists_plus_even : ('a, 'b, 'sum) plus * 'sum even -> ('a, 'b) exists_plus_even

(* The proof that if a and b are even, there exists sum that is the sum of a and b that is even.
   This is a valid proof since it terminated, and it is total (no assert false, and the exhaustiveness
   test of pattern matching doesn't generate a warning) *)
let rec even_plus_even : type a b. a even -> b even -> (a, b) exists_plus_even =
  fun a_even b_even ->
    match b_even with
    | Even_zero ->
      Exists_plus_even (Plus_zero, a_even)
    | Even_ss b_even' ->
      let Exists_plus_even (plus, even) = even_plus_even a_even b_even' in
      Exists_plus_even (Plus_succ (Plus_succ plus), Even_ss even)

(* 3.3 *)

(* 0 + n = n *)
let rec plus_zero : type a. a nat -> (zero, a, a) plus = function
  | Zero -> Plus_zero
  | Succ a -> Plus_succ (plus_zero a)

(* a + b = n => (a + 1) + b = (n + 1) *)
let rec plus_succ_left : type a b sum. (a, b, sum) plus -> (a succ, b, sum succ) plus =
  function
  | Plus_zero -> Plus_zero
  | Plus_succ plus -> Plus_succ (plus_succ_left plus)

(* a + b = n => b + a = n *)
let rec plus_commutative : type a b sum. a nat -> (a, b, sum) plus -> (b, a, sum) plus =
  fun a plus ->
    match plus with
    | Plus_zero -> plus_zero a
    | Plus_succ plus' ->
      plus_succ_left (plus_commutative a plus')


```



## Omega


```omega
data Even :: Nat ~> *0 where
   EZ:: Even Z
   ES:: Even n -> Even (S (S n))

plus:: Nat ~> Nat ~> Nat
{plus Z m} = m
{plus (S n) m} = S {plus n m}

even_plus:: Even m -> Even n -> Even {plus m n}
even_plus EZ en = en
even_plus (ES em) en = ES (even_plus em en)

```



## Phix

Silly text-based version, just for fun.

Obviously 1.1/1.2/1.3 are (loosely) eumlated by the strings "int"/"even"/"odd", and addition by axioms[4].

Clearly 3.2 and 3.3 are not attempted, and I'm not sure which of 3.4/4.1/4.2 the last test is
closest to, or for that matter what the difference between them is supposed to be.

```Phix
constant axioms = {{"even+1","odd"},
                   {"even","2*int"},
                   {"2*int+2*int","2*(int+int)"},
                   {"(int+int)","int"},
                   {"2*int","even"},
                   {"odd","2*int+1"}}
                   
procedure proof(string a, b)
-- try to convert a into b using only the above axioms
    string w = a
    sequence seen = {}          -- (avoid infinite loops)
    while not find(w,seen) do
        seen = append(seen,w)
        ?w
        if w=b then exit end if
        integer hit = 0
        for i=1 to length(axioms) do
            string {r,s} = axioms[i]
            integer k = match(r,w)
            if k then
                w[k..k+length(r)-1] = s
                hit = i
                exit
            end if
        end for
        if hit=0 then exit end if
        puts(1,"== ")
    end while
    ?{a,b,iff(w=b?"true","false")}
end procedure

proof("even+even","even")
proof("even+1","odd")
--bad proofs:
proof("int","even")
proof("even+even","odd")
```

```txt

"even+even"
== "2*int+even"
== "2*int+2*int"
== "2*(int+int)"
== "2*int"
== "even"
{"even+even","even","true"}
"even+1"
== "odd"
{"even+1","odd","true"}
"int"
{"int","even","false"}
"even+even"
== "2*int+even"
== "2*int+2*int"
== "2*(int+int)"
== "2*int"
== "even"
== {"even+even","odd","false"}

```



## Racket


Via <code>#lang cur</code>. Currently (August 29th, 2019), I am using a development version, which could be installed by the following commands:


```txt

raco pkg install https://github.com/stchang/macrotypes.git?path=macrotypes-lib#cur
raco pkg install https://github.com/stchang/macrotypes.git?path=turnstile-lib#cur
raco pkg install https://github.com/wilbowma/cur.git?path=cur-lib#turnstile-core

```



```racket
#lang cur

(require rackunit
         cur/stdlib/equality
         cur/stdlib/sugar
         cur/ntac/base
         cur/ntac/standard
         cur/ntac/rewrite)

;; Task 1.1

(data nat : 0 Type
      [O : nat]
      [S : (-> nat nat)])

(define-syntax #%datum
  (syntax-parser
    [(_ . n:exact-nonnegative-integer)
     #:when (zero? (syntax-e #'n))
     #'O]
    [(_ . n:exact-nonnegative-integer)
     #`(S (#%datum . #,(- (syntax-e #'n) 1)))]))

(check-equal? (S (S (S (S O)))) 4)

;; Task 1.2

(data even : 0 (-> nat Type)
      [even-O : (even 0)]
      [even-SS : (forall [n : nat] (-> (even n) (even (S (S n)))))])

;; Task 1.3

(data odd : 0 (-> nat Type)
      [odd-O : (odd 1)]
      [odd-SS : (forall [n : nat] (-> (odd n) (odd (S (S n)))))])

;; Task 2.1

(define/rec/match + : nat [m : nat] -> nat
  [O => m]
  [(S n-1) => (S (+ n-1 m))])

(check-equal? (+ 2 3) 5)

;; Task 3.1

(define-theorem even-plus-even-is-even
  (∀ [n : nat] [m : nat] (-> (even n) (even m) (even (+ n m))))
  (by-intros n m Hn Hm)
  (by-induction Hn #:as [() (n* IHn)])

  ;; subgoal 1
  simpl
  by-assumption

  ;; subgoal 2
  (by-apply even-SS)
  by-assumption)

;; Task 3.2

(define-theorem addition-assoc
  (∀ [a : nat] [b : nat] [c : nat] (== nat (+ (+ a b) c) (+ a (+ b c))))

  (by-intros a b c)
  (by-induction a #:as [() (a-1 IHa)])

  ;; subgoal 1
  reflexivity

  ;; subgoal 2
  display-focus ; show how the context and goal are before rewrite
  (by-rewrite IHa)
  display-focus ; show how the context and goal are after rewrite
  reflexivity)
```


```txt

b : nat
c : nat
a-1 : nat
IHa : (== nat (+ (+ a-1 b) c) (+ a-1 (+ b c)))
--------------------------------
(== nat (S (+ (+ a-1 b) c)) (S (+ a-1 (+ b c))))

b : nat
c : nat
a-1 : nat
IHa : (== nat (+ (+ a-1 b) c) (+ a-1 (+ b c)))
--------------------------------
(== nat (S (+ a-1 (+ b c))) (S (+ a-1 (+ b c))))

```


The fact that there's no error indicates that all proofs are verified.


## Salmon

Note that the only current implementation of Salmon is an interpreter that ignores proofs and doesn't try to check them, but in the future when there is an implementation that checks proofs, it should be able to check the proof in this Salmon code.


```Salmon
pure function even(x) returns boolean ((x in [0...+oo)) && ((x % 2) == 0));
theorem(forall(x : even, y : even) ((x + y) in even))
proof
  {
    forall (x : even, y : even)
      {
      L1:
        x in even;
      L2:
        ((x in [0...+oo)) && ((x % 2) == 0)) because type_definition(L1);
      L3:
        ((x % 2) == 0) because simplification(L2);
      L4:
        y in even;
      L5:
        ((y in [0...+oo)) && ((y % 2) == 0)) because type_definition(L4);
      L6:
        ((y % 2) == 0) because simplification(L5);
      L7:
        (((x + y) % 2) == (x % 2) + (y % 2)); // axiom of % and +
      L8:
        (((x + y) % 2) == 0 + (y % 2)) because substitution(L3, L7);
      L9:
        (((x + y) % 2) == 0 + 0) because substitution(L6, L8);
      L10:
        (((x + y) % 2) == 0) because simplification(L9);
        (x + y) in even because type_definition(even, L10);
      };
  };
```



## Tcl

Using the <code>datatype</code> package from the [[Pattern Matching#Tcl|Pattern Matching]] task...

```tcl
package require datatype
datatype define Int = Zero | Succ val
datatype define EO = Even | Odd
 
proc evenOdd val {
    global environment
    datatype match $val {
	case Zero		-> { Even }
	case [Succ [Succ x]]	-> { evenOdd $x }
	case t		-> {
	    set term [list evenOdd $t]
	    if {[info exists environment($term)]} {
		return $environment($term)
	    } elseif {[info exists environment($t)]} {
		return [evenOdd $environment($t)]
	    } else {
		return $term
	    }
	}
    }
}
 
proc add {a b} {
    global environment
    datatype match $a {
	case Zero	-> { return $b }
	case [Succ x]	-> { Succ [add $x $b] }
	case t		-> {
	    datatype match $b {
		case Zero	-> { return $t }
		case [Succ x]	-> { Succ [add $t $x] }
		case t2		-> {
		    set term [list add $t $t2]
		    if {[info exists environment($term)]} {
			return $environment($term)
		    } elseif {[info exists environment($t)]} {
			return [add $environment($t) $t2]
		    } elseif {[info exists environment($t2)]} {
			return [add $t $environment($t2)]
		    } else {
			return $term
		    }
		}
	    }
	}
    }
}

puts "BASE CASE"
puts "evenOdd Zero = [evenOdd Zero]"
puts "evenOdd \[add Zero Zero\] = [evenOdd [add Zero Zero]]"

puts "\nITERATIVE CASE"
set environment([list evenOdd p]) Even
puts "if evenOdd p = Even..."
puts "\tevenOdd \[Succ \[Succ p\]\] = [evenOdd [Succ [Succ p]]]"
unset environment
puts "if evenOdd \[add p q\] = Even..."
set environment([list evenOdd [add p q]]) Even
foreach {a b} {
    p q
    {Succ {Succ p}} q
    p {Succ {Succ q}}
    {Succ {Succ p}} {Succ {Succ q}}
} {
    puts "\tevenOdd \[[list add $a $b]\] = [evenOdd [add $a $b]]"
}
```

Output:

```txt
BASE CASE
evenOdd Zero = Even
evenOdd [add Zero Zero] = Even

ITERATIVE CASE
if evenOdd p = Even...
	evenOdd [Succ [Succ p]] = Even
if evenOdd [add p q] = Even...
	evenOdd [add p q] = Even
	evenOdd [add {Succ {Succ p}} q] = Even
	evenOdd [add p {Succ {Succ q}}] = Even
	evenOdd [add {Succ {Succ p}} {Succ {Succ q}}] = Even
```

It is up to the caller to take the output of this program and interpret it as a proof.


## Twelf



```twelf
nat : type.
z   : nat.
s   : nat -> nat.


plus   : nat -> nat -> nat -> type.
plus-z : plus z N2 N2.
plus-s : plus (s N1) N2 (s N3)
          <- plus N1 N2 N3.


%% declare totality assertion
%mode plus +N1 +N2 -N3.
%worlds () (plus _ _ _).

%% check totality assertion
%total N1 (plus N1 _ _).



even   : nat -> type.
even-z : even z.
even-s : even (s (s N))
          <- even N.


sum-evens : even N1 -> even N2 -> plus N1 N2 N3 -> even N3 -> type.
%mode sum-evens +D1 +D2 +Dplus -D3.

sez : sum-evens 
       even-z 
       (DevenN2 : even N2)
       (plus-z : plus z N2 N2)
       DevenN2.

ses : sum-evens 
       ( (even-s DevenN1') : even (s (s N1')))
       (DevenN2 : even N2)
       ( (plus-s (plus-s Dplus)) : plus (s (s N1')) N2 (s (s N3')))
       (even-s DevenN3')
       <- sum-evens DevenN1' DevenN2 Dplus DevenN3'.

%worlds () (sum-evens _ _ _ _).
%total D (sum-evens D _ _ _).

```
