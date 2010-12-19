postulate s :: Set
postulate e :: Set
postulate t :: Set
postulate ext :: (a::Set) |-> (s -> a) -> a
postulate int :: (a::Set) |-> a -> (s -> a)
postulate box :: t -> t
postulate id :: e -> e -> t
postulate and :: t -> t -> t
postulate or :: t -> t -> t
postulate imply :: t -> t -> t
postulate equiv :: t -> t -> t
postulate not :: t -> t
postulate forall :: (e -> t) -> t
postulate exists :: (e -> t) -> t
postulate H :: t -> t
postulate F :: t -> t

CN :: Set
CN = e -> t

IV :: Set
IV = e -> t

(/) :: Set -> Set -> Set
a / b = (s -> b) -> a

(//) :: Set -> Set -> Set
a // b = (s -> b) -> a

T   = t / IV
TV  = IV / T
IAV = IV / IV

Det = T / CN
DTV = TV / T
TTV = TV / T
PP  = IV / TV
Adj :: Set
Adj = e -> t

be :: TV
be p x = ext p (int (\(y::e) -> id x y))

necessarily :: t / t
necessarily p = box (ext p)

postulate j :: e
postulate m :: e
postulate b :: e
postulate n :: e

John :: T
John P = ext P j

Mary :: T
Mary P = ext P m

Bill :: T
Bill P = ext P b

ninty :: T
ninty P = ext P n

a :: Det
a p q = exists (\(x::e) -> and (ext p x) (ext q x))

the :: Det
the p q = exists (\(y::e) ->
                  forall (\(x::e) -> and (equiv (ext p x) (id x y)) (ext q x)))

every :: Det
every p q = forall (\(x::e) -> imply (ext p x) (ext q x))

no :: Det
no p q = forall (\(x::e) -> not (and (ext p x) (ext q x)))

be' :: IV / Adj
be' P x = ext P x

by :: PP / T
by P R x = ext P (int (\(y::e) -> ext R (int (\(Q::s -> e -> t) -> ext Q x)) y))

-- NG
by' :: PP / T
by' P R x = ext P (int (\(y::e) -> int R {! !} {! !} {! !} {! !}))
-- λP λR λx P{^ (λy [^R(y, ^(λP P{x}))])}

bar :: ((R::{! !}) -> (h::{! !}) -> {! !}) / T
bar = \(P::s->T) -> \(R::{! !}) -> \(x::e) ->
      ext P (int (\(y::e) -> {! !}))

{-
Q : s -> e -> x

int (\Q -> ext Q x)
-}

F25 :: TV -> Adj
F25 delta x = exists (\(y::e) -> H (delta (int (\(P::s->e->t) -> ext P x)) y))
