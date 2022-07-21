import Text.ParserCombinators.Parsec
import System.IO

--Analise Lexica e Sintatica
data Term = Atom String
        | Var Name
        | Predicate Predicate
        deriving (Show, Eq)

type Name = (Int, String)
type Predicate = (String, [Term])
type Rule = (Predicate, [Predicate])
type Substituition = [(Name, Term)]

d1 = [
        (("yellow", [Atom "banana"]), []),
        (("likes", [Atom "bob", Atom "apple"]), []),
        (("likes", [Atom "bob", Atom "grape"]), []),
        (("likes", [Atom "alice", Atom "apple"]), []),
        (("likes", [Atom "alice", Atom "banana"]), []),
        (("likes", [Atom "ronald", Var (0,"X")]),
        [("yellow", [Var (0, "X")]), ("likes", [Atom "alice", Var (0, "X")])]),
        (("append", [Atom "nil", Var (0, "M"), Var (0, "M")]), []),
        (("append", [Predicate ("cons", [Var (0, "H"), Var (0, "T")]),
                Var (0, "X"),
                Predicate ("cons", [Var (0, "H"), Var (0, "R")])]),
                [("append", [Var (0, "T"), Var (0, "X"), Var (0, "R")])])]

--(("likes", [Atom "ronald", Var (0,"X")]), [("yellow", [Var (0, "X")]), ("likes", [Atom "alice", Var (0, "X")])])
p1 = ("likes",[Atom "alice",Var (0,"Z")])


not_main = do
        print d1

occursCheck :: Term -> Name -> Bool
occursCheck (Atom a) b =
        False
occursCheck (Var a) b =
        a == b
occursCheck (Predicate (s,l)) b =
        any ((flip occursCheck) b) l

subst :: Substituition -> Term -> Term
subst s (Var a) =
        case lookup a s of
                Just t -> t
                Nothing -> Var a
subst s (Atom a) =
        Atom a
subst s (Predicate (st,l)) = Predicate (st,map (subst s) l)

--PREDICADOS DE NOMES DIFERENTES AINDA UNIFICAM COM MESMOS ARGUMENTOS
unifyTerm :: Term -> Term -> Maybe Substituition
unifyTerm (Var a) (Var b) | a==b = Just []      --REFL
unifyTerm (Atom a) (Atom b) | a==b = Just []    --ATOM
unifyTerm (Var a) t | not (occursCheck t a) = Just [(a,t)]      --LEFT
unifyTerm t (Var a) | not (occursCheck t a) = Just [(a,t)]      --RIGHT
unifyTerm (Predicate (s1,l1)) (Predicate (s2,l2)) =     --PRED
        if s1==s2 then unifyPredicate (Predicate (s1,l1)) (Predicate (s2,l2))
        else Nothing
unifyTerm _ _ = Nothing

--unify predicate
unifyPredicate :: Term -> Term ->  Maybe Substituition
unifyPredicate (Predicate (s1,(l1:[]))) (Predicate (s2, (l2:[]))) = do
        u1 <- unifyTerm l1 l2
        return u1
unifyPredicate (Predicate (s1,(l1:ls1))) (Predicate (s2,(l2:ls2))) = do
        u1 <- unifyTerm l1 l2
        u2 <- unifyPredicate (Predicate (s1, map (subst u1) ls1)) (Predicate (s2, map (subst u1) ls2))
        return (compose u2 u1)


compose :: Substituition -> Substituition -> Substituition
compose s1 s2 =
        s1 ++ fmap (\(x,t) -> (x,subst s1 t)) s2
        

listTermToPredicate :: [Term] -> [Predicate]
listTermToPredicate [] = []
listTermToPredicate (Predicate x:xs) = x: listTermToPredicate xs

listPredicateToTerm :: [Predicate] -> [Term]
listPredicateToTerm [] = []
listPredicateToTerm (a:xs) = (Predicate a): listPredicateToTerm xs

freshen :: Rule -> Rule
freshen (rhead,rtail) = 
        let
                Predicate termrhead = aux $ Predicate rhead
                termrtail = map aux $ listPredicateToTerm rtail
                aux :: Term -> Term
                aux (Atom a) = Atom a
                aux (Var (num,x)) = (Var (num+1,x))
                aux (Predicate (s,l)) = (Predicate (s,map aux l))
        in (termrhead, listTermToPredicate termrtail)


resolve :: Predicate -> [Rule] -> [Substituition]
resolve g r =
        let 
                r' = map freshen r
                l = map (aux r' g) r'
                c = concat $ concat l
        in
                return c
        where
                aux :: [Rule] -> Predicate -> Rule -> [Substituition]
                aux r' g (head,body) = 
                        let u1 = (unifyTerm (Predicate g) (Predicate head)) in
                        if u1 == Nothing then []
                        else let Just uni = u1 in resolveBody uni r' body


resolveBody :: Substituition -> [Rule] -> [Predicate] -> [Substituition]
resolveBody s1 _ [] = [s1]
resolveBody s1 r (p:ps) =
        let
                r' = map freshen r
                Predicate s1' = subst s1 (Predicate p)
                sub = resolve s1' r'
                sub' =  map (compose s1) sub    --sub == [s2], s2*s1 = map composte sub s1
        in
                concat $ map (organizedResolveBody ps r') sub'
        where
                organizedResolveBody :: [Predicate] -> [Rule] -> Substituition -> [Substituition]
                organizedResolveBody p r s = resolveBody s r p


{-


unifyPredicate :: Predicate -> Predicate -> Maybe Substitution
unifyBody :: [Term] -> [Term] -> Maybe Substitution
-}


