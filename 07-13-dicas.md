# Dicas para o trabalho final

Um possível arquivo de entrada, contendo expressões em Prolog, é fornecido abaixo:

    yellow(banana).

    likes(bob, apple).
    likes(bob, grape).

    likes(alice, apple).
    likes(alice, banana).

    likes(ronald, X) :- yellow(X), likes(alice, X).

    append(nil, M, M).

    append(cons(H, T), X, cons(H, R)) :- append(T, X, R).

O parser deve reconhecer o conteúdo acima como uma série de regras. Usando os tipos fornecidos no enunciado do trabalho, o código acima irá representar a seguinte estrutura de dados:

    database = [
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
                        [("append", [Var (0, "T"), Var (0, "X"), Var (0, "R")])])
    ]

O código acima pode ser usado para testes, ou caso o aluno decisa pular a implementação do parser.

O conteúdo mínimo do trabalho deve ser a implementação do **unificador**, capaz de dizer se dois predicados podem ou não unificar, implementados através das funções a seguir:

    unifyTerm :: Term -> Term -> Maybe Substitution
    unifyPredicate :: Predicate -> Predicate -> Maybe Substitution
    unifyBody :: [Term] -> [Term] -> Maybe Substitution

    occursCheck :: Term -> Name -> Bool

Lembrando que, diferente do código feito em sala, há a necessidade de se unificar TERMOS (pela função `unifyTerm`) e unificar LISTAS DE TERMO (pela função `unifyBody`). A terceira função (`unifyPredicate`) existe meramente como auxiliar.

Idealmente, o unificador pode ser testado da maneira:

    main = do
        -- likes(alice, X)
        let a = Predicate ("likes", [Atom "alice", Var (0, "X")])
        -- likes(Y, apple)
        let b = Predicate ("likes", [Var (0, "Y"), Atom "apple"])
        -- Conseguimos unificar a e b?
        print $ unifyTerm a b

O código acima deverá imprimir que, sim, os dois termos unificam:

    Just [((0,"X"),Atom "apple"),((0,"Y"),Atom "alice")]

Isto é, temos o unificador { X |-> apple, Y |-> alice } como resposta.
