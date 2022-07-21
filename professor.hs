import Text.ParserCombinators.Parsec

data Type = TypeInt
          | TypeVar Name
          | TypeArrow Type Type

--
-- Note que o Haskell nos introduziu três construtores:
--
-- TypeInt :: Type
-- TypeVar :: Name -> Type
-- TypeArrow :: Type -> Type -> Type
--

--
-- Representa o nome de variáveis; nesse caso, apenas
-- uma String é o suficiente!
--
type Name = String

--
-- Representa uma substituição; basicamente, temos uma lista
-- de tuplas entre nomes e termos, da seguinte forma:
--
--   [(a, t), (b, u)] representa { a |-> t, b |-> u }
--
type Unifier = [(Name, Type)]

--
-- O parser int deve ler o texto Int da entrada e imediatamente
-- retornar um TypeInt!
--
--   int: "Int"
--
parseInt :: Parser Type
parseInt = do
    string "Int"
    return TypeInt

--
-- O parser var irá ler uma sequência de um ou mais caracteres
-- minúsculos, e, em seguida, retornar um TypeVar com esses mesmos
-- caracteres como o seu nome.
--
--   var: lowercase+
--
parseVar :: Parser Type
parseVar = do
    -- O combinador many1 recebe um parser e retorna uma
    -- sequência de uma ou mais entradas para aquele parser!
    -- Usaremos ele com o parser lower, que retorna um único
    -- caracter em letra minúscula!
    --
    -- many1 :: Parser a -> Parser [a]
    -- lower :: Parser Char
    -- many1 lower :: Parser String
    name <- many1 lower
    -- name :: String
    return (TypeVar name)

--
-- Vamos criar uma regra atom que irá ler um valor atômico,
-- isto é, um valor que serve de base para a construção de
-- tipos maiores. Temos três opções: um Int, uma variável, ou
-- um tipo entre parênteses!
--
--   atom: int | var | paren
--
parseAtom :: Parser Type
parseAtom = do
    parseInt <|> parseVar <|> parseParen

--
-- Uma expressão entre parênteses, paren, simplesmente lê
-- um parênteses abrindo, um tipo qualquer, e um parênteses
-- fechando!
--
--   paren: "(" type ")"
--
parseParen :: Parser Type
parseParen = do
    string "("
    whitespace
    t <- parseType
    whitespace
    string ")"
    -- Note que o resultado desse parser é o resultado da
    -- chamada acima! Só precisamos ler o parênteses após
    -- ela!
    return t

--
-- A regra gramatical type me deixa ler um tipo qualquer,
-- sendo ele um tipo atômico, básico, ou um tipo composto,
-- que no nosso caso é representado apenas pela seta!
--
--   type: function | atom
--
parseType :: Parser Type
parseType = do
    -- Usamos a função try :: Parser a -> Parser a para dizer,
    -- queremos TENTAR um parser de função, e, se der errado,
    -- ignora o trabalho feito e tenta a próxima regra
    (try parseFun) <|> parseAtom

--
-- Finalmente, precisamos de uma regra gramatical para
-- representar funções.
--
--   fun: atom "->" type
--
parseFun :: Parser Type
parseFun = do
    a <- parseAtom
    whitespace
    string "->"
    whitespace
    t <- parseType
    return (TypeArrow a t)

--
-- Uma pequena regra auxiliar para ignorar espaços em branco!
--
--  whitespace: " "*
--
whitespace :: Parser ()
whitespace = do
    -- O combinador many lê zero ou mais de algo!
    many (string " ")
    return ()

--
-- Entrada principal: queremos um type e acabou, queremos
-- que o texto termine após isso!!!
--
--   input: type EOF
--
parseInput :: Parser Type
parseInput = do
    whitespace
    t <- parseType
    whitespace
    many (string "\n")
    eof
    return t

test :: Unifier
test =
    [("a", TypeInt), ("b", TypeArrow TypeInt TypeInt)]

main :: IO ()
main = do
    -- print $ TypeArrow (TypeArrow TypeInt (TypeVar "a")) TypeInt
    -- print $ TypeArrow TypeInt (TypeArrow (TypeVar "a") TypeInt)
    putStrLn "Digite o primeiro tipo:"
    str1 <- getLine
    putStrLn "Digite o segundo tipo:"
    str2 <- getLine
    case (parse parseInput "<stdin>" str1, parse parseInput "<stdin>" str2) of
        (Right t1, Right t2) ->
            print $ unify t1 t2
        (Left err, _) -> do
            print "Error!"
            print err
        (_, Left err) -> do
            print "Error!"
            print err

--
-- Vamos criar uma instância Show para o tipo Type que acabamos
-- de criar, para informar ao programa como transformar ele em
-- uma String para mostrar na tela! Cuidado com os parênteses!
--
instance Show Type where
    show TypeInt =
        "Int"
    show (TypeVar x) =
        x
    show (TypeArrow s@(TypeArrow _ _) t) =
        -- Sabemos que s é um TypeArrow!
        "(" ++ show s ++ ") -> " ++ show t
    show (TypeArrow s t) =
        show s ++ " -> " ++ show t

--
-- SUBSTITUIÇÃO: recebemos uma substituição s e um tipo
-- t, e queremos retornar a substituição de s em t, isto
-- é, o tipo s(t)
--
subst :: Unifier -> Type -> Type

--
-- Substituição em uma variávei irá verificar se a substituição
-- nos instrui a trocar o conteúdo da variável, sendo definida
-- da seguinte forma:
--
--   s(a) = t         se s contém a |-> t,
--          a         do contrário
--
-- Logo, temos que, por exemplo:
--   { a |-> Int, b |-> Int -> Int }(a) = Int
--   { a |-> Int, b |-> Int -> Int }(c) = c
--
subst s (TypeVar a) =
    -- Na biblioteca padrão do Haskell, existe a função lookup,
    -- que possui o seguinte tipo:
    --
    --   lookup :: Eq a => a -> [(a, b)] -> Maybe b
    --
    -- Então podemos procurar se o nome a é usado na substituição!
    case lookup a s of
        -- A variável a está associada ao tipo t!
        Just t ->
            t
        -- A variável a não está associada a nenhum tipo!
        Nothing ->
            -- Como precisamos retornar um tipo, usamos o construtor
            -- TypeVar para isso, como o que recebemos!
            TypeVar a

--
-- O tipo Int faz parte da estrutura e, portanto, nunca pode ser
-- alterado; com isso, temos que:
--
--   s(Int) = Int
--
subst s (TypeInt) =
    TypeInt

--
-- Finalmente, precisamos verificar tipos de função, precisamos
-- nos lembrar que substituição é um homomorfismo, e preserva
-- a estrutura do conjunto; nesse caso, aplicação em função
-- simplesmente distribui sobre o mesmo:
--
--   s(t1 -> t2) = s(t1) -> s(t2)
--
subst s (TypeArrow t1 t2) =
    -- Distribui a chamada aos subtipos
    TypeArrow (subst s t1) (subst s t2)

--
-- A funlçao occursCheck é usada para verificar se uma
-- variável aparece, de forma livre, em um tipo; devemos
-- notar que, na nossa gramática simplificada de tipos,
-- todos os usos de uma variável são livres.
--
occursCheck :: Name -> Type -> Bool

--
-- Verificamos, por exemplo, se a variável a existe dentro
-- de um tipo que é apenas o tipo Int. Nesse caso, sabemos
-- imediatamente que o nome não aparece lá.
--
occursCheck a (TypeInt) =
    False

--
-- Ao procurarmos um nome a, tendo recebido uma variável b,
-- podemos verificar apenas se a é igual a b. Se forem iguais,
-- o nome aparece livre no tipo. Se forem diferentes, não
-- pode aparecer: o tipo é composto apenas de b!
--
occursCheck a (TypeVar b) =
    a == b

--
-- Queremos verificar se a variável a aparece num tipo t1 -> t2;
-- para isso, basta verificarmos se a variável a existe em
-- qualquer um dos subelementos, isto é, nesse nosso caso, em
-- t1 ou em t2
--
occursCheck a (TypeArrow t1 t2) =
    occursCheck a t1 || occursCheck a t2

--
-- A função unify irá, dados dois tipos, tentar encontrar uma
-- substituição que os faça ficarem iguais.
--
unify :: Type -> Type -> Maybe Unifier

--
-- A regra (REFL) nos diz que, se recebermos duas variáveis
-- iguais, podemos unificá-las, retornando um unificador
-- vazio.
--
--   --------------- (REFL)
--      a ~ a = {}
--
unify (TypeVar a) (TypeVar b) | a == b =
    -- TEMOS um resultado, então retornamos o resultado com Just,
    -- e o unificador é vazio; como unificadores são LISTAS de
    -- substituições, e não há uma substituição, retornamos uma
    -- lista vazia apenas. :)
    Just []

--
-- A regra (LEFT) diz que uma variável na esquerda unifica corretamente
-- com um tipo na direita, trocando a variável para esse tipo, ao longo
-- que a variável não apareça também nesse tipo.
--
--    a não aparece livre em t
--   --------------------------- (LEFT)
--       a ~ t = { a |-> t }
--
unify (TypeVar a) t | not (occursCheck a t) =
    -- Temos uma solução, e por isso retornamos um unificador usando o
    -- Just; o unificador diz que devemos trocar a variável a pelo tipo t
    Just [(a, t)]

--
-- De forma análoga, temos a regra (RIGHT) que diz que uma variável na
-- direita unifica com um tipo qualquer na esquerda, contanto que a variável
-- não apareça nesse tipo.
--
--    a não aparece livre em t
--   --------------------------- (RIGHT)
--       t ~ a = { a |-> t }
--
unify t (TypeVar a) | not (occursCheck a t) =
    Just [(a, t)]

--
-- Nesse sistema de tipos, usamos o tipo Int como nossa única constante,
-- que faz parte da estrutura e portanto não deve ser alterada; porém,
-- se já tivermos duas constantes iguais em ambos os lados, eles unificam
-- sem alterar nada
--
--   --------------------- (INT)
--        Int ~ Int = {}
--
unify (TypeInt) (TypeInt) =
    Just []

--
-- Finalmente, a regra (ARROW) unifica dois tipos de função, precisando
-- de duas hipóteses como verdadeiras para que isso aconteça.
--
--     t1 ~ t2 = s1      s1(r1) ~ s1(r2) = s2
--   ------------------------------------------ (ARROW)
--          t1 -> r1 ~ t2 -> r2 = s2 * s1
--
{-
unify (TypeArrow t1 r1) (TypeArrow t2 r2) =
    -- A primeira hipótese é verdadeira?
    case unify t1 t2 of
        -- Unificam!
        Just s1 ->
            -- Verificamos a segunda hipótese
            case unify (subst s1 r1) (subst s1 r2) of
                -- Unificam!
                Just s2 ->
                    -- Opa, temos s1 e s2, então as setas unificam!
                    Just (compose s2 s1)
                -- Não unificam!
                Nothing ->
                    -- Então as setas não unificam!
                    Nothing
        -- Não unificam!
        Nothing ->
            -- Portanto, as setas também não unificam!
            Nothing
-}
unify (TypeArrow t1 r1) (TypeArrow t2 r2) = do
    -- Mesmo código que acima, mas em forma monádica!
    s1 <- unify t1 t2
    s2 <- unify (subst s1 r1) (subst s1 r2)
    return (compose s2 s1)

--
-- Se não caiu em nenhuma das regra acima é porque não podemos
-- unificar os dois tipos.
--
unify _ _ =
    Nothing

--
-- O objetivo da função compose é unir duas substituições, que junta as
-- da esquerda com todas da direita tendo a da esquerda aplicada a cada
-- pares.
--
-- compose [(x1, e1), ..., (xn, en)] [(y1, f1), ..., (ym, fm)] =
--        [(x1, e1),
--          ...,                    <- Copia do unificador da esquerda
--          (xn, en),
--          (y1, subst [(x1, e1), ..., (xn, en)] f1),
--          ...                     <- Copia do unificador da direita, aplicando
--          (y1, subst [(x1, e1), ..., (xn, en)] f1)   o da esquerda em cada item!
--        ]
--
compose :: Unifier -> Unifier -> Unifier
compose s1 s2 =
    s1 ++ fmap (\(x, t) -> (x, subst s1 t)) s2
    --          ^^^^^^^^^^^^^^^^^^^^^^^^^^
    -- where
    --     troca (x, t) =
    --         (x, subst s1 t)