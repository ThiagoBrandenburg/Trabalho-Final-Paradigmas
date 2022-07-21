import Text.ParserCombinators.Parsec
import Debug.Trace


-- ALUNOS: ANDRÉ LUIS FRANCISCO JUNIOR
--         GILSON SOHN JUNIOR

data Type = TypeInt
          | TypeVar Name
          | TypeArrow Type Type
          deriving Show

type Name = String

type Unifier = [(Name, Type)]

parseType :: Parser Type     
parseType = try parseFun <|> parseCelula

parseCelula :: Parser Type     
parseCelula = parseInt <|> parseVar  <|> parseParenteses

parseInt :: Parser Type      
parseInt = do
    string "Int"
    return (TypeInt)

parseVar :: Parser Type      -- var = minuscula
parseVar = do
    start <- lower      -- primeira letra minuscula
    rest <- many letter -- próximas são letras quaisquer
    return (TypeVar (start:rest))

parseFun :: Parser Type      
parseFun = do
    esq <- parseCelula    -- função é uma célula seguida de uma flecha depois um tipo
    try (many (char ' ')) -- pula os espaços em branco
    string "->"
    try (many (char ' '))
    dir <- parseType
    return (TypeArrow esq dir)

parseParenteses :: Parser Type    -- parenteses: "(" type ")"
parseParenteses = do
    char '('
    tipo <- parseType
    char ')'
    return (tipo)

unificar :: Type -> Type -> Maybe Unifier

-- Regra REFL, Isso aqui diz que duas variáveis do mesmo tipo podem ser unificadas
unificar (TypeVar x) (TypeVar y) 
    | x == y = Just []

-- Regra INT, A unificação entre dois tipos inteiros é possível também
unificar (TypeInt) (TypeInt) = Just []

-- Regra LEFT, se uma variável à esquerda não aparece livre na direita então a unificação é possível

unificar (TypeVar x) tipoT =
    if occursCheck x tipoT then 
        Nothing
    else
        Just [(x,tipoT)]

-- Regra RIGHT, se uma variável a direita não aparece livre na esquerda então a unificação também é possível
unificar tipoT (TypeVar x) =
    if occursCheck x tipoT then
        Nothing
    else
        Just [(x,tipoT)]

-- Regra ARROW
-- se der pra unificar dois termos de uma função (rolando uma substituição) cujos resultados também são unificáveis entre si, 
-- então dá pra unificar as funções também (por favor funcione sr. Haskell)
unificar (TypeArrow esq1 dir1) (TypeArrow esq2 dir2) =
    case (unificar esq1 esq2) of
        Nothing -> Nothing
        Just unifier1 -> 
            case (unificar (subst unifier1 dir1) (subst unifier1 dir2)) of
                Nothing -> Nothing
                Just unifier2 -> Just (compoe unifier2 unifier1)

-- Caso base, nenhum das regras se aplica
unificar _ _ = Nothing

occursCheck :: Name -> Type -> Bool      -- verifica se uma variável aparece livre em um tipo
occursCheck x (TypeInt) = False
occursCheck x (TypeVar y) = x == y
occursCheck x (TypeArrow e d) = 
    (occursCheck x e) || (occursCheck x d)

compoe :: Unifier -> Unifier -> Unifier -- compõe unificações
compoe _ [] = [] -- unificação vazia composta com qualquer coisa é unificação vazia
compoe [] _ = []
compoe xs ys = 
    xs ++ aplicaSubstituicao xs ys -- regra de composição de substituições

aplicaSubstituicao :: Unifier -> Unifier -> Unifier
aplicaSubstituicao x y =
    let tupla (primeiro, segundo) = (primeiro, subst x segundo)
    in  fmap tupla y

subst :: Unifier -> Type -> Type         -- faz uma substituição
subst xs TypeInt = TypeInt
subst xs (TypeVar nome) =     -- Isso deveria funcionar (?)
    case lookup nome xs of
        Just outronome -> outronome
        Nothing -> TypeVar nome
subst xs (TypeArrow esq dir) = 
    TypeArrow (subst xs esq) (subst xs dir)

main :: IO ()
main = do
    putStrLn $ "Primeiro tipo:"
    input1 <- getLine

    case parse parseType "<stdin>" input1 of -- coisa estranha de input do Haskell
        Right x ->                           -- acho que isso funciona
            do
                putStrLn $ "Segundo tipo:"
                input2 <- getLine

                case parse parseType "<stdin>" input2 of
                    Right y ->
                        do 
                            putStrLn $ "Unificador mais Geral:"
                            case (unificar x y) of 
                                Just unificador -> print $ unificador
                                Nothing -> putStrLn $ "    Nao foi possivel unificar"
                    
                    _ -> putStrLn $ "Tipo Nao Reconhecido"
        _ -> putStrLn $ "Tipo Nao Reconhecido"
