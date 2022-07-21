parseVariable :: Parser Term
parseVariable = do
        str <- 

--swish 
--usar freshen r in, 

--____PARSER____

parseLcAlphaNum :: Parser String
parseLcAlphaNum = do
        x <- lower
        xs <- many alphaNum
        return (x:xs)
parseUcAlphanum :: Parser String
parseUcAlphanum = do
        x <- upper
        xs <- many alphaNum
        return (x:xs)

parseWord :: Parser String
parseWord = do
        string "\""
        w <- many alphaNum
        string "\""
        return w

whitespace :: Parser ()
whitespace = do
    -- O combinador many lê zero ou mais de algo!
    many $ space <|> (char '\n') <|> tab
    return ()

--ATOMOS
parseAtom :: Parser Term
parseAtom = do
        whitespace
        string "Atom"
        whitespace
        char '"'
        str <- parseLcAlphaNum <|> many1 digit
        char '"'
        return (Atom str)
--VARIAVEIS
parseVariable :: Parser Term
parseVariable = do
        whitespace
        str <- parseUcAlphanum <|> many1 digit
        return (Var (0, str))

--Argumentos 
parseArguments :: Parser [Term]
parseArguments = do
        whitespace
        cabeca <- parseTerm
        cauda <- parseArgumentsTail
        return (cabeca:cauda)
parseArgumentsTail :: Parser [Term]
parseArgumentsTail =
        (char ',' >> parseArguments)
        <|> (return [])

--PREDICADO
parsePredicate :: Parser Term
parsePredicate = do
        string "("
        char '"'
        s <- parseLcAlphaNum
        char '"'
        string ","
        whitespace
        string "["
        arg <- parseArguments
        string "])"
        return (Predicate (s,arg))

--TERMOS = ATOMO OU VARIAVEL OU PREDICADO
parseTerm :: Parser Term
parseTerm = do
        parseAtom <|> parseVariable <|> parsePredicate


--Lista de Predicados para Rules
parsePredicateList :: Parser [Predicate]
parsePredicateList = do
        char '['
        whitespace
        l <- parsePredicateHead
        whitespace
        char ']'
        return l
parsePredicateHead :: Parser [Predicate]
parsePredicateHead = do
        whitespace
        (Predicate head) <- parsePredicate
        tail <- parsePredicateTail
        return (head:tail)
parsePredicateTail :: Parser [Predicate]
parsePredicateTail = 
        (char ',' >> parsePredicateHead)
        <|> (return [])
parsePredicateEmpty :: Parser [Predicate]
parsePredicateEmpty = do
        string "[]"
        return []

--REGRA
parseRule :: Parser Rule
parseRule = do
        char '('
        (Predicate cabecalho) <- parsePredicate
        string ","
        whitespace
        corpo <- parsePredicateEmpty <|> parsePredicateList
        whitespace
        char ')'
        return (cabecalho,corpo)

--REGRAS
parseRules :: Parser [Rule]
parseRules = do
        whitespace
        cabeca <- parseRule
        whitespace
        cauda <- parseRulesTail
        return (cabeca:cauda)
parseRulesTail :: Parser [Rule]
parseRulesTail =
        (char ',' >> parseRules)
        <|> (return [])

parseDatabase :: Parser [Rule]
parseDatabase = do
        whitespace
        string "database"
        whitespace
        string "="
        whitespace
        string "["
        whitespace
        r <- parseRules
        whitespace
        string "]"
        many (string "\n")
        eof
        return r

main = do
        leitor <- openFile "example.txt" ReadMode
        conteudo <- hGetContents leitor
        print $ parse parseDatabase "<stdin>" conteudo
        hClose leitor

