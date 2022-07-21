--Biblioteca padrao entrada e saida
import System.IO

--Computação imperativa apenas na monada IO!

main = do
    handle <- openFile "arquivo.txt" ReadMode
    --Ler conteudo de um arquivo
    --contents :: String
    contents <- hGetContents handle
    --Fechar o arquivo
    hClose handle
    --imprimir conteúdo
    print contents