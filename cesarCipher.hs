-- Breno Arthur Rotte Fernandes Oliveira - 20.1.8124
-- Jhonatan Figueiredo Almeida - 20.1.8164
-- Para rodar o programa, entre no GHCi e digite main

module Main where

import Data.Char (chr, ord, isUpper, isLower)
import System.IO (hFlush, stdout)

-- Função para deslocar uma única letra mantendo maiúsculas e minúsculas
shiftChar :: Int -> Char -> Char
shiftChar shift c
    | isUpper c = chr (newPos 'A')
    | isLower c = chr (newPos 'a')
    | otherwise = c
    where
        newPos base = ((ord c - ord base + shift) `mod` 26) + ord base

-- Função para criptografar uma string inteira
encrypt :: Int -> String -> String
encrypt shift = map (shiftChar shift)

-- Função para descriptografar uma string inteira
decrypt :: Int -> String -> String
decrypt shift = encrypt (-shift)

-- Função para normalizar texto removendo espaços extras
normalizeText :: String -> String
normalizeText = unwords . words  

main :: IO ()
main = do
    putStrLn "=== Cifra de César ==="
    loopMenu

loopMenu :: IO ()
loopMenu = do
    putStrLn "\nEscolha uma opção:"
    putStrLn "1 - Criptografar uma mensagem"
    putStrLn "2 - Descriptografar uma mensagem"
    putStrLn "3 - Sair"
    putStr "Opção: "
    hFlush stdout  -- Força a exibição do prompt antes da entrada do usuário
    option <- getLine
    case option of
        "1" -> process encrypt "Mensagem criptografada:"
        "2" -> process decrypt "Mensagem descriptografada:"
        "3" -> putStrLn "Saindo... Obrigado por usar a Cifra de César!"
        _   -> putStrLn "Opção inválida! Tente novamente." >> loopMenu

process :: (Int -> String -> String) -> String -> IO ()
process func label = do
    putStr "Digite a mensagem: "
    hFlush stdout
    message <- getLine
    putStr "Digite a chave (número inteiro): "
    hFlush stdout
    keyInput <- getLine
    case reads keyInput :: [(Int, String)] of
        [(key, "")] -> do
            let result = func key (normalizeText message)
            putStrLn $ label ++ " " ++ result
            loopMenu
        _ -> do
            putStrLn "Chave inválida! Digite um número inteiro."
            process func label
