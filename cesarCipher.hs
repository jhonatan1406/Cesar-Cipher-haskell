import Data.Char

-- Função para cifrar um caractere
shiftChar :: Int -> Char -> Char
shiftChar shift c
    | isAlpha c = chr $ base + mod (ord c - base + shift) 26
    | otherwise = c
  where base = if isUpper c then ord 'A' else ord 'a'

-- Função para criptografar uma mensagem
encrypt :: Int -> String -> String
encrypt shift = map (shiftChar shift)

-- Função para descriptografar uma mensagem
decrypt :: Int -> String -> String
decrypt shift = encrypt (-shift)

-- Função principal para interação com o usuário
main :: IO ()
main = do
    putStrLn "Escolha uma opção:"
    putStrLn "1 - Criptografar"
    putStrLn "2 - Descriptografar"
    opcao <- getLine
    putStrLn "Digite a mensagem:"
    mensagem <- getLine
    putStrLn "Digite a chave (deslocamento):"
    chaveStr <- getLine
    let chave = read chaveStr :: Int
    let resultado = case opcao of
                      "1" -> encrypt chave mensagem
                      "2" -> decrypt chave mensagem
                      _   -> "Opção inválida!"
    putStrLn "Resultado:"
    putStrLn resultado
