-- Questo programma legge le linee e risponde se sono palindrome o no
main = interact respondePalindromes

respondePalindromes =
  unlines . map (\xs -> if isPalindrome xs then "palindrome" else "not palindrome") . lines
  where
    isPalindrome xs = xs == reverse xs