import Basics as B
import Debug as D
import List as L
dividesEvenly n t = n % t == 0
-- The simplest primality test is trial division: Given an input number n,
-- check whether any prime integer m from 2 to √n evenly divides n 
-- (the division leaves no remainder)
isPrime n =
  L.length (L.filter (dividesEvenly n) (L.range 2 (B.floor (B.sqrt (B.toFloat n))))) == 0

isPrimeDivisor n t = n % t == 0 && isPrime t

searchPrime n t =
  if n % t == 0 && isPrime t then searchPrime (n // D.log "Found: " t) (t+1)
  else if n % t == 0 then searchPrime (n//t) (t+1)
  else searchPrime n t+1