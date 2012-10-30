namespace Problems99

module Arithmetic = 
    open Problems99.Utils
    // 31: Determine whether a given integer number is prime. 
    let isPrime n = 
        if n = 2 then 
            true
        elif n < 2 || n % 2 = 0 then
            false
        else
            let r = int(sqrt (float n))        
            let rec trydiv d = 
                if d > r then
                    true
                elif n % d = 0 then 
                    false
                else
                    trydiv (d+2)
            trydiv 3

    let isPrime' n =  
        let sqrtn n = int <| sqrt (float n)
        n > 1 && seq { 2 .. sqrtn n } |> Seq.exists(fun i -> n % i = 0) |> not

    assertEqual (List.map isPrime [1;2;3;4;5]) [false;true;true;false;true] "isPrime"
    assertEqual (List.map isPrime' [1;2;3;4;5]) [false;true;true;false;true] "isPrime'"


    //32: Determine the greatest common divisor of two positive integer numbers.      
    let rec gcd a b = 
        if b = 0 then a else gcd b (a % b)

    assertEqual (List.map (fun (x,y) -> gcd x y) [(3,7);(3,6);(36,63)]) [1;3;9] "gcd"


    // 33: Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.
    let coprime a b = 
        gcd a b = 1

    assertEqual (List.map (fun (x,y) -> coprime x y) [(6,14);(9,14)]) [false;true] "coprime"
            

    // 34: Calculate Euler's totient function phi(m). Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m. Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.
    let totient m = 
        seq {1..(m-1)} |> Seq.filter (coprime m) |> Seq.length

    assertEqual (totient 10) 4 "totient"    


    // 35: Determine the prime factors of a given positive integer. Construct a flat list containing the prime factors in ascending order. 
    let makeNext (s: seq<'a>) =             
        let items = s.GetEnumerator()
        (fun () -> ignore (items.MoveNext()); items.Current)

    let primes = Seq.initInfinite (fun i -> i) |> Seq.filter isPrime

    let primeFactors n = 
        let nextPrime = makeNext primes
        let rec loop n p accu = 
            if n = 1 then 
                accu |> List.rev
            elif n % p = 0 then 
                loop (n/p) p (p::accu)
            else 
                loop n (nextPrime()) accu
        loop n (nextPrime()) []

    assertEqual (primeFactors 315) [3;3;5;7] "primeFactors"  


    // 36: Determine the prime factors of a given positive integer. 
    let primeFactorsMult n = 
        n |> primeFactors //|> Lists.runLengthEncode |> List.map (fun (e,b) -> (b,e))
        |> Seq.countBy id |> List.ofSeq
        
    assertEqual (primeFactorsMult 315) [(3,2);(5,1);(7,1)] "primeFactorsMult"
    

    //37: Calculate Euler's totient function phi(m) (improved). See problem 34 for the definition of Euler's totient function. If the list of the prime factors of a number m is known in the form of problem 36 then the function phi(m) can be efficiently calculated as follows: Let ((p1 m1) (p2 m2) (p3 m3) ...) be the list of prime factors (and their multiplicities) of a given number m. Then phi(m) can be calculated with the following formula: 
    //phi(m) = (p1 - 1) * p1 ** (m1 - 1) + (p2 - 1) * p2 ** (m2 - 1) + (p3 - 1) * p3 ** (m3 - 1) + ...
    let totient2 = 
        primeFactorsMult 
        >> List.fold (fun accu (p,e) -> accu * (p-1) * (pown p (e-1))) 1

    assertEqual (totient2 10) 4 "totient2"

    //38: Compare the two methods of calculating Euler's totient function. 
    let time f x = 
        let timer = System.Diagnostics.Stopwatch()
        timer.Start()
        try f x finally
        printf "Took %d ms" timer.ElapsedMilliseconds

    //time totient 10090
    //time totient2 10090


    //39: A list of prime numbers. 
    let primesBetween a b = 
        primes 
        |> Seq.skipWhile ((>) a)
        |> Seq.takeWhile ((>=) b)
        |> List.ofSeq
        
    assertEqual (primesBetween 10 20) [11; 13; 17; 19] "primesBetween" 

    //40: Goldbach's conjecture. Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case. It has been numerically confirmed up to very large numbers (much larger than w e can go with our Prolog system). Write a predicate to find the two prime numbers that sum up to a given even integer. 
    let goldbachPrimes n = 
        let h = n / 2
        let rec loop i = 
            if isPrime i && isPrime (n-i) then 
                Some (i, n-i)
            elif i > h then
                None
            else
                loop (i+1)
        loop 2

    assertEqual (goldbachPrimes 28) (Some (5,23)) "goldbachPrimes"

    //41: Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition. 
    let printGoldbach a b = 
        [a .. b]
        |> List.filter (fun x -> x%2 = 0)
        |> List.iter (fun x -> 
                        match goldbachPrimes x with
                        | Some (p1,p2) -> printfn "%d (%d,%d)" x p1 p2
                        | _ -> ())