namespace Algorithms
open System

module Bits = 
    
    let toBinary (i: int) = 
        Convert.ToString(i, 2)

    let fromBinary (s: string) = 
        Convert.ToInt32(s, 2)

    /// replace the binary part of a[i..j] with b
    /// e.g. replace 110101001 6 4 010 = 110010001
    let replace a i j b = 
        // get the lower part of 110 101 001 -> 000 000 001
        let a'' = a &&& ((pown 2 (j-1))-1)
        // get rid of lower part and zero the one to be replaced: 110 101 001 -> 110 000
        let a' = (a >>> i) <<< (j-1);
        // combine it with b: 110 000 -> 110 010 
        let a' = a' ||| b
        // add back the lower part
        let a' = (a' <<< (i-j+1)) ||| a''
        a' 


    module Test = 
        open NUnit.Framework

        [<Test>]
        let TestReplace() = 
            let r = replace (fromBinary "110101001") 6 4 (fromBinary "010") |> toBinary
            let e = "110010001"
            Assert.AreEqual(e, r)



