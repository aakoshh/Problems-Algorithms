namespace Algorithms

open System.Collections.Generic;
open System.Collections.Concurrent;

module Common =

    let memoize f = 
        let d = new Dictionary<_,_>(HashIdentity.Structural)
        fun x -> 
            if d.ContainsKey(x) then
                d.[x]
            else
                let v = f x
                d.[x] <- v
                v

    let memoizeConcurrent f = 
        let d = new ConcurrentDictionary<_,Lazy<_>>(HashIdentity.Structural)
        fun x -> 
            d.GetOrAdd(x, fun x -> lazy f x).Value



    module Tests = 
        open NUnit.Framework

        [<Test>]
        let TestMemoize() = 
            let cnt = ref 0
            let fn = memoizeConcurrent (fun x ->
                        cnt := !cnt + 1
                        x)
            let a = fn 1
            let b = fn 1
            Assert.AreEqual(a,b)
            Assert.AreEqual(1, !cnt)
