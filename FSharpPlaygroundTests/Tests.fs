namespace FSharpPlaygroundTests

open FSharpPlayground

open Microsoft.VisualStudio.TestTools.UnitTesting
open FsCheck

[<TestClass>]
type SorterTests() =
    [<TestMethod>]
    member __.bubbleSort_WhenGivenAListOfNumbers_WorksUsingMagicalGenerativeTesting() =
        let isSorted input = List.isEmpty input || List.exists2 (=) (Sorters.bubbleSortier input) (List.sort input)
        Check.QuickThrowOnFailure isSorted
