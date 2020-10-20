module FSharpPlayground.Sorters

  // First pass at writing, I didn't use an accumulator which suggests I'm still thinking imperatively
  let bubbleSort (list: list<int>) =
    let rec sort (list: list<int>) (currentIndex: int) (possiblyFinished: bool) =
      let startOfList = currentIndex = 0
      let endOfList = currentIndex = max ((List.length list) - 1) 0

      let currentItem = if List.isEmpty list
                        then 0
                        else List.item currentIndex list

      let toCompare = if endOfList
                      then 0
                      else List.item (currentIndex + 1) list

      let nextIndex = if endOfList
                      then 0
                      else currentIndex + 1

      let actuallyFinished = endOfList && possiblyFinished
      let needToSwap = currentItem > toCompare

      match list with
      | [] -> list
      | [_]  -> list

      // If the sort finished without touching anything, we're done!
      | _ when actuallyFinished -> list

      // If we're at the end of the list and the sort hasn't finished
      | _ when endOfList -> sort list nextIndex possiblyFinished

      // If we're at the start of the list, see if we need to swap and continue
      | _ when startOfList && needToSwap ->
          sort ([toCompare; currentItem] @ list.[(currentIndex + 2)..]) nextIndex false

      // If we're at the final swap, see if we need to swap anything
      | _ when currentIndex = ((List.length list) - 2) && needToSwap ->
          sort (list.[..(currentIndex - 1)] @ [toCompare; currentItem]) nextIndex false

      // For the rest of the list, do some swappin'
      | _ when needToSwap ->
          sort (list.[..(currentIndex - 1)] @ [toCompare; currentItem] @ list.[(currentIndex + 2)..]) nextIndex false

      // We don't need to swap
      | _ when startOfList -> sort list nextIndex true
      | _ -> sort list nextIndex possiblyFinished

    sort list 0 false


  // Written after doing some reading, an accumulator can replace the currentIndex state tracking.
  // We can then just check the first and second elements in the list (which is nice to do because of pattern matching using
  // the cons operator)
  let bubbleSortier (list: list<int>) =
    let rec sort (list: list<int>) (acc: list<int>) (possiblyFinished: bool) =
      match list with
      | [] when possiblyFinished -> acc
      | [] ->
          sort acc list true

      // Time to swap!
      | x::y::tail when x > y ->
          sort (x::tail) (acc @ [y]) false

      // Don't do a swap
      | x::tail ->
          sort (tail) (acc @ [x]) possiblyFinished

    sort list [] true
