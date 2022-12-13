namespace Day11 {

    // processed counts x items
    type alias State = (List[List[Int64]], List[Int64])

    pub def get(ix: Int32, default: a, list: List[a]): a =
        list |> List.drop(ix) |> List.head |> Option.getWithDefault(default)

    pub def update(ix: Int32,  u: a -> a, list: List[a]): List[a] =
        let init = List.take(ix, list);
        let rest = List.drop(ix, list);
        let newRest = rest |> List.head |> Option.map(u) |> Option.map(h -> h :: (List.drop(1, rest))) |> Option.getWithDefault(rest);
        List.append(init, newRest)


    def action(ix: Int32, op: Int64 -> Int64, divisor : Int64, trueTarget: Int32, falseTarget: Int32, state: State): State= {
        let (itemList, counts) = state;
        let items = get(ix, Nil, itemList);
        let newCounts = update(ix, c -> c + (List.length(items) |> Int32.toInt64), counts);
        let (l1, l2) = items |> List.map(op) |> List.map(i -> i / 3i64) |> List.partition(i -> i mod divisor == 0i64);
        let il0 = update(ix, _ -> Nil, itemList);
        let il1 = update(trueTarget, l -> List.append(l, l1), il0);
        let il2 = update(falseTarget, l -> List.append(l, l2), il1);
        (il2, newCounts)
    }

    def action2(ix: Int32, op: Int64 -> Int64, divisor : Int64, trueTarget: Int32, falseTarget: Int32, cd: Int64, state: State): State= {
        let (itemList, counts) = state;
        let items = get(ix, Nil, itemList);
        let newCounts = update(ix, c -> c + (List.length(items) |> Int32.toInt64), counts);
        let (l1, l2) = items |> List.map(op) |> List.map(i -> i mod cd) |> List.partition(i -> i mod divisor == 0i64);
        let il0 = update(ix, _ -> Nil, itemList);
        let il1 = update(trueTarget, l -> List.append(l, l1), il0);
        let il2 = update(falseTarget, l -> List.append(l, l2), il1);
        (il2, newCounts)
    }

    def itemsEx(): List[List[Int64]] = {
        ((79 :: 98 :: Nil) ::
        (54 :: 65 :: 75 :: 74 :: Nil) ::
        (79 :: 60 :: 97 :: Nil) ::
        (74 :: Nil) ::
        Nil) |> List.map(List.map(Int32.toInt64))
    }


    def task1Ex(): Unit \ IO = {
        let monkeys : List[State -> State] =
            action(0, o -> o * 19i64, 23i64, 2, 3) ::
            action(1, o -> o + 6i64, 19i64, 2, 0) ::
            action(2, o -> o * o, 13i64, 1, 3) ::
            action(3, o -> o + 3i64, 17i64, 0, 1) ::
            Nil;

        run(itemsEx(), monkeys, 20)
    }

    def task2Ex(): Unit \ IO = {
        let cd = (23 * 19 * 13 * 17)  |> Int32.toInt64;

        let monkeys : List[State -> State] =
            action2(0, o -> o * 19i64, 23i64, 2, 3, cd) ::
            action2(1, o -> o + 6i64, 19i64, 2, 0, cd) ::
            action2(2, o -> o * o, 13i64, 1, 3, cd) ::
            action2(3, o -> o + 3i64, 17i64, 0, 1, cd) ::
            Nil;

        run(itemsEx(), monkeys, 10000)
    }

    def items(): List[List[Int64]] = {
            ((54 :: 82 :: 90 :: 88 :: 86 :: 54 :: Nil) ::
            (91 :: 65 :: Nil) ::
            (62 :: 54 :: 57 :: 92 :: 83 :: 63 :: 63 :: Nil) ::
            (67 :: 72 :: 68 :: Nil) ::
            (68 :: 89 :: 90 :: 86 :: 84 :: 57 :: 72 :: 84 :: Nil) ::
            (79 :: 83 :: 64 :: 58 :: Nil) ::
            (96 :: 72 :: 89 :: 70 :: 88 :: Nil) ::
            (79 :: Nil) ::
            Nil) |> List.map(List.map(Int32.toInt64))
    }

    def task1(): Unit \ IO = {

        let monkeys : List[State -> State] =
            action(0, o -> o * 7i64, 11i64, 2, 6) ::
            action(1, o -> o * 13i64, 5i64, 7, 4) ::
            action(2, o -> o + 1i64, 7i64, 1, 7) ::
            action(3, o -> o * o, 2i64, 0, 6) ::
            action(4, o -> o + 7i64, 17i64, 3, 5) ::
            action(5, o -> o + 6i64, 13i64, 3, 0) ::
            action(6, o -> o + 4i64, 3i64, 1, 2) ::
            action(7, o -> o + 8i64, 19i64, 4, 5) ::
            Nil;

        run(items(), monkeys, 20)
    }

    def task2(): Unit \ IO = {

        let cd = (11 * 5 * 7 * 2 * 17 * 13 * 3 * 19) |> Int32.toInt64;

        let monkeys : List[State -> State] =
            action2(0, o -> o * 7i64, 11i64, 2, 6, cd) ::
            action2(1, o -> o * 13i64, 5i64, 7, 4, cd) ::
            action2(2, o -> o + 1i64, 7i64, 1, 7, cd) ::
            action2(3, o -> o * o, 2i64, 0, 6, cd) ::
            action2(4, o -> o + 7i64, 17i64, 3, 5, cd) ::
            action2(5, o -> o + 6i64, 13i64, 3, 0, cd) ::
            action2(6, o -> o + 4i64, 3i64, 1, 2, cd) ::
            action2(7, o -> o + 8i64, 19i64, 4, 5, cd) ::
            Nil;

        run(items(), monkeys, 10000)
    }

    def run(items : List[List[Int64]], monkeys : List[State -> State], iterations: Int32): Unit \ IO = {

        let init = (items, List.repeat(List.length(items), 0i64));
        let state =
            List.foldLeft(st -> _ix -> {
                List.foldLeft(s -> f -> {
                    let newState = f(s);
                    newState
                }, st, monkeys)
            }, init, List.range(0,iterations));
        let (_, counts) = state;
        let result = counts |> List.sortBy(a -> -a) |> List.take(2) |> l -> match l {
            case a :: b :: Nil => a * b
            case _ => -1i64
        };
        println(result)
    }

    pub def main(): Unit \ IO = {
        task1Ex();
        task1();
        task2Ex();
        task2();
        ()
    }

}
