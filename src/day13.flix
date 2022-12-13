namespace Day13 {

    enum Message {
        case IntVal(Int32)
        case ListVal(List[Message])
    }

    def compare(a: Message, b: Message): Comparison = match (a, b) {
        case (IntVal(ai), IntVal(bi)) => Comparison.fromInt32(ai - bi)
        case (ListVal(Nil), ListVal(Nil)) => EqualTo
        case (ListVal(Nil), ListVal(_)) => LessThan
        case (ListVal(_), ListVal(Nil)) => GreaterThan
        case (ListVal(ah :: ar), ListVal(bh :: br)) => {
            let c = compare(ah, bh);
            if (c == EqualTo) compare(ListVal(ar), ListVal(br)) else c
        }
        case (ListVal(_), IntVal(_)) => compare(a, ListVal(b::Nil))
        case (IntVal(_), ListVal(_)) => compare(ListVal(a :: Nil), b)
    }

    instance Order[Message] {
            pub def compare(x: Message, y: Message): Comparison = compare(x,y)
        }


    pub def toString(x: Message): String = {
        match x {
            case IntVal(v) => Int32.toString(v)
            case ListVal(l) => "[${l |> List.map(toString) |> List.intersperse(",") |> List.foldLeft(String.concat, "")}]"
        }
    }

    instance ToString[Message] {
        pub def toString(x: Message): String = toString(x)
    }

    pub def eq(a: Message, b: Message): Bool = match (a, b) {
        case (IntVal(ai), IntVal(bi)) => ai == bi
        case (ListVal(Nil), ListVal(Nil)) => true
        case (ListVal(ah :: ar), ListVal(bh :: br)) => ah == bh and eq(ListVal(ar), ListVal(br))
        case _ => false
    }

    instance Eq[Message] {
        pub def eq(a: Message, b: Message): Bool = eq(a, b)
    }

    def parseList(line: List[Char]): Result[(List[Message], List[Char]), String] = match line {
        case ']' :: rest => Ok(Nil, rest)
        case _ => {
            use Result.flatMap;
            let* (mn, r) = parseMessage(line);
            let* (l , r1) = parseList(List.dropWhile(c -> c == ',', r));
            Ok(mn :: l, r1)
        }
    }

    def parseMessage(line: List[Char]): Result[(Message, List[Char]), String] = match line {
        case '[' :: rest => parseList(rest) |> Result.map(match (l,r) -> (ListVal(l), r))
        case _ => {
            List.takeWhile(Char.isDigit, line) |> List.map(Char.toString) |> List.foldLeft(String.concat, "") |> Int32.parse(10) |> Result.map(v -> (IntVal(v), List.dropWhile(Char.isDigit, line)))
        }
    }

    def parse(line: String): Result[Message, String] = {
        Result.map(fst, parseMessage(String.toList(line)))
    }

    def splitOn(x: a, list: List[a]): List[List[a]] with Eq[a] =
        List.foldLeft(match (cl, acc) -> h -> if (h == x) (Nil, List.reverse(cl) :: acc) else (h :: cl, acc), (Nil, Nil), list)
            |> match (cl, acc) -> List.reverse(cl) :: acc |> List.reverse

    def task1(lines: List[String]): Result[Int32, String] = {
        use Result.flatMap;
        let grouped : List[List[String]]= splitOn("", lines);
        let* parsed : List[List[Message]] = Result.traverse(Result.traverse(parse), grouped);
        parsed |> List.map(l -> match l {
            case a :: b :: Nil => compare(a, b) == LessThan
            case _ => false
        }) |> List.zipWithIndex |> List.filter(fst) |> List.map(snd) |> List.sum |> Ok
    }

    def task2(lines: List[String]): Result[Int32, String] \ IO = {
        use Result.flatMap;
        let* parsed : List[Message] = Result.traverse(parse, "[[2]]" :: "[[6]]" :: List.filter(l -> not (l == ""), lines));
        let sorted = List.sortWith(compare, parsed);
        List.forEach(e -> println(e), sorted);
        let* ix1 = List.indexOf(ListVal(ListVal(IntVal(2)::Nil)::Nil), sorted) |> Option.toOk("Couldn't find [[2]]");
        let* ix2 = List.indexOf(ListVal(ListVal(IntVal(6)::Nil)::Nil), sorted) |> Option.toOk("Couldn't find [[6]]");
        Ok((ix1 + 1) * (ix2 + 1))
    }

    pub def main(): Unit \ IO = {
        use Result.flatMap;
        let result = {
            let* lines = File.readLines("src/input13.txt");
            println(task1(lines));
            println(task2(lines));
            Ok()
        };
        match result {
            case Err(err) => println("Error: ${err}")
            case Ok(_) => ()
        }
    }
}
