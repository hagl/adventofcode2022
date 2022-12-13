namespace Day13 {


    enum Message {
        case IntVal(Int32)
        case ListVal(List[Message])
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




    def task1(lines: List[String]): Option[Int32] = {
        Some(3)
    }

    pub def main(): Unit \ IO = {
        use Result.flatMap;
        let result = {
            // let* lines = File.readLines("src/input13.txt");
            // println(task1(lines));
            // println(task2(lines));
            println(parseMessage("[1,[2,[3,[4,[5,6,0]]]],8,9]"))
            // Ok ()
        };
        match result {
            case Err(err) => println(err)
            case Ok(()) => ()
        }
    }
}
