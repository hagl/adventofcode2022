namespace Day5 {

    pub def parseMove(line: String): Option[(Int32, Int32, Int32)] = {
        match List.map(Int32.parse(10), String.split({regex="move | from | to "}, line)) {
            case _ :: Ok(c) :: Ok(f) :: Ok(t) :: Nil => Some ((c,f,t))
            case _ => None
        }
    }

   def task1(lines: List[String], reverse: Bool): String = {
        let splitIndex = Option.getWithDefault(0, List.indexOf("", lines));
        let moves = List.drop(splitIndex, lines)
            |> List.filterMap(parseMove);
        let initial = List.take(splitIndex - 1, lines);
        let count = (String.length(Option.getWithDefault("", List.last(initial))) + 2) / 4;
        let empty : List[List[Char]]= List.repeat(count, Nil);
        let stacks = List.foldLeft(s -> line -> {
            List.map(t -> {
                let (cs,i) = t;
                let ix = i*4+1;
                match if (ix < String.length(line)) String.charAt(ix, line) else ' ' {
                    case ' ' => cs
                    case c => cs ++ (c :: Nil)
                }
            }, List.zipWithIndex(s))
        }, empty, initial);
        let result = List.foldLeft(move(reverse), stacks, moves);
        let stringList = List.map(Char.toString, List.filterMap(List.head, result));
        List.foldLeft(String.concat, "", stringList)
    }

    def get(ix: Int32, list: List[a], default: a): a = {
        list |> List.drop(ix) |> List.head |> Option.getWithDefault(default)
    }

    pub def move(reverse: Bool, stacks: List[List[c]], move: (Int32, Int32, Int32)): List[List[c]] = {
        let (c, f1, t1) = move;
        let f = f1 - 1;
        let t = t1 - 1;
        let fStack = get(f, stacks, Nil);
        let tStack = get(t, stacks, Nil);
        let toMove = fStack |> List.take(c) |> (if (reverse) List.reverse else identity);
        stacks |> List.update(t, List.append(toMove, tStack)) |> List.update(f, List.drop(c, fStack))
    }

    pub def main(): Unit \ IO = {
        match File.readLines("src/input5.txt") {
            case Err(err) => println(err)
            case Ok(lines) => {
                println(task1(lines, true));
                println(task1(lines, false));
                ()
            }
        };
        ()
    }
}


