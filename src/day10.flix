namespace Day10 {

    enum Instruction {
        case NoOp
        case AddX(Int32)
    }

    def parse(input: List[String]): Result[List[Instruction], String] = {
        Result.traverse(line -> match String.splitOn({substr = " "}, line) {
            case "noop" :: Nil => Ok(NoOp)
            case "addx" :: arg :: Nil => Result.map(AddX, Int32.parse(10, arg))
            case _ => Err("Parse error: ${line}")
        }, input)
    }

   def task1(input: List[Instruction]): Int32 = region r {
        let (_lastX, rev) = List.foldLeft(match (x, res) -> instruction -> match instruction {
            case NoOp => (x, x :: res)
            case AddX(d) => (x + d, x :: x :: res)
        }, (1, Nil), input);

        let a = List.toArray(r, List.reverse(rev));

        let cycles = 20 :: 60 :: 100 :: 140 :: 180 :: 220 :: Nil;

        List.foldLeft(acc -> x -> {
                acc + (a[x-1] * x)
            }, 0, cycles)
   }

   def task2(input: List[Instruction]): Unit \ IO = region r {
        let (_lastX, rev) = List.foldLeft(match (x, res) -> instruction -> match instruction {
            case NoOp => (x, x :: res)
            case AddX(d) => (x + d, x :: x :: res)
        }, (1, Nil), input);

        let a = List.toArray(r, List.reverse(rev));
        let (res, _)= Array.foldLeft(match (set, ix) -> v -> {
            let x = ix mod  40;
            let y = ix / 40;
            let newSet : Set[(Int32, Int32)] = if (Int32.abs(x - v) < 2) Set.insert((x,y), set) else set;
            (newSet, ix + 1)
        }, (Set.empty(), 0), a);
        Utils.printGrid(res)
   }

    pub def main(): Unit \ IO = {
        use Result.flatMap;
        let result = {
            let* lines = File.readLines("src/input10.txt");
            let* parsed = parse(lines);
            println(task1(parsed));
            task2(parsed);
            Ok ()
        };
        match result {
            case Err(err) => println(err)
            case Ok(()) => ()
        }
    }
}
