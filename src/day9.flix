namespace Day9 {

    type alias Point = (Int32, Int32)

    def moveHead(dir: Char, point: Point) : (Int32, Int32) = {
        match (dir, point) {
            case ('U', (x, y)) => (x, y - 1)
            case ('D', (x, y)) => (x, y + 1)
            case ('L', (x, y)) => (x - 1, y)
            case ('R', (x, y)) => (x + 1, y)
            case _ => point
        }
    }

    def moveTails(prev: Point, tails: List[Point]) : List[Point] = match tails {
        case Nil => Nil
        case h::t => {
            let newHead = moveTail(prev, h);
            newHead :: moveTails(newHead, t)
        }
    }

    def moveTail(head: Point, tail: Point) : Point = {
        match (head, tail) {
            case ((hx, hy), (tx, ty)) => {
                let dx = hx - tx;
                let dy = hy - ty;
                if (Int32.abs(dx) > 1 or Int32.abs(dy) > 1) {
                    (tx + Int32.signum(dx), ty + Int32.signum(dy))
                } else {
                    tail
                }
            }
        }
    }


   def task1(input: List[String]): Int32 \ IO= {
        let (visited, _, _) = List.foldLeft(acc -> line -> {
            let dir = String.charAt(0, line);
            let count = Int32.parse(10, String.drop(2, line));
            match (count) {
                case Ok(c) => {
                    List.foldLeft(match (set, head, tail) -> _ ->{
                        let newHead = moveHead(dir, head);
                        let newTail = moveTail(newHead, tail);
                        let newSet = Set.insert(newTail, set);
                        // println(printGrid(newSet));
                        (newSet, newHead, newTail)
                    }, acc, List.range(0, c))
                }
                case Err(e) => {
                    println(e);
                    acc
                }
            }
       }, (Set.empty(), (0,0), (0,0)), input);
        Set.size(visited)
   }

   def task2(input: List[String]): Int32 \ IO= {
        let (visited, _, _) = List.foldLeft(acc -> line -> {
            let dir = String.charAt(0, line);
            let count = Int32.parse(10, String.drop(2, line));
            match (count) {
                case Ok(c) => {
                    List.foldLeft(match (set, head, tails) -> _ ->{
                        let newHead = moveHead(dir, head);
                        let newTails = moveTails(newHead, tails);
                        let newSet = match List.last(newTails) {
                            case Some(tail) =>  Set.insert(tail, set)
                            case _ => set
                        };
                        // println(printGrid(newSet));
                        (newSet, newHead, newTails)
                    }, acc, List.range(0, c))
                }
                case Err(e) => {
                    println(e);
                    acc
                }
            }
       }, (Set.empty(), (0,0), List.repeat(9, (0,0))), input);
        Set.size(visited)
   }

    pub def printGrid(pointSet: Set[(Int32, Int32)]): Unit \ IO = {
        let xs = Set.map(match (x, _y) -> x, pointSet) |> Set.toList;
        let ys = Set.map(match (_x, y) -> y, pointSet) |> Set.toList;
        match (List.minimum(xs), List.maximum(xs), List.minimum(ys), List.maximum(ys)) {
            case (Some(minX), Some(maxX), Some(minY), Some(maxY)) =>
                println("(${minX}, ${minY}) - (${maxX}, ${maxY})");
                List.forEach(y -> {
                    List.forEach(x -> if (Set.memberOf((x,y), pointSet)) print('X') else print("_"), List.range(minX, maxX));
                    println("")
                }, List.range(minY, maxY))
            case (minX, maxX, minY, maxY) =>
                println("(${minX}, ${minY}) - (${maxX}, ${maxY})")
        }
    }

    pub def main(): Unit \ IO = {
        match File.readLines("src/input9.txt") {
            case Err(err) => println(err)
            case Ok(lines) => {
                println(task1(lines));
                println(task2(lines));
                ()
            }
        };
        ()
    }
}


