namespace Day12 {


    type alias Point = (Int32, Int32)

    type alias Point3d = (Int32, Int32, Int32)

    pub def getEl(ix: Int32, default: a, list: List[a]): a =
        list |> List.drop(ix) |> List.head |> Option.getWithDefault(default)

    pub def search(step: Int32, current: List[Point3d], visited: Set[Point], grid: List[List[Int32]]) : Option[Int32] \ IO = {
        println("${step} : ${current}");
        let end = Char.toInt32('E');
        let zh = Char.toInt32('z');
        let (nextPoints, newVisited) = List.foldLeft(match (nl, v) -> match (x, y, h) -> {
                List.foldLeft(match (nl1, v1) -> match (dx, dy) -> {
                    let (nx, ny) = (x + dx, y + dy);
                    // let nh = grid[ny][nx];
                    let nh = getEl(nx, -1, getEl(ny, Nil, grid));
                    if (((nh == end and zh - h <= 1) or (nh != end and nh - h <= 1)) and not Set.memberOf((nx, ny), v1)) {
                        ((nx, ny, nh) :: nl1, Set.insert((nx, ny), v1))
                    } else (nl1, v1)
                }, (nl, v), (-1, 0) :: (0, 1) :: (1, 0) :: (0, -1) :: Nil)
            }, (Nil, visited), current);

        match List.find(match (_x, _y, h) -> h == end, nextPoints) {
            case Some(_) => Some(step)
            case _ => search(step + 1, nextPoints, newVisited, grid)
        }
    }


    def task1(lines: List[String]): Option[Int32] \ IO = {
        let width = lines |> List.head |> Option.map(String.length) |> Option.getWithDefault(0);
        let grid : List[List[Int32]] = List.append(String.repeat(width, "|") :: lines, String.repeat(width, "|") :: Nil)
            |> List.map(line -> "|${line}|")
            |> List.map(String.toList)
            |> List.map(List.map(Char.toInt32));

        let cs = Char.toInt32('S');

        let startY = List.find(match (row, _ix) -> List.count(c -> c == cs, row) > 0, List.zipWithIndex(grid)) |> Option.map(snd) |> Option.getWithDefault(0);
        let startX = List.indexOf(cs, getEl(startY, Nil, grid)) |> Option.getWithDefault(0);
        println("${startX}, ${startY}");

        search(1, (startX, startY, Char.toInt32('a')) :: Nil, Set.singleton((startX, startY)), grid)
    }


    pub def search2(step: Int32, current: List[Point3d], visited: Set[Point], grid: List[List[Int32]]) : Option[Int32] \ IO = {
        println("${step} : ${current}");
        match current {
            case Nil => None
            case _ => let ah = Char.toInt32('a');
                let (nextPoints, newVisited) = List.foldLeft(match (nl, v) -> match (x, y, h) -> {
                        List.foldLeft(match (nl1, v1) -> match (dx, dy) -> {
                            let (nx, ny) = (x + dx, y + dy);
                            // let nh = grid[ny][nx];
                            let nh = getEl(nx, -1, getEl(ny, Nil, grid));
                            if ((h - nh <= 1) and not Set.memberOf((nx, ny), v1)) {
                                ((nx, ny, nh) :: nl1, Set.insert((nx, ny), v1))
                            } else (nl1, v1)
                        }, (nl, v), (-1, 0) :: (0, 1) :: (1, 0) :: (0, -1) :: Nil)
                    }, (Nil, visited), current);

                match List.find(match (_x, _y, h) -> h == ah, nextPoints) {
                    case Some(_) => Some(step)
                    case _ => search2(step + 1, nextPoints, newVisited, grid)
                }
        }
    }


    def task2(lines: List[String]): Option[Int32] \ IO = {
        let width = lines |> List.head |> Option.map(String.length) |> Option.getWithDefault(0);
        let grid : List[List[Int32]] = List.append(String.repeat(width, "|") :: lines, String.repeat(width, "|") :: Nil)
            |> List.map(line -> "|${line}|")
            |> List.map(String.replace({from = "S"}, {to = "a"}))
            |> List.map(String.toList)
            |> List.map(List.map(Char.toInt32));

        let cs = Char.toInt32('E');

        let startY = List.find(match (row, _ix) -> List.count(c -> c == cs, row) > 0, List.zipWithIndex(grid)) |> Option.map(snd) |> Option.getWithDefault(0);
        let startX = List.indexOf(cs, getEl(startY, Nil, grid)) |> Option.getWithDefault(0);
        println("${startX}, ${startY}");

        search2(1, (startX, startY, Char.toInt32('z')) :: Nil, Set.singleton((startX, startY)), grid)
    }

    pub def main(): Unit \ IO = {
        use Result.flatMap;
        let result = {
            let* lines = File.readLines("src/input12.txt");
            println(task1(lines));
            println(task2(lines));
            Ok ()
        };
        match result {
            case Err(err) => println(err)
            case Ok(()) => ()
        }
    }
}
