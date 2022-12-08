namespace Day8 {


   def task1(input: List[List[Char]]): Int32 = {
        let area = List.mapWithIndex(y -> List.mapWithIndex(x -> h -> ((x,y), (Char.toInt32(h)-48))), input);
        let visibles : Set[(Int32, Int32)] = Set.empty();
        let visiblesLR1 = List.foldLeft(v -> row ->
            fst(List.foldLeft(match (vr, currentHeight) -> match (xy, h) -> if (h <= currentHeight) (vr, currentHeight) else (Set.insert(xy, vr), h), (v, -1), row))
        , visibles, area);

        let visiblesRL2 = List.foldLeft(v -> row ->
            fst(List.foldLeft(match (vr, currentHeight) -> match (xy, h) -> if (h <= currentHeight) (vr, currentHeight) else (Set.insert(xy, vr), h), (v, -1), List.reverse(row)))
        , visiblesLR1, area);

        let visiblesTD3 = List.foldLeft(v -> row ->
            fst(List.foldLeft(match (vr, currentHeight) -> match (xy, h) -> if (h <= currentHeight) (vr, currentHeight) else (Set.insert(xy, vr), h), (v, -1), row))
        , visiblesRL2, List.transpose(area));

        let visiblesDT4 = List.foldLeft(v -> row ->
            fst(List.foldLeft(match (vr, currentHeight) -> match (xy, h) -> if (h <= currentHeight) (vr, currentHeight) else (Set.insert(xy, vr), h), (v, -1), List.reverse(row)))
        , visiblesTD3, List.transpose(area));

        Set.size(visiblesDT4)
   }


    def visibleTrees(s: Int32, l: List[Int32]): Int32 = match l {
        case Nil => 0
        case h :: t => if (h >= s) 1 else (1 + visibleTrees(s, t))
    }

    def visibleTreesRow(l: List[Int32]): List[Int32] = match l {
        case Nil => Nil
        case h :: t => visibleTrees(h, t) :: visibleTreesRow(t)
    }


    def mult(a: List[List[Int32]], b: List[List[Int32]]): List[List[Int32]] = List.zipWith(List.zipWith(x -> y -> x * y), a, b)

    def task2(input: List[List[Char]]): Option[Int32] = {
        let area = List.map(List.map(h -> Char.toInt32(h)-48), input);
        let distLR = List.map(visibleTreesRow, area);
        let distRL = List.map(List.reverse >> visibleTreesRow >> List.reverse, area);
        let distUD = List.transpose(List.map(visibleTreesRow,List.transpose(area)));
        let distDU = List.transpose(List.map(List.reverse >> visibleTreesRow >> List.reverse,List.transpose(area)));
        let prods = mult(mult(distLR, distRL), mult(distUD, distDU));
        List.maximum(List.flatten(prods))
   }

    pub def printGrid(xMax: Int32, yMax: Int32, set: Set[(Int32, Int32)]): Unit \ IO = {
        List.forEach(y -> {
            List.forEach(x -> if (Set.memberOf((x,y), set)) print('X') else print("_"), List.range(0, xMax));
            println("")
        }, List.range(0, yMax))
    }

    pub def main(): Unit \ IO = {
        match File.readLines("src/input8.txt") {
            case Err(err) => println(err)
            case Ok(lines) => {
                println(task1(List.map(String.toList, lines)));
                println(task2(List.map(String.toList, lines)));
                ()
            }
        };
        ()
    }
}


