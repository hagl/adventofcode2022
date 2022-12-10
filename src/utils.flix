namespace Utils {

    pub def printGrid(pointSet: Set[(Int32, Int32)]): Unit \ IO = {
        let xs = Set.map(match (x, _y) -> x, pointSet) |> Set.toList;
        let ys = Set.map(match (_x, y) -> y, pointSet) |> Set.toList;
        match (List.minimum(xs), List.maximum(xs), List.minimum(ys), List.maximum(ys)) {
            case (Some(minX), Some(maxX), Some(minY), Some(maxY)) =>
                println("(${minX}, ${minY}) - (${maxX + 1}, ${maxY + 1})");
                List.forEach(y -> {
                    List.forEach(x -> if (Set.memberOf((x,y), pointSet)) print('⬛') else print("⬜"), List.range(minX, maxX + 1));
                    println("")
                }, List.range(minY, maxY + 1))
            case (minX, maxX, minY, maxY) =>
                println("(${minX}, ${minY}) - (${maxX}, ${maxY})")
        }
    }
}
