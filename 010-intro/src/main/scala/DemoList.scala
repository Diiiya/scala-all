object DemoList {
    var nums = List(0, 1, 1, 2, 5)

    def main(args: Array[String]): Unit = {

        // Not RT => Not good for FP
        println("For Loop:")
        for(n <- nums) println(n)

        println("---")

        // nums.reverse does not change the original nums
        nums.reverse;
        println("For each Loop:")
        nums.foreach { i: Int => println(i) }

        println("---")

        // nums.reverse changes the original nums if it is assigned to a new var
        // like a return method does nothing if not assigned to a var
        var reversedNums = nums.reverse;
        println("For each Loop Reverse:")
        reversedNums.foreach { i: Int => println(i) }

        println("---")

        // drop first 3 and take only 1 following -- 0, 1, 1, 2, 5 -> output: 2
        var dropTake = nums.drop(3).take(1);
        println("Drop && Take v1:" + dropTake)
        
        var dropTakeV2 = nums drop 3 take 1;        
        println("Drop && Take v2:" + dropTakeV2)
    }
}