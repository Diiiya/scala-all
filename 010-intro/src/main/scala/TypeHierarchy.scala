object TypeHierarchy {
    // No error, List with dynamically assigned "Any" type for the data vars
    var random = List(0, 1, 1, true, "Navidad")

    // create Student class
    case class Student(id : Int, name : String, marks : Int)

    val students = List(Student(1, "John", 80), Student(2, "Jane", 88), Student(3, "Lol", 40))

    // . head gives the 1st element of a List
    // . tail -- the last

    // Gets the students with grades > 60
    val toppers = students.filter(s => s.marks >= 60);

    // Creates (a tuple) 2 lists > 1st satisfies the condition and 2nd - returns the rest
    val toppersPartition = students.partition(s => s.marks >= 60);

    // Those 2 lists can be accessed:
    val partAbove60 = toppersPartition._1;
    val partBelow60 = toppersPartition._2;

    // Same could also be written just as:
    val (partAbove60v2, partBelow60v2) = students.partition(s => s.marks >= 60);

    def main(args: Array[String]): Unit = {
        println(random);
        println("Student 1st: " + students.head);
        println("Toppers: " + toppers);
        println("Toppers partition: " + toppersPartition);
        println("Toppers Above 60: " + partAbove60);
        println("Toppers Below 60: " + partBelow60);
    }
}