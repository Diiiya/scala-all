object DemoClass {

    // Student constructor example
    case class Student(var rollNo : Int, var name : String, var marks : Int)

    // Student constructor example with default values
    case class DefaultStudent(var rollNo : Int = 1, var name : String = "John" , var marks : Int = 80)

    var ds1 = DefaultStudent();
    // automatically overrides the default:
    var ds2 = DefaultStudent(2, "Jane", 88);
    // if not all params are given, it will override them starting with the 1st (if the type matches)
    var ds3 = DefaultStudent(3, "Jill");


    // Student constructor with methods
    case class DefaultStudentMethods(var rollNo : Int = 1, var name : String = "John" , var marks : Int = 80)
    {
        def sayHi() = println("Heeej");

        // Compares the marjs on student 1 and student and returns true if student 1.marks > student.marks
        def >(student : DefaultStudentMethods) : Boolean = marks > student.marks
    }

    var ds4 = DefaultStudentMethods(4, "Lalala");
    var ds5 = DefaultStudentMethods(4, "LOL", 100);
    var ds6 = DefaultStudentMethods(4, "LOL", 50);

    def main(args: Array[String]): Unit = {
        println("Default Student 1: " + ds1)
        println("Default Student 2: " + ds2)
        println("Default Student 3: " + ds3)

        ds4.sayHi();

        var marksResult = ds4.>(ds5);
        println("Student 4 (" + ds4.marks + ") has HIGHER marks that Student 5 (" + ds5.marks + ") : " + marksResult);
        
        var marksResult2 = ds4.>(ds6);
        println("Student 4 (" + ds4.marks + ") has HIGHER marks that Student 6 (" + ds6.marks + ") : " + marksResult2);
    }

}