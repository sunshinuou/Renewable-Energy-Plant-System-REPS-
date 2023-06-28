import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.io.{Source, StdIn}
import java.io.PrintWriter

object dataProcessor {
  val fName = "src/PowerData.csv"

  def collection(): Unit = {
    //Read the data files of three different renewable energy sources separately. and merge the data to write in a new file.

    val file1 = "src/wind.csv"
    val file2 = "src/solar.csv"
    val file3 = "src/hydro.csv"

    val outFile = new PrintWriter(fName)

    val file1Columns = Source.fromFile(file1).getLines.map(line => {
      val cols = line.split(",").take(3)
      cols.mkString(",")
    }).toList

    val file2Columns = Source.fromFile(file2).getLines.map(line => {
      val cols = line.split(",")
      val thirdCol = cols(2)
      thirdCol
    }).toList

    val file3Columns = Source.fromFile(file3).getLines.map(line => {
      val cols = line.split(",")
      val thirdCol = cols(2)
      thirdCol
    }).toList

    val writer = new PrintWriter(outFile)

    for (i <- 0 until file1Columns.size) {
      val file1Cols = file1Columns(i).split(",")
      val file2Col = file2Columns(i)
      val file3Col = file3Columns(i)
      writer.println(s"${file1Cols(0)},${file1Cols(1)},${file1Cols(2)},$file2Col,$file3Col")
    }

    writer.close()
    println("Successfully collected and stored.")
  }

  def dataDetecting(): Unit = {
    //For the detection and treatment of renewable energy sources.
    //Allows the user to select the type of generation to be detected and to specify minimum standards.
    
    process()
    def process(): Unit = {
      try {
        val a = readIntFromUser("Enter the type of energy you want to detect: \n1.Wind Turbine\n2.Solar Panel\n3.Hydropower\n", List(1, 2, 3))
        val b = readIntFromUser("Enter the minimum energy criteria for low energy output: ")

        val data = readCsvFile(fName)
        val selectedColumn = a + 1

        val filteredData = data.filter(row => {
          val value = row(selectedColumn).toDouble
          value < b
        })

        if (filteredData.nonEmpty) {
          filteredData.foreach(row => {
            val selectedRowData = s"StartTime(UTC): ${row.head}, EndTime(UTC): ${row(1)}, MWh/h: ${row(selectedColumn)}"
            //Automatically determines machine failure and reports a warning if any data falls below 0
            if (row(selectedColumn).toDouble < 0) {
              println(s"- ${selectedRowData} \n- WARNING: data less than zero, equipment malfunction.")
            } else {
              println(selectedRowData)
            }
          })
        } else {
          println("No data found. All values are greater than the given criteria.")
        }

        val repeat = repeatOrNot("Do you want to continue detecting? (y/n): ")
        if (repeat) {
          process()
        }
      } catch {
        case e: Exception => println(e.getMessage)
      }
    }

    def readIntFromUser(prompt: String, validValues: List[Int] = List()): Int = {
      val input = scala.io.StdIn.readLine(prompt).trim.toInt
      if (validValues.nonEmpty && !validValues.contains(input)) {
        throw new Exception("Invalid input value. Only 1/2/3 can be entered.")
      }
      input
    }

    def readCsvFile(fileName: String): List[List[String]] = {
      val source = Source.fromFile(fileName)
      try {
        val lines = source.getLines().toList
        //Read data and remove header row
        lines.drop(1).map(_.split(",").toList)
      } finally {
        source.close()
      }
    }
  }


  def repeatOrNot(prompt: String): Boolean = {
    val input = scala.io.StdIn.readLine(prompt).trim.toLowerCase
    input match {
      case "y" | "yes" => true
      case "n" | "no" => false
      case _ => throw new Exception("Invalid input value.")
    }
  }


  def dataHandleCenter(): Unit = {
    //function dataHandle is to get the user selections of energy type and the kinid of sort, filter the data according to selections and analyze data
    dataHandle()
    def dataHandle(): Unit = {
      //get user selections
      println("Select an energy type you want to filter:\n 1.Wind Turbine\n 2.Solar Panel\n 3.Hydropower")
      val energy = StdIn.readInt()
      println("select what kind of sort you want:\n 1.Every Hour\n 2.Every Day\n 3.Every Week\n 4.Every Month")
      val select = StdIn.readInt()
      println("What analysis do you want:\n 1.Average\n 2.Middle\n 3.Mode\n 4.Range\n 5.Midrange")
      val ope = StdIn.readInt()
      //if selections is not valid, it will print error message and return back
      if ((energy != 1 && energy != 2 && energy != 3) || (select != 1 && select != 2 && select != 3 && select != 4) || (ope != 1 && ope != 2 && ope != 3 && ope != 4 && ope != 5)) {
        println("Your input is not in the requirement!")
        dataHandle()
      } else {
        val column = energy + 1
        val list = filterData(column, select)
        dataAnalyze(ope, list)
      }
      //ask if user want to continue
      val repeat = repeatOrNot("Do you want to continue analysis? (y/n): ")
      if (repeat) {
        dataHandle()
      }
    }
    //filterData will filter data by hour/day/week/month
    def filterData(column: Int, select: Int): List[Double] = {
      val lines = Source.fromFile(fName).getLines().toList
      val dataColumn = lines.drop(1).map(_.split(",")(column).toDouble)
      //get data from file and then change the time column from String to time form
      val dateFormat = DateTimeFormatter.ofPattern("yyyy/M/d H:mm")
      
      //filter by hours
      if (select == 1) {
        val hourData = lines.drop(1).zipWithIndex.foldLeft(Map.empty[Int, Double].withDefaultValue(0.0)) { (acc, lineIndex) =>
          val (line, index) = lineIndex
          val dateStr = line.split(",")(0)
          val date = LocalDate.parse(dateStr, dateFormat)
          val hourNumber = index + 1 // Each week contains 168 data points
          val value = line.split(",")(column).toDouble
          acc + (hourNumber -> (acc(hourNumber) + value))
        }
        val hours = hourData.toList.sortBy(_._1)
        val hourList = hours.map(_._2)
        hourList
      } else if (select == 2) {//filter by day
        val dayliyData = lines.drop(1).zipWithIndex.foldLeft(Map.empty[Int, Double].withDefaultValue(0.0)) { (acc, lineIndex) =>
          val (line, index) = lineIndex
          val dateStr = line.split(",")(0)
          val date = LocalDate.parse(dateStr, dateFormat)
          val dayNumber = (index / 24) + 1 // Each week contains 168 data points
          val value = line.split(",")(column).toDouble
          acc + (dayNumber -> (acc(dayNumber) + value))
        }
        val days = dayliyData.toList.sortBy(_._1)
        val dayList = days.map(_._2)
        dayList
      } else if (select == 3) {//filter by week
        val weeklyData = lines.drop(1).zipWithIndex.foldLeft(Map.empty[Int, Double].withDefaultValue(0.0)) { (acc, lineIndex) =>
          val (line, index) = lineIndex
          val dateStr = line.split(",")(0)
          val date = LocalDate.parse(dateStr, dateFormat)
          val weekNumber = (index / 168) + 1 // Each week contains 168 data points
          val value = line.split(",")(column).toDouble
          acc + (weekNumber -> (acc(weekNumber) + value))
        }
        val weeks = weeklyData.toList.sortBy(_._1)
        val weekList = weeks.map(_._2)
        weekList
      } else {//filter by month
        val monthlyData = lines.drop(1).foldLeft(Map.empty[String, Double].withDefaultValue(0.0)) { (acc, line) =>
          val dateStr = line.split(",")(0)
          val date = LocalDate.parse(dateStr, dateFormat)
          val month = date.getMonth.toString
          val value = line.split(",")(column).toDouble
          acc + (month -> (acc(month) + value))
        }
        val months = monthlyData.toList
        val monthList = months.map(_._2)
        monthList
      }
    }
    
    //dataAnalyze it to analyze data to calculate their average/middle/mode/range/midrange by the selection of user
    def dataAnalyze(i: Int, list: List[Double]): Any = {
      if (i == 1) {
        val avg = average(list)
        println("The average is "+avg)
      } else if (i == 2) {
        val mid = middle(list)
        println("The middle is "+mid)
      } else if (i == 3) {
        val mo = mode(list)
        println("The mode is "+mo)
      } else if (i == 4) {
        val ran = range(list)
        println("The range is " +ran)
      } else {
        val midran = midrange(list)
        println("The midrange is "+midran)
      }
    }
    
    //calculate average
    def average(input: List[Double]): Double = {
      input.sum / input.length
    }
    //calcultae middle
    def middle(input: List[Double]): Double = {
      val sortedList = input.sorted
      val middleIndex = sortedList.length / 2
      if (sortedList.length % 2 == 0) {
        val middle1 = sortedList(middleIndex - 1)
        val middle2 = sortedList(middleIndex)
        (middle1 + middle2) / 2.0
      } else {
        sortedList(middleIndex)
      }
    }
    //calculate mode
    def mode(list: List[Double]): List[Double] = {
      val frequencyMap = list.groupBy(identity).mapValues(_.size)
      val maxFrequency = frequencyMap.values.max
      val modes = frequencyMap.filter { case (_, frequency) => frequency == maxFrequency }.keys.toList
      modes
    }
    //calculate range
    def range(list: List[Double]): Double = {
      val sortedList = list.sorted
      val difference = sortedList.last - sortedList.head
      difference
    }
    //calculate midrange
    def midrange(list: List[Double]): Double = {
      val min = list.min
      val max = list.max
      val midrange = (min + max) / 2.0
      midrange
    }

  }

}
