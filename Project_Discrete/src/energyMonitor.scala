import java.io._
import scala.io._
import scala.util.{Failure, Success, Try}
object energyMonitor {
  
  //The method to monitor the information of equipment
  def monitor(): Unit = {
    val file = new File("")
    val project_path = file.getAbsolutePath()
    val fileName = "src/equipment.csv"
    val equipmentRows = Source.fromFile(fileName).getLines().toList
    val equipmentMap = equipmentRows.map { row =>
      val cols = row.split(",").map(_.trim)
      cols(0) -> (Try(cols(1).toInt).getOrElse(0), Try(cols(2).toInt).getOrElse(0), Try(cols(3)).getOrElse(0))
    }.toMap

    println("Which type would you like to monitor?")
    println("1. Solar panels")
    println("2. Wind turbines")
    println("3. Hydropower")
    println("4. Exit")
    val choice = Try(StdIn.readInt()).getOrElse(0)

    choice match {
      case 1 =>
        val (running, total, status) = equipmentMap.getOrElse("Solar Panel", (0, 0, null))
        println(s"Type: Solar panel\nNumber of Solar panel in operation: $running\nTotal number of Solar panel: $total\nThe angle of the solar panel is currently (°): $status")
        println()
        monitor()
      case 2 =>
        val (running, total, status) = equipmentMap.getOrElse("Wind Turbine", (0, 0, null))
        println(s"Type: Wind turbine\nNumber of Wind turbine in operation: $running\nTotal number of Wind turbine: $total\nThe power of the turbine is currently (MW): $status")
        println()
        monitor()
      case 3 =>
        val (running, total, status) = equipmentMap.getOrElse("Hydropower", (0, 0, null))
        println(s"Type: Hydropower\nNumber of Hydropower in operation: $running\nTotal number of Hydropower: $total\nThe flow rate of water is currently (m³/s): $status")
        println()
        monitor()
      case 4 => println("Exiting...")
      case _ =>
        println("Invalid choice.")
        println()
        monitor()

    }
  }
  
  //The method to control the running number of the equipment
  def control(): Unit = {
    val file = new File("")
    val project_path = file.getAbsolutePath()
    val fileName = "src/equipment.csv"
    val lines = io.Source.fromFile(fileName).getLines().drop(1)
    val equipment = lines.map(line => {
      val cols = line.split(",").map(_.trim)
      (cols(0), cols(1).toInt, cols(2).toInt, cols(3))
    }).toArray

    println("Which equipment would you like to control?")
    println("1. Solar Panels")
    println("2. Wind Turbines")
    println("3. Hydropower")
    println("4. Exit")
    val choice = StdIn.readInt()

    choice match {
      case 1 =>
        updateEquipment("Solar Panel", equipment)
        control()
      case 2 =>
        updateEquipment("Wind Turbine", equipment)
        control()
      case 3 =>
        updateEquipment("Hydropower", equipment)
        control()
      case 4 => println("Exiting...")
      case _ =>
        println("Invalid choice. Please try again.")
        control()
    }

    def updateEquipment(equipmentType: String, equipmentList: Array[(String, Int, Int, String)]): Unit = {
      val equipment = equipmentList.filter(_._1 == equipmentType).head
      val (eType, runningNum, totalNum, status) = equipment

      println(s"Selected $eType (Running: $runningNum  Total: $totalNum)")
      println("Do you want to increase or decrease the running number? Enter + to increase or - to decrease.")
      val operator = StdIn.readLine()

      println("Enter the number of units to increase or decrease:")
      val input = Try(StdIn.readInt())

      input match {
        case Success(units) =>
          val newRunningNum: Int = operator match {
            case "+" => runningNum + units
            case "-" => runningNum - units
            case _ => runningNum
          }
          if (newRunningNum > totalNum) {
            throw new IllegalArgumentException(s"Running number cannot exceed total number ($totalNum)")
          }
          val newEquipmentList = equipmentList.map {
            case (`eType`, _, _, _) => (eType, newRunningNum, totalNum, status)
            case other => other
          }
          writeToFile(fileName, newEquipmentList)
          println(s"$eType updated. New running number: $newRunningNum")
        case Failure(_) => println("Invalid input. Please enter a number.")
      }
    }

    def writeToFile(fileName: String, equipmentList: Array[(String, Int, Int, String)]): Unit = {
      val pw = new PrintWriter(new File(fileName))
      pw.write("Type,Running number,Total number,Status\n")
      equipmentList.foreach(equipment => {
        pw.write(equipment.productIterator.mkString(",") + "\n")
      })
      pw.close()
    }
  }

}
