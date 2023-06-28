import java.io.{File, PrintWriter}

trait RE {
  //Define the abstract method status() and view()
  def status(s: String): String
  def view(): Unit
  
  //The method to adjust the status of equipment
  def adjust(equipmentType: String, f: String => String): Unit = {
    val file = new File("")
    val project_path = file.getAbsolutePath()
    val fileEquipment = "src/equipment.csv"
    val line1 = io.Source.fromFile(fileEquipment).getLines().drop(1)
    val equipmentList = line1.map(line => {
      val cols = line.split(",").map(_.trim)
      (cols(0), cols(1).toInt, cols(2).toInt, cols(3))
    }).toArray
    val equipment = equipmentList.filter(_._1 == equipmentType).head
    val (eType, runningNum, totalNum, status) = equipment
    val newStatus = f(status)
    val newequipmentList = equipmentList.map {
      case (`eType`, _, _, _) => (eType, runningNum, totalNum, newStatus)
      case other => other
    }
    println(s"$eType status updated.")
    val pw = new PrintWriter(new File(fileEquipment))
    pw.write("Type,Running number,Total number,Status\n")
    newequipmentList.foreach(equipment => {
      pw.write(equipment.productIterator.mkString(",") + "\n")
    })
    pw.close()
  }










}

