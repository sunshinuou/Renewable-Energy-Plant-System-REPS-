import java.io.File
import scala.io.Source
import scala.util.Try
import java.awt.Font
import java.io.File
import java.util.Vector
import javax.swing.{JFrame, JScrollPane, JTable}

class Hydropower(eType: String) extends RE {
  
  //Override the method status(status: String) of the trait
  override def status(status: String): String = {
    println(s"The current status is $status MW")
    println("Please select an water rate to adjust:")
    println("1. 15 m³/s (low rate)")
    println("2. 20 m³/s (middle rate)")
    println("3. 25 m³/s (high rate)")
    val choice = scala.io.StdIn.readInt()
    val newStatus: String = choice match {
      case 1 => "15"
      case 2 => "20"
      case 3 => "25"
      case _ =>
        println("Invalid choice. Please try again.")
        status
    }
    return newStatus
  }
  
  //Heritage the method from the trait
  override def adjust(equipmentType: String, f: String => String): Unit = super.adjust(equipmentType, status)

  //Override the method view() of the trait
  override def view(): Unit = {
    val filePower = "src/PowerData.csv"
    val file = new File("")
    val project_path = file.getAbsolutePath()
    val fileEquipment = "src/equipment.csv"
    val lines = Source.fromFile(filePower).getLines().toList

    val equipmentRows = Source.fromFile(fileEquipment).getLines().toList
    val equipmentMap = equipmentRows.map { row =>
      val cols = row.split(",").map(_.trim)
      cols(0) -> (Try(cols(1).toInt).getOrElse(0), Try(cols(2).toInt).getOrElse(0), Try(cols(3)).getOrElse(0))
    }.toMap
    val (running, total, status) = equipmentMap.getOrElse(eType, (0, 0, null))

    UIview()

    def UIview() = {
      //Create a table view with the fetched data.
      
      val data = new Vector[Vector[String]]()
      data.add(new Vector[String](java.util.Arrays.asList("Power generated in the last hour", lines.last.split(",")(4) + " MWh/h")))
      data.add(new Vector[String](java.util.Arrays.asList(s"Number of $eType in operation", s"$running")))
      data.add(new Vector[String](java.util.Arrays.asList(s"Total number of $eType", s"$total")))
      data.add(new Vector[String](java.util.Arrays.asList(s"The water rate of the $eType is currently (m³/s)", s"$status")))
      data.add(new Vector[String](java.util.Arrays.asList("Storage capacity", "1000 MWh")))

      val columnNames = new Vector[String](java.util.Arrays.asList("Description", "Value"))
      
      //Some formats are prescribed for aesthetic purposes.
      val table = new JTable(data, columnNames) {
        setFont(new Font("Arial", Font.PLAIN, 16))
        setRowHeight(25)
        setFillsViewportHeight(true)
        setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS)
        getColumnModel.getColumn(0).setPreferredWidth(350)
        getColumnModel.getColumn(1).setPreferredWidth(100)
      }

      val scrollPane = new JScrollPane(table)
      val frame = new JFrame(s"$eType")
      frame.add(scrollPane)
      frame.setSize(450, 150)
      frame.pack()
      frame.setLocationRelativeTo(null)
      frame.setVisible(true)
    }
  }

}
