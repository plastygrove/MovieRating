package com.movierating

import java.awt.Dimension
import java.io.File
import java.io.PrintStream
import scala.Array.canBuildFrom
import scala.swing.Button
import scala.swing.FileChooser
import scala.swing.GridPanel
import scala.swing.Label
import scala.swing.MainFrame
import scala.swing.SimpleSwingApplication
import scala.swing.TextField
import scala.swing.event.ButtonClicked
import org.jsoup.Jsoup
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits._
import javax.swing.JProgressBar
import scala.swing.ProgressBar
import scala.swing.Swing
import java.awt.Color

object MovieRating extends SimpleSwingApplication {

  val txtStatus = new Label
  val progressBar = new ProgressBar

  def top = new MainFrame {
    println("Starting")
    title = "Movie Rating"
    val txtMovieFolder = new TextField
    val txtOutput = new TextField

    val lblMovieFolder = new Label {
      text = "Movie Folder"
    }

    val lblOutput = new Label {
      text = "Output File"
    }

    val btnDir = new Button {
      text = "Choose Movie Dir"
      reactions += {
        case ButtonClicked(_) => {
          val file = chooseDir("Choose Source Folder")
          txtMovieFolder.text = file.getOrElse("").toString
          txtMovieFolder.background = Color.YELLOW
        }
      }
    }

    val btnOutDir = new Button {
      text = "Choose Output Dir"
      reactions += {
        case ButtonClicked(_) => {
          val file = chooseDir("Choose Output file folder")
          txtOutput.text = file.getOrElse("").toString + "\\ratings.csv"
          txtOutput.background = Color.YELLOW
        }
      }
    }

    val btnRatings = new Button {
      text = "Click to get rating"
      reactions += {
        case ButtonClicked(_) => {
          if (txtMovieFolder.text == "") {
            txtMovieFolder.background = Color.RED
          } else if (txtOutput.text == "") {
            txtOutput.background = Color.red
          } else {
            val ratingFuture = future {
              getRatings(txtMovieFolder.text, txtOutput.text)
              //testRate
            }

            ratingFuture onSuccess {
              case _ => Swing.onEDT {
                txtStatus.text = "Done"
              }
            }
          }
        }
      }
    }

    val btnExit = new Button {
      text = "Exit"
      reactions += {
        case ButtonClicked(_) => System.exit(0)
      }
    }

    contents = new GridPanel(4, 3) {
      hGap = 5
      vGap = 5
      contents.append(lblMovieFolder, txtMovieFolder, btnDir, lblOutput, txtOutput, btnOutDir, new Label(""), btnRatings, btnExit)
      contents.append(txtStatus, new Label(""), progressBar)
    }
    size = new Dimension(450, 150)
    resizable = false

    def chooseDir(title: String = ""): Option[File] = {
      val chooser = new FileChooser(new File("."))
      chooser.title = title
      chooser.fileSelectionMode = FileChooser.SelectionMode.DirectoriesOnly
      val result = chooser.showOpenDialog(null)
      if (result == FileChooser.Result.Approve) {
        println("Approve -- " + chooser.selectedFile)
        Some(chooser.selectedFile)
      } else None
    }

  }

  def updateProgress(qty: Int) = {
    val fut = future {
      Swing.onEDT {
        progressBar.value = qty
      }
    }
  }

  def updateStatus(txt: String) = {
    val fut = future {
      Swing.onEDT {
        txtStatus.text = txt
      }
    }
  }

  def testRate = {
    (1 to 10) foreach { x =>
      Thread.sleep(1000)
      updateStatus(x.toString)
    }
  }

  def getRatings(dir: String, outCsv: String): Unit = {
    val regex = """(.*)\.(.*)""".r
    val regex2 = """(.*)([ ]*[0-9]{4}[ ]*)(.*)""".r
    val regex3 = """(.*)( .*[rR][iI][pP].*)(.*)""".r
    val regex4 = """(.*)( .*[sS][cC][rR].*)(.*)""".r

    //val dir = "E://MovieS/English"
    val movs = new File(dir).list
    val removeList = List("1080p", "720p", "480p", "560p", "HD")

    //val outFile = new File("C://Users//Abhiram//Desktop//tmp//movielistSEng.csv")
    val outFile = new File(outCsv)
    println(dir)
    println(outFile)
    if (!outFile.exists) outFile.createNewFile
    val outStream = new PrintStream(outFile)

    /*val outStream = new FileWriter("C://Users//Abhiram//Desktop//tmp")
			if(!outStream.exists)
				outStream.createNewFile
			*/

    val fixedMovs = movs.map(x => {
      val init = removeList.foldLeft(x)((p, q) => p.replace(q, ""))
      val first = init.replace("_", " ").replace("(", "").replace(")", "").replace("-", " ").replace("x264", "").replace("X264", "")
      val fixedName = regex.replaceAllIn(first, _.group(1)).replace(".", " ")
      val fixedName2 = regex2.replaceAllIn(fixedName, m => m.group(1) + m.group(3))
      val fixedName3 = regex3.replaceAllIn(fixedName2, m => m.group(1) + m.group(3))
      val fixedName4 = regex4.replaceAllIn(fixedName3, m => m.group(1) + m.group(3))
      fixedName4
    })

    outStream.print("Movie" + "," + "Rating\n	")
    println("Found movies")
    fixedMovs.toList.foreach(println(_))
    println("Starting scrape ... ")

    fixedMovs.toList.foreach(m => {
      updateStatus(m)

      def notFound = {
        println(m + ": Rating not found")
        outStream.print(m + "," + "N/A\n")
      }

      val gLink = "https://www.google.com/search?q=imdb+rating+" + (m.replace(" ", "+"))
      val wait = (Math.random() * 90).toInt
      Thread.sleep(wait)
      val doc = Jsoup.connect(gLink).userAgent("Mozilla/6.0 (Windows NT 6.2; WOW64; rv:16.0.1) Gecko/20121011 Firefox/16.0.1").referrer("http://www.google.com").get();
      println("Google connected")
      val linkEle = doc.select("cite").first
      if (linkEle != null) {
        val link = "http://" + linkEle.text
        if (link.contains("imdb")) {
          val imdbDoc = Jsoup.connect(link).userAgent("Mozilla/6.0 (Windows NT 6.2; WOW64; rv:16.0.1) Gecko/20121011 Firefox/16.0.1").referrer("http://www.google.com").get();
          println("imdb connected")
          val ratingEle = imdbDoc.select("div.star-box-details span[itemprop=ratingValue]").first
          if (ratingEle != null) {
            val rating = ratingEle.text
            println(m + ": " + rating)
            outStream.print(m + "," + rating + "\n")
          } else notFound
        } else notFound
      } else notFound
    })

    println("Scrape completed")
    outStream.close
  }

}