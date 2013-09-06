package multitool

import opennlp.tools.sentdetect._
import opennlp.tools.postag._
import java.io.FileInputStream

object OpenNLP {
  lazy val sd_model : SentenceModel = new SentenceModel(new FileInputStream("src/main/resources/en-sent.bin"))
  lazy val mSD = new SentenceDetectorME(sd_model)
  lazy val pos_model = new POSModel(new FileInputStream("src/main/resources/en-pos-maxent.bin"))
  lazy val mPT = new POSTaggerME(pos_model)

  def sentenceDetect(s : String) : Array[String] = {
    mSD.sentDetect(s)
  }

  def postag(s : String) : Array[String] = {
    mPT.tag(s.split("\\s+"))
  }
}
