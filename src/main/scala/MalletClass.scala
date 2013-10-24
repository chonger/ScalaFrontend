import scala.collection.mutable.{HashSet,HashMap}
import cc.mallet.classify._
import cc.mallet.classify.evaluate._
import cc.mallet.pipe._
import cc.mallet.pipe.iterator._
import cc.mallet.types._
import multitool._

abstract class FExtractor[D,F] {
    
  def extract(d : D) : List[F] //take a datum and produce the list of features 
  def featureString(f : F) : String = { //print a string for a feature
    f.toString()
  }

  def extractBinary(d : D) : HashMap[F,Double] = {
    val fs = new HashSet[F]() ++ extract(d)
    val ret = new HashMap[F,Double]()
    fs.iterator.foreach(ret += _ -> 1.0)
    ret
  }
  def extractCounts(d : D) : HashMap[F,Double] = {
    var counts = extract(d).groupBy(x => x).map({
      case (f,fs) => (f,fs.length.toDouble)
    })
    val ret = new HashMap[F,Double]()
    counts.foreach(ret += _)
    ret
  }

}

class MalletClass[A](val regularizer : Option[Double]) extends Serializable {

  private val serialVersionUID = 42L

  def this() = this(None)

  val inds = new HashMap[A,Int]()
  var model : MaxEnt = null
  val pipeList = new java.util.ArrayList[Pipe]()
  pipeList.add(new Target2Label())
  pipeList.add(new Csv2FeatureVector())
  val pipe = new SerialPipes(pipeList)

  def toInd(a : A) = {
    if(!inds.keySet.contains(a))
      inds += a -> inds.size
    inds(a)
  }

  def makeInst(data : HashMap[A,Double], label : String) = { 
    val fStr = data.iterator.toArray.map(a => toInd(a._1) + ":" + a._2).mkString(" ") + " defallt:1.0"
    new Instance(fStr,label,"NAME!","SOURCE")
  }

  def makeInsts(data : List[(String,HashMap[A,Double])]) = {
    val insts = new InstanceList(pipe)
    data.foreach({
      case (lbl,d) => {
        insts.addThruPipe(makeInst(d,lbl))
      }
    })
    insts
  }

  def getTrainer() = {
    regularizer match {
      case Some(d) => new MaxEntTrainer(d)
      case None => new MaxEntTrainer()
    } 
  }

  def train(data : List[(String,HashMap[A,Double])]) = {
    val insts = makeInsts(data)
    val trainer = getTrainer()
    try {
      trainer.train(insts)
    } catch {
      case e : Exception => {
        println(e)
      }
    }
    model = trainer.getClassifier()
  }

  def predict(fs : HashMap[A,Double]) = {
    val ts = pipe.instanceFrom(makeInst(fs,"unknown"))
    val lblin = model.classify(ts).getLabeling()
    val lbls : Array[Object] = lblin.getLabelAlphabet().toArray
    lbls.map({
      case ll : String => {
        val k = lblin.getLabelAlphabet().lookupLabel(ll)
        val sc : Double = lblin.value(k)
        (ll,sc)
      }
    })
  }
  
  def eval(data : List[(String,HashMap[A,Double])]) : Double = eval(data,true)
  def eval(data : List[(String,HashMap[A,Double])], verbose : Boolean) : Double = {
    
    val ts_insts = makeInsts(data)
    val trial = new Trial(model, ts_insts)
    val acc = trial.getAccuracy()
    
    println("Accuracy: " + acc)
    println(new ConfusionMatrix(trial))

    acc
  }

  
}

object MalletClass {

  def load[A](file : String) : MalletClass[A] = {
    import java.io._
    var fis = new FileInputStream(file)
    var ois = new ObjectInputStream(fis)
    val ret = ois.readObject().asInstanceOf[MalletClass[A]]
    ois.close()
    ret
  }

  def save[A](file : String, model : MalletClass[A]) = {
    import java.io._
    var fos = new FileOutputStream(file)
    var oos = new ObjectOutputStream(fos)
    oos.writeObject(model)
    oos.close()
  }

  def main(args : Array[String]) : Unit = {

    val fisher = io.Source.fromFile("/home/chonger/Fisher.txt").getLines.toList.drop(1).map(l => {
      val parts = l.trim().split("\\s")
      val fMap = new HashMap[String,Double]()
      fMap += "PW" -> parts(1).toDouble
      fMap += "PL" -> parts(2).toDouble
      fMap += "SW" -> parts(3).toDouble
      fMap += "SL" -> parts(4).toDouble
      (parts(0),fMap)
    })

    val nData = fisher.length
    val nTrain = (nData * .8).toInt
    val train = fisher.slice(0,nTrain)
    val test = fisher.drop(nTrain)
    val saveFile = "/home/chonger/FisherSave.xxx"


    val model = new MalletClass[String]()

    model.train(train)
    model.eval(test)

    
    save(saveFile,model)

    
    val model2 = load[String](saveFile)
    model2.eval(test)

    test.foreach(x => {

      println(x._1 + " --- " + model2.predict(x._2).mkString(" "))

    })
    
  }
  
}

